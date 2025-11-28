const Command = @This();
const std = @import("std");
const Ast = @import("Ast");
const Allocator = std.mem.Allocator;
const String = []const u8;
const c = std.c;
const Io = std.Io;

const MAXARG = 8;
const PIPESIZE = 64;

const FdList = std.ArrayList(c.fd_t);
const RedirList = struct { pipe: [2]c.fd_t, fds: FdList = .empty };

const CmdError = error{ OutOfMemory, PipeFailed, CmdFailed, BuiltinFailed };
const Builtin = enum { cd, exit, echo, pwd, command };

io: Io,
tree: Ast,
allocator: Allocator,

pub fn init(io: Io, allocator: Allocator, cmd: [:0]const u8, error_token: *String) !Command {
    const tree = Ast.parse(allocator, cmd, error_token) catch |err| switch (err) {
        error.EmptyCmd => return error.EmptyCmd,
        else => return err,
    };
    return .{ .io = io, .tree = tree, .allocator = allocator };
}

pub fn deinit(self: *Command) void {
    self.tree.deinit(self.allocator);
}

pub fn run(self: *Command) CmdError!void {
    std.log.debug("{f}\n", .{self.tree});
    try self.runNode(.root, true);
}

fn runNode(self: *Command, index: Ast.Node.Index, root: bool) CmdError!void {
    const tag = self.tree.nodeTag(index);
    const data = self.tree.nodeData(index);
    const builtin_cmd: ?Builtin = bc: {
        if (tag == .exec) {
            const tokens = self.tree.tokens.items(.lexeme)[data.token_range.start..data.token_range.end];
            if (std.meta.stringToEnum(Builtin, tokens[0])) |bc| break :bc bc;
        }
        break :bc null;
    };

    const st: enum { root, child } = if (!root) .child else if (builtin_cmd != null) .root else switch (tag) {
        .list, .@"and", .@"or" => .root,
        else => if (c.fork() == 0) .child else {
            const s = wait();
            if (s != 0) return error.CmdFailed;
            return;
        },
    };

    switch (tag) {
        .list => {
            const cmds = self.tree.extraDataSlice(data.extra_range, Ast.Node.Index);
            if (cmds.len == 0) {
                // nothing
            } else {
                for (cmds[0 .. cmds.len - 1]) |cmd| {
                    self.runNode(cmd, st == .root) catch {};
                }
                try self.runNode(cmds[cmds.len - 1], st == .root);
            }
        },
        .@"and" => {
            self.runNode(data.node_and_node[0], st == .root) catch return error.CmdFailed;
            try self.runNode(data.node_and_node[1], st == .root);
        },
        .@"or" => {
            self.runNode(data.node_and_node[0], st == .root) catch {
                try self.runNode(data.node_and_node[1], st == .root);
                return;
            };
        },
        .exec => {
            const tokens = self.tree.tokens.items(.lexeme)[data.token_range.start..data.token_range.end];
            if (builtin_cmd) |cmd| {
                try self.runBuiltin(cmd, tokens[1..]);
            } else {
                execute(self.allocator, tokens);
            }
        },
        .back => if (c.fork() == 0) {
            self.runNode(data.node, true) catch {};
            c.exit(0);
        },
        .pipe => try self.pipe(data.node_and_node),
        .redir, .piped_redir => try self.redirect(tag, data.node_and_extra),
        else => unreachable,
    }

    if (st == .child) c.exit(0);
}

/// builtins must be called by the parent, not the child.
fn runBuiltin(self: *Command, tag: Builtin, tokens: []const String) CmdError!void {
    switch (tag) {
        .cd => {
            if (tokens.len == 0) {
                std.log.err("cd: missing argument\n", .{});
                return error.BuiltinFailed;
            }
            const path = try self.allocator.dupeSentinel(u8, tokens[0], 0);
            defer self.allocator.free(path);
            if (c.chdir(path) != 0) {
                std.log.err("cd: {s}: No such file or directory\n", .{tokens[0]});
                return error.BuiltinFailed;
            }
        },
        .exit => {
            const n: u8 = if (tokens.len == 0) 0 else std.fmt.parseInt(u8, tokens[0], 0) catch 1;
            c.exit(n);
        },
        .echo => {
            for (tokens, 0..) |token, i| {
                if (i > 0) self.print(" ", .{});
                self.print("{s}", .{token});
            }
            self.print("\n", .{});
        },
        .pwd => {
            var buffer: [std.fs.max_path_bytes]u8 = undefined;
            if (c.getcwd(&buffer, buffer.len)) |ptr| {
                const len = std.mem.findScalar(u8, buffer[0..], 0) orelse buffer.len;
                self.print("{s}\n", .{ptr[0..len]});
            }
        },
        .command => {
            if (c.fork() == 0) execute(self.allocator, tokens);
            const s = wait();
            if (s != 0) return error.CmdFailed;
        },
    }
}

fn pipe(self: *Command, nodes: [2]Ast.Node.Index) CmdError!noreturn {
    var p: [2]c.fd_t = undefined;
    if (c.pipe(&p) != 0) return error.PipeFailed;

    var child_pids: [2]c_int = undefined;
    for (nodes, [2]u8{ 1, 0 }, 0..) |cmd, fd, i| {
        const pid = c.fork();
        if (pid == 0) {
            _ = c.close(p[1 - fd]);
            _ = c.dup2(p[fd], fd);
            _ = c.close(p[0]);
            _ = c.close(p[1]);
            _ = self.runNode(cmd, false) catch @as(u32, 1);
            c.exit(0);
        }
        child_pids[i] = pid;
    }

    _ = c.close(p[0]);
    _ = c.close(p[1]);

    // Wait for all children; use last command's exit status
    var status: u32 = 0;
    for (child_pids, 0..) |pid, i| {
        var raw: c_int = 0;
        _ = c.waitpid(pid, &raw, 0);
        if (i == nodes.len - 1) {
            status = @intCast((raw >> 8) & 0xff);
        }
    }
    c.exit(@intCast(status));
}

//    +------------+    +-------------+    +-------------+
//    | stdin file |    | stderr file |    | stdout file |
//    +------------+    +-------------+    +-------------+
//        |                   |                     |
//   read |                   | write               | write
//        |                   ⇑                     |
//        +--⇒ ----+  fork +----+    fork  +---- ⇒--+
//            | sh | ⇐---- | sh | -------⇒ | sh |
//  write +--⇐ ----+       +----+ ----+    +---- ⇐--+ read
//        |              read ⇑       |             |
//        ‖                   ‖   fork|             ‖
//   pipe ‖                   ‖   exec|             ‖ pipe
//        ‖             write ⇑       |             ‖
//        |                +-----+ ⇐--+             |
//   read +---------------⇒| cat |⇒-----------------+ write
//                         +-----+
fn redirect(self: *Command, tag: Ast.Node.Tag, node_and_extra: struct { Ast.Node.Index, Ast.ExtraIndex }) CmdError!noreturn {
    const redir = self.tree.extraData(node_and_extra[1], Ast.Node.Redirection);

    var redir_list = rl: {
        var list: [3]?RedirList = @splat(null);

        for (0..3, [3]bool{
            redir.stdin != .none,
            redir.stdout != .none or redir.stdout_append != .none or tag == .piped_redir,
            redir.stderr != .none or redir.stderr_append != .none,
        }) |i, exist| {
            if (exist) {
                var p: [2]c.fd_t = undefined;
                if (c.pipe(&p) != 0) return error.PipeFailed;
                list[i] = .{ .pipe = p };
            }
        }

        if (tag == .piped_redir) try list[1].?.fds.append(self.allocator, 1);
        break :rl list;
    };

    if (c.fork() == 0) {
        for (redir_list, 0.., [3]usize{ 0, 1, 1 }) |list, stdio, i| {
            if (list) |ls| {
                _ = c.close(@intCast(stdio));
                _ = c.dup2(ls.pipe[i], @intCast(stdio));
                _ = c.close(ls.pipe[0]);
                _ = c.close(ls.pipe[1]);
            }
        }
        _ = self.runNode(node_and_extra[0], false) catch @as(u32, 1);
        c.exit(0);
    }

    inline for (.{ .stdin, .stdout }) |stdio| {
        if (c.fork() == 0) {
            self.redirectIo(redir, &redir_list, stdio);
            c.exit(0);
        }
    }

    self.redirectIo(redir, &redir_list, .stderr);

    var status: u8 = 0;
    for (0..3) |_| {
        status += @truncate(wait());
    }
    c.exit(status);
}

fn redirectIo(
    self: *Command,
    redir: Ast.Node.Redirection,
    list: *[3]?RedirList,
    comptime stdio: enum(u8) { stdin, stdout, stderr },
) void {
    const idx: usize = @intFromEnum(stdio);

    for (list, 0..) |redir_list, i| {
        if (idx == i) continue;
        if (redir_list) |ls| {
            _ = c.close(ls.pipe[0]);
            _ = c.close(ls.pipe[1]);
        }
    }

    // Fields iterate in declaration order: stdout before stdout_append,
    // stderr before stderr_append, so TRUNC opens before APPEND within
    // a single IO process.
    inline for (
        std.meta.fields(Ast.Node.Redirection),
        [_]c.O{
            .{ .ACCMODE = .RDONLY },
            .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true },
            .{ .ACCMODE = .WRONLY, .CREAT = true, .APPEND = true },
            .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true },
            .{ .ACCMODE = .WRONLY, .CREAT = true, .APPEND = true },
        },
        .{ 0, 1, 1, 2, 2 },
    ) |field, flags, i| {
        if (idx != i) continue;

        if (@field(redir, field.name).unwrap()) |node| {
            const files = self.tree.extraDataSlice(self.tree.nodeData(node).extra_range, Ast.TokenIndex);
            const tokens = self.tree.tokens.items(.lexeme);

            for (files) |file| {
                const path = self.allocator.dupeSentinel(u8, tokens[file], 0) catch continue;
                defer self.allocator.free(path);

                const fd = c.open(path, flags, @as(c_uint, 0o666));
                if (fd >= 0) {
                    list[i].?.fds.append(self.allocator, fd) catch {};
                }
            }
        }
    }

    var pipe_buffer: [PIPESIZE]u8 = undefined;
    if (stdio == .stdin) {
        if (list[0]) |ls| {
            _ = c.close(ls.pipe[0]);
            defer _ = c.close(ls.pipe[1]);

            for (ls.fds.items) |fd| {
                while (true) {
                    const nbytes = c.read(fd, &pipe_buffer, PIPESIZE);
                    if (nbytes <= 0) break;
                    _ = c.write(ls.pipe[1], &pipe_buffer, @intCast(nbytes));
                }
            }
        }
    } else for (list[1..], 1..) |redir_list, i| {
        if (idx != i) continue;

        if (redir_list) |ls| {
            _ = c.close(ls.pipe[1]);
            defer _ = c.close(ls.pipe[0]);

            // Read from pipe and write to all files each chunk,
            // avoiding the race of concurrent reads on the same pipe.
            while (true) {
                const nbytes = c.read(ls.pipe[0], &pipe_buffer, PIPESIZE);
                if (nbytes <= 0) break;
                const len: usize = @intCast(nbytes);
                for (ls.fds.items) |fd| {
                    _ = c.write(fd, &pipe_buffer, len);
                }
            }
        }
    }
}

fn execute(allocator: Allocator, tokens: []const String) noreturn {
    execTokens(allocator, tokens) catch |err| switch (err) {
        error.TooBig => std.log.err("sh: too many args\n", .{}),
        error.FileNotFound => std.log.err("{s}: command not found\n", .{tokens[0]}),
        else => std.log.err("exec {s} failed\n", .{tokens[0]}),
    };
    c.exit(1);
}

fn execTokens(allocator: Allocator, tokens: []const String) !void {
    if (tokens.len > MAXARG) return error.TooBig;
    var list = try allocator.allocSentinel(?[*:0]const u8, tokens.len, null);
    defer allocator.free(list);

    for (tokens, 0..) |token, i| {
        list[i] = try allocator.dupeSentinel(u8, token, 0);
    }

    const argv = list[0 .. list.len + 1];
    const argv_ptr: [*:null]const ?[*:0]const u8 = @ptrCast(argv);
    const envp: [*:null]const ?[*:0]const u8 = @ptrCast(c.environ);

    if (std.mem.findScalar(u8, tokens[0], '/') != null) {
        _ = c.execve(argv[0].?, argv_ptr, envp);
        return error.FileNotFound;
    }

    const path_env = getPath() orelse return error.FileNotFound;
    var path_buf: [std.fs.max_path_bytes + 1]u8 = undefined;

    var start: usize = 0;
    while (start < path_env.len) {
        const end = std.mem.findScalarPos(u8, path_env, start, ':') orelse path_env.len;
        const dir = path_env[start..end];
        start = end + 1;

        if (dir.len == 0) continue;

        const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ dir, tokens[0] }) catch continue;
        path_buf[full_path.len] = 0;
        const path_z: [*:0]const u8 = path_buf[0..full_path.len :0];

        _ = c.execve(path_z, argv_ptr, envp);
    }

    return error.FileNotFound;
}

fn print(self: *Command, comptime fmt: []const u8, args: anytype) void {
    var buffer: [64]u8 = undefined;
    var stdout: Io.File.Writer = .init(.stdout(), self.io, &buffer);
    stdout.interface.print(fmt, args) catch {};
    stdout.interface.flush() catch {};
}

fn getPath() ?[]const u8 {
    var i: usize = 0;
    while (c.environ[i]) |entry| : (i += 1) {
        const s = std.mem.sliceTo(entry, 0);
        if (std.mem.startsWith(u8, s, "PATH=")) {
            return s[5..];
        }
    }
    return null;
}

fn wait() u32 {
    var raw: c_int = 0;
    _ = c.waitpid(-1, &raw, 0);
    return @intCast((raw >> 8) & 0xff);
}
