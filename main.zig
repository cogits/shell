const std = @import("std");
const Ast = @import("Ast");
const String = []const u8;
const posix = std.posix;
const print = std.debug.print;

const MAXARG = 8;
const PIPESIZE = 64;

const color = struct {
    const red = "\x1b[31m";
    const green = "\x1b[32m";
    const yellow = "\x1b[33m";
    const reset = "\x1b[0m";
};

const CmdError = error{OutOfMemory} ||
    posix.ForkError || posix.PipeError || posix.GetCwdError ||
    posix.OpenError || posix.ReadError || posix.WriteError;

pub const Builtin = enum { cd, exit, echo, pwd, command };

var arena_allocator: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
const allocator = arena_allocator.allocator();
const stdin = std.io.getStdIn().reader();

pub fn main() !void {
    var buffer: [1024]u8 = undefined;
    while (true) {
        _ = arena_allocator.reset(.retain_capacity); // This reclaims all allocations!
        print(color.yellow ++ "$ " ++ color.reset, .{});

        // get command
        const bytes = try stdin.readUntilDelimiterOrEof(&buffer, '\n') orelse posix.exit(0);
        buffer[bytes.len] = 0;
        const cmd: [:0]const u8 = buffer[0..bytes.len :0];

        // parse command
        var error_token: String = undefined;
        var tree = Ast.parse(allocator, cmd, &error_token) catch |err| switch (err) {
            error.EmptyCmd => continue,
            error.TokenizeError, error.ParseError => {
                const position = @intFromPtr(error_token.ptr) - @intFromPtr(cmd.ptr);
                print("{s:[2]}{s:~<[3]}\n", .{ "", "^", position + 2, error_token.len });
                print("sh: parse error near `{s}'\n", .{error_token});
                continue;
            },
            else => return err,
        };
        defer tree.deinit(allocator);

        // run command
        std.log.debug("{}\n{r}", .{ tree, tree });
        try runcmd(tree, .root, true);
    }
}

fn runcmd(tree: Ast, index: Ast.Node.Index, root: bool) CmdError!void {
    const tag = tree.nodeTag(index);

    // Process creation strategy:
    // - Root-level commands may need child processes (except builtins/lists)
    // - Nested commands always execute in current process
    const state: enum { root, parent, child } = if (root)
        switch (tag) {
            .list, .builtin => .root,
            else => if (try posix.fork() == 0) .child else .parent,
        }
    else
        .child;

    // Parent process cleanup: wait for any forked children
    if (state == .parent) {
        _ = posix.waitpid(0, 0);
        return;
    }

    // Command execution logic (runs in either root shell or child process)
    const data = tree.nodeData(index);
    switch (tag) {
        .list => {
            const cmds = tree.extraDataSlice(data.extra_range, Ast.Node.Index);
            for (cmds) |cmd| {
                try runcmd(tree, cmd, state == .root);
            }
        },
        .back => {
            if (try posix.fork() == 0) try runcmd(tree, data.node, false);
        },
        .builtin, .exec => {
            const tokens = tree.tokens.items(.lexeme)[data.token_range.start..data.token_range.end];
            if (tag == .builtin) {
                try builtin(std.meta.stringToEnum(Builtin, tokens[0]).?, tokens[1..]);
            } else {
                execute(tokens);
            }
        },
        .pipe => {
            try pipe(tree, data.node_and_node);
        },
        .redir, .piped_redir => {
            try redirect(tree, tag, data.node_and_extra[0], data.node_and_extra[1]);
        },
        else => unreachable,
    }

    if (state == .child) posix.exit(0);
}

/// builtins must be called by the parent, not the child.
fn builtin(tag: Builtin, tokens: []const String) !void {
    switch (tag) {
        .cd => {
            const path = try allocator.dupeZ(u8, tokens[0]);
            defer allocator.free(path);
            posix.chdir(path) catch |err|
                print("cannot cd {s}: {s}\n", .{ path, @errorName(err) });
        },
        .exit => {
            const n = if (tokens.len == 0) 0 else std.fmt.parseInt(u8, tokens[0], 0) catch 1;
            posix.exit(n);
        },
        .echo => {
            for (tokens) |token| {
                print("{s} ", .{token});
            }
            print("\n", .{});
        },
        .pwd => {
            var buffer: [std.fs.max_path_bytes]u8 = undefined;
            print("{s}\n", .{try std.process.getCwd(&buffer)});
        },
        .command => {
            if (try posix.fork() == 0) execute(tokens);
            _ = posix.waitpid(0, 0);
        },
    }
}

fn execute(tokens: []const String) noreturn {
    execvpe(tokens) catch |err| switch (err) {
        error.Overflow => print("sh: too many args\n", .{}),
        error.FileNotFound => print("{s}: command not found\n", .{tokens[0]}),
        else => |e| print("exec {s} failed: {s}\n", .{ tokens[0], @errorName(e) }),
    };
    posix.exit(1);
}

fn execvpe(tokens: []const String) !void {
    var list: std.BoundedArray(?[*:0]const u8, MAXARG + 2) = .{};

    for (tokens) |token| {
        try list.append(try allocator.dupeZ(u8, token));
    }
    try list.append(null);

    const argv = list.slice();
    return posix.execvpeZ(argv[0].?, @ptrCast(argv), @ptrCast(std.os.environ.ptr));
}

fn pipe(tree: Ast, nodes: [2]Ast.Node.Index) !void {
    const p = try posix.pipe();

    for (nodes, [2]u8{ 1, 0 }) |cmd, fd| {
        if (try posix.fork() == 0) {
            posix.close(fd);

            _ = try posix.dup(p[fd]);
            posix.close(p[0]);
            posix.close(p[1]);
            try runcmd(tree, cmd, false);
        }
    }

    posix.close(p[0]);
    posix.close(p[1]);
    _ = posix.waitpid(0, 0);
    _ = posix.waitpid(0, 0);
}

fn redirect(tree: Ast, tag: Ast.Node.Tag, exec: Ast.Node.Index, extra: Ast.ExtraIndex) !void {
    const redir = tree.extraData(extra, Ast.Node.Redirection);
    const FdList = std.ArrayList(posix.fd_t);

    const stdio_pipe, var fd_list = blk: {
        var pipes: [3]?[2]c_int = undefined;
        var fds: [3]?FdList = undefined;

        for (0..3, [3]bool{
            redir.stdin != .none,
            redir.stdout != .none or redir.stdout_append != .none or tag == .piped_redir,
            redir.stderr != .none or redir.stderr_append != .none,
        }) |i, exist| {
            if (exist) {
                pipes[i] = try posix.pipe();
                fds[i] = .init(allocator);
            } else {
                pipes[i] = null;
                fds[i] = null;
            }
        }

        if (tag == .piped_redir) try fds[1].?.append(1);
        break :blk .{ pipes, fds };
    };

    // fork a new process to run the command
    if (try posix.fork() == 0) {
        for (stdio_pipe, 0.., [3]usize{ 0, 1, 1 }) |pi, stdio, pipe_i| {
            if (pi) |p| {
                posix.close(@intCast(stdio));
                _ = try posix.dup(p[pipe_i]);
                posix.close(p[0]);
                posix.close(p[1]);
            }
        }
        try runcmd(tree, exec, false);
    }

    // open input/output files
    inline for (
        std.meta.fields(Ast.Node.Redirection),
        [_]posix.O{
            .{ .ACCMODE = .RDONLY }, // <
            .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true }, // >
            .{ .ACCMODE = .WRONLY, .CREAT = true, .APPEND = true }, // >>
            .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true }, // 2>
            .{ .ACCMODE = .WRONLY, .CREAT = true, .APPEND = true }, // 2>>
        },
        .{ 0, 1, 1, 2, 2 },
    ) |field, flags, i| {
        const redir_node = @field(redir, field.name);
        if (redir_node.unwrap()) |node| {
            const files = tree.extraDataSlice(tree.nodeData(node).extra_range, Ast.TokenIndex);
            const tokens = tree.tokens.items(.lexeme);

            for (files) |file| {
                const path = try allocator.dupeZ(u8, tokens[file]);
                defer allocator.free(path);
                try fd_list[i].?.append(try posix.open(path, flags, std.fs.File.default_mode));
            }
        }
    }

    // pipe data between child and input/output files
    {
        const buf = try allocator.alloc(u8, PIPESIZE);
        defer allocator.free(buf);

        // read from input files and write to child stdin pipe
        if (stdio_pipe[0]) |p| {
            posix.close(p[0]);
            defer posix.close(p[1]);

            for (fd_list[0].?.items) |fd| {
                while (true) {
                    const nbytes = try posix.read(fd, buf);
                    if (nbytes == 0) break;
                    _ = try posix.write(p[1], buf[0..nbytes]);
                }
            }
        }

        // read from child stdout/stderr pipes and write to output files
        for (stdio_pipe[1..], fd_list[1..]) |pi, list| {
            if (pi) |p| {
                posix.close(p[1]);
                defer posix.close(p[0]);

                const fds = list.?.items;
                while (true) {
                    const nbytes = try posix.read(p[0], buf);
                    if (nbytes == 0) break;

                    for (fds) |fd| {
                        _ = try posix.write(fd, buf[0..nbytes]);
                    }
                }
            }
        }
    }

    _ = posix.waitpid(0, 0);
}
