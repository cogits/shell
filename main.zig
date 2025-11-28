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

const FdList = std.ArrayList(posix.fd_t);
const RedirList = struct { pipe: [2]posix.fd_t, fds: FdList = .empty };

const CmdError = error{OutOfMemory} ||
    posix.ForkError || posix.PipeError || posix.GetCwdError ||
    posix.OpenError || posix.ReadError || posix.WriteError;

const Builtin = enum { cd, exit, echo, pwd, command };

var arena_allocator: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
const allocator = arena_allocator.allocator();

const prompt = "$ ";
var cmd_buffer: [1024:0]u8 = undefined;
var stdin = std.fs.File.stdin().reader(&cmd_buffer);

const usage =
    \\Usage: shell [OPTION]... [FILE]...
    \\
    \\Options:
    \\  --help         Display this help message and exit
    \\
    \\Execute commands from:
    \\  (no args)      Start interactive REPL shell
    \\  FILE           Run commands from each specified file
;

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len > 1) {
        if (std.mem.eql(u8, args[1], "--help")) {
            print("{s}\n", .{usage});
        } else {
            for (args[1..]) |file| {
                try script(file);
            }
        }
        return;
    }

    try repl();
}

fn repl() !void {
    var status: u32 = 0;
    while (true) {
        _ = arena_allocator.reset(.retain_capacity); // This reclaims all allocations!
        const prompt_color = if (status == 0) color.yellow else color.red;
        print("{s}{s}" ++ color.reset, .{ prompt_color, prompt });

        const bytes = try stdin.interface.takeDelimiter('\n') orelse {
            print("\n", .{});
            posix.exit(0);
        };
        cmd_buffer[bytes.len] = 0;
        const cmd: [:0]const u8 = cmd_buffer[0..bytes.len :0];
        status = try runcmd(cmd, true);
    }
}

fn script(path: String) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var file_reader = file.reader(&cmd_buffer);
    while (try file_reader.interface.takeDelimiter('\n')) |line| {
        _ = arena_allocator.reset(.retain_capacity); // This reclaims all allocations!
        cmd_buffer[line.len] = 0;
        const cmd: [:0]const u8 = cmd_buffer[0..line.len :0];
        if (try runcmd(cmd, false) != 0) return error.CmdFailed;
    }
}

fn runcmd(cmd: [:0]const u8, debug: bool) !u32 {
    // parse command
    var error_token: String = undefined;
    var tree = Ast.parse(Builtin, allocator, cmd, &error_token) catch |err| switch (err) {
        error.EmptyCmd => return 0,
        error.TokenizeError, error.ParseError => {
            if (debug) {
                const position = @intFromPtr(error_token.ptr) - @intFromPtr(cmd.ptr);
                print("{s:[2]}{s:~<[3]}\n", .{ "", "^", position + prompt.len, error_token.len });
                print("sh: parse error near `{s}'\n", .{error_token});
            }
            return 1;
        },
        else => return err,
    };
    defer tree.deinit(allocator);

    // run command
    if (debug) std.log.debug("{f}", .{tree});
    return runnode(tree, .root, true);
}

fn runnode(tree: Ast, index: Ast.Node.Index, root: bool) CmdError!u32 {
    // Process creation strategy:
    // - Root-level commands may need child processes (except builtins/lists)
    // - Nested commands always execute in current process
    const tag = tree.nodeTag(index);
    const state: enum { root, parent, child } = if (!root) .child else switch (tag) {
        .list, .builtin, .@"and", .@"or" => .root,
        else => if (try posix.fork() == 0) .child else .parent,
    };

    // Parent process cleanup: wait for any forked children
    if (state == .parent) {
        const ret = posix.waitpid(0, 0);
        return ret.status;
    }

    // Command execution logic (runs in either root shell or child process)
    var status: u32 = 0;
    const data = tree.nodeData(index);
    switch (tag) {
        .list => {
            const cmds = tree.extraDataSlice(data.extra_range, Ast.Node.Index);
            for (cmds) |cmd| {
                status = try runnode(tree, cmd, state == .root);
            }
        },
        .@"and", .@"or" => {
            status = try runnode(tree, data.node_and_node[0], state == .root);
            if (tag == .@"and" and status != 0) return status;
            if (tag == .@"or" and status == 0) return status;
            status = try runnode(tree, data.node_and_node[1], state == .root);
        },
        .builtin, .exec => {
            const tokens = tree.tokens.items(.lexeme)[data.token_range.start..data.token_range.end];
            if (tag == .builtin) {
                status = try builtin(std.meta.stringToEnum(Builtin, tokens[0]).?, tokens[1..]);
            } else {
                execute(tokens);
            }
        },
        .back => if (try posix.fork() == 0) {
            _ = try runnode(tree, data.node, true);
        },
        .pipe => try pipe(tree, data.node_and_node),
        .redir, .piped_redir => try redirect(tree, tag, data.node_and_extra),
        else => unreachable,
    }

    if (state == .child) posix.exit(0);
    return status;
}

/// builtins must be called by the parent, not the child.
fn builtin(tag: Builtin, tokens: []const String) !u32 {
    var status: u32 = 0;
    switch (tag) {
        .cd => {
            const path = try allocator.dupeZ(u8, tokens[0]);
            defer allocator.free(path);
            posix.chdir(path) catch |err| {
                print("cannot cd {s}: {s}\n", .{ path, @errorName(err) });
                status = 1;
            };
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
            status = posix.waitpid(0, 0).status;
        },
    }
    return status;
}

fn execute(tokens: []const String) noreturn {
    execvpe(tokens) catch |err| switch (err) {
        error.TooBig => print("sh: too many args\n", .{}),
        error.FileNotFound => print("{s}: command not found\n", .{tokens[0]}),
        else => |e| print("exec {s} failed: {s}\n", .{ tokens[0], @errorName(e) }),
    };
    posix.exit(1);
}

fn execvpe(tokens: []const String) !void {
    if (tokens.len > MAXARG) return error.TooBig;
    var list = try allocator.allocSentinel(?[*:0]const u8, tokens.len, null);
    defer allocator.free(list);

    for (tokens, 0..) |token, i| {
        list[i] = try allocator.dupeZ(u8, token);
    }

    const argv = list[0 .. list.len + 1];
    return posix.execvpeZ(argv[0].?, @ptrCast(argv), @ptrCast(std.os.environ.ptr));
}

fn pipe(tree: Ast, nodes: [2]Ast.Node.Index) !noreturn {
    const p = try posix.pipe();

    for (nodes, [2]u8{ 1, 0 }) |cmd, fd| {
        if (try posix.fork() == 0) {
            posix.close(fd);

            _ = try posix.dup(p[fd]);
            posix.close(p[0]);
            posix.close(p[1]);
            _ = try runnode(tree, cmd, false);
        }
    }

    posix.close(p[0]);
    posix.close(p[1]);
    _ = posix.waitpid(0, 0);
    const ret = posix.waitpid(0, 0);
    posix.exit(if (ret.status != 0) 1 else 0);
}

fn redirect(tree: Ast, tag: Ast.Node.Tag, node_and_extra: struct { Ast.Node.Index, Ast.ExtraIndex }) !noreturn {
    const redir = tree.extraData(node_and_extra[1], Ast.Node.Redirection);

    var redir_list = rl: {
        var list: [3]?RedirList = @splat(null);

        for (0..3, [3]bool{
            redir.stdin != .none,
            redir.stdout != .none or redir.stdout_append != .none or tag == .piped_redir,
            redir.stderr != .none or redir.stderr_append != .none,
        }) |i, exist| {
            if (exist) list[i] = .{ .pipe = try posix.pipe() };
        }

        if (tag == .piped_redir) try list[1].?.fds.append(allocator, 1);
        break :rl list;
    };

    // fork a new process to run the command
    if (try posix.fork() == 0) {
        for (redir_list, 0.., [3]usize{ 0, 1, 1 }) |list, stdio, i| {
            if (list) |ls| {
                posix.close(@intCast(stdio));
                _ = try posix.dup(ls.pipe[i]);
                posix.close(ls.pipe[0]);
                posix.close(ls.pipe[1]);
            }
        }
        _ = try runnode(tree, node_and_extra[0], false);
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
                try redir_list[i].?.fds.append(allocator, try posix.open(path, flags, std.fs.File.default_mode));
            }
        }
    }

    // pipe data between child and input/output files
    {
        const buf = try allocator.alloc(u8, PIPESIZE);
        defer allocator.free(buf);

        // read from input files and write to child stdin pipe
        if (redir_list[0]) |ls| {
            posix.close(ls.pipe[0]);
            defer posix.close(ls.pipe[1]);

            for (ls.fds.items) |fd| {
                while (true) {
                    const nbytes = try posix.read(fd, buf);
                    if (nbytes == 0) break;
                    _ = try posix.write(ls.pipe[1], buf[0..nbytes]);
                }
            }
        }

        // read from child stdout/stderr pipes and write to output files
        for (redir_list[1..]) |list| {
            if (list) |ls| {
                posix.close(ls.pipe[1]);
                defer posix.close(ls.pipe[0]);

                const fds = ls.fds.items;
                while (true) {
                    const nbytes = try posix.read(ls.pipe[0], buf);
                    if (nbytes == 0) break;

                    for (fds) |fd| {
                        _ = try posix.write(fd, buf[0..nbytes]);
                    }
                }
            }
        }
    }

    const ret = posix.waitpid(0, 0);
    posix.exit(if (ret.status != 0) 1 else 0);
}
