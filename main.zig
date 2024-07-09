const std = @import("std");
const Ast = @import("Ast");
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

var arena_allocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const allocator = arena_allocator.allocator();

pub fn main() !void {
    while (true) {
        try run();
    }
}

fn run() !void {
    _ = arena_allocator.reset(.retain_capacity);

    print(color.yellow ++ "$ " ++ color.reset, .{});
    const cmd = try getcmd();
    var tree = try Ast.parse(allocator, cmd) orelse return;
    defer tree.deinit(allocator);

    if (tree.error_token != null) {
        const error_token = tree.error_token.?;
        const position = @intFromPtr(error_token.ptr) - @intFromPtr(tree.source.ptr);
        print("{s:[2]}{s:~<[3]}\n", .{ "", "^", position + 2, error_token.len });
        print("sh: parse error near `{s}'\n", .{error_token});
        return;
    }

    runcmd(tree, 0) catch |err| switch (err) {
        error.Overflow => print("sh: too many args\n", .{}),
        else => return err,
    };
}

fn getcmd() ![:0]u8 {
    var buf = try allocator.alloc(u8, 256);
    const stdin = std.io.getStdIn().reader();
    const cmd = try stdin.readUntilDelimiterOrEof(buf, '\n') orelse posix.exit(0);
    buf[cmd.len] = 0; // chop \n
    return buf[0..cmd.len :0];
}

fn runcmd(ast: Ast, index: Ast.Node.Index) !void {
    const node = ast.nodes.get(index);
    switch (node.tag) {
        .list => {
            const cmds = ast.extra_data[node.data.lhs..node.data.rhs];
            for (cmds) |cmd| {
                switch (ast.nodes.get(cmd).tag) {
                    .builtin, .list => try runcmd(ast, cmd),
                    .exec, .pipe, .redir, .redir_pipe, .back => {
                        if (try posix.fork() == 0) try runcmd(ast, cmd);
                        _ = posix.waitpid(0, 0);
                    },
                    else => unreachable,
                }
            }
        },
        .pipe => {
            const p = try posix.pipe();

            for ([2]Ast.Node.Index{ node.data.lhs, node.data.rhs }, [2]u8{ 1, 0 }) |cmd, fd| {
                if (try posix.fork() == 0) {
                    posix.close(fd);

                    _ = try posix.dup(p[fd]);
                    posix.close(p[0]);
                    posix.close(p[1]);
                    try runcmd(ast, cmd);
                }
            }

            posix.close(p[0]);
            posix.close(p[1]);
            _ = posix.waitpid(0, 0);
            _ = posix.waitpid(0, 0);
            posix.exit(0);
        },
        .redir, .redir_pipe => {
            const extra = ast.extraData(node.data.rhs, Ast.Node.Redirection);
            const FdList = std.ArrayList(posix.fd_t);

            const stdio_pipe, var fd_list = blk: {
                var pipes: [3]?[2]c_int = undefined;
                var fds: [3]?FdList = undefined;

                for (0..3, [3]bool{
                    extra.stdin != 0,
                    extra.stdout != 0 or extra.stdout_append != 0 or node.tag == .redir_pipe,
                    extra.stderr != 0 or extra.stderr_append != 0,
                }) |i, exist| {
                    if (exist) {
                        pipes[i] = try posix.pipe();
                        fds[i] = FdList.init(allocator);
                    } else {
                        pipes[i] = null;
                        fds[i] = null;
                    }
                }

                if (node.tag == .redir_pipe) try fds[1].?.append(1);
                break :blk .{ pipes, fds };
            };

            // fork a new process to run the command
            if (try posix.fork() == 0) {
                for (stdio_pipe, 0.., [3]usize{ 0, 1, 1 }) |pipe, stdio, pipe_i| {
                    if (pipe) |p| {
                        posix.close(@intCast(stdio));
                        _ = try posix.dup(p[pipe_i]);
                        posix.close(p[0]);
                        posix.close(p[1]);
                    }
                }
                try runcmd(ast, node.data.lhs);
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
                const field_index = @field(extra, field.name);
                if (field_index != 0) {
                    const field_node = ast.nodes.get(field_index);
                    const files = ast.extra_data[field_node.data.lhs..field_node.data.rhs];
                    const tokens = ast.tokens.items(.lexeme);

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
                for (stdio_pipe[1..], fd_list[1..]) |pipe, list| {
                    if (pipe) |p| {
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
            posix.exit(0);
        },
        .exec => {
            var list = std.BoundedArray(?[*:0]const u8, MAXARG){};

            const tokens = ast.tokens.items(.lexeme)[node.data.lhs..node.data.rhs];
            for (tokens) |token| {
                try list.append(try allocator.dupeZ(u8, token));
            }
            try list.append(null);

            const argv = list.slice();
            const err = posix.execvpeZ(argv[0].?, @ptrCast(argv), @ptrCast(std.os.environ.ptr));
            switch (err) {
                error.FileNotFound => print("{s}: command not found\n", .{argv[0].?}),
                else => |e| print("sh: exec {s} failed: {s}\n", .{ argv[0].?, @errorName(e) }),
            }

            posix.exit(0);
        },
        .builtin => {
            // chdir must be called by the parent, not the child.
            const tokens = ast.tokens.items(.lexeme)[node.data.lhs..node.data.rhs];
            const path = try allocator.dupeZ(u8, tokens[1]);
            defer allocator.free(path);
            posix.chdir(path) catch |err|
                print("cannot cd {s}: {s}\n", .{ path, @errorName(err) });
        },
        .back => {
            if (try posix.fork() == 0) try runcmd(ast, node.data.lhs);
            posix.exit(0); // Let parent exit before child.
        },
        else => unreachable,
    }
}
