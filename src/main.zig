const std = @import("std");
const Io = std.Io;
const Dir = Io.Dir;
const Terminal = Io.Terminal;
const Command = @import("Command");

const prompt = "$ ";

const usage =
    \\Usage: shell [OPTION] [FILE]...
    \\
    \\  --help    show this help message
    \\
    \\With no FILE, run as an interactive shell.
    \\Otherwise, execute commands from each FILE in order.
;

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const arena = init.arena;
    const args = try init.minimal.args.toSlice(arena.allocator());

    const stdout = Io.File.stdout();
    var stdout_writer = stdout.writer(io, &.{});
    const terminal: Terminal = .{
        .writer = &stdout_writer.interface,
        .mode = try .detect(io, stdout, false, false),
    };

    if (args.len > 1) {
        if (std.mem.eql(u8, args[1], "--help")) {
            try terminal.writer.print("{s}\n", .{usage});
        } else {
            for (args[1..]) |file| {
                try script(io, arena, file);
            }
        }
        return;
    }

    try repl(io, arena, terminal);
}

fn repl(io: Io, arena: *std.heap.ArenaAllocator, terminal: Terminal) !void {
    const allocator = arena.allocator();
    var cmd_buffer: [1024:0]u8 = undefined;

    var ok: bool = true;
    var stdin = Io.File.stdin().reader(io, &cmd_buffer);
    while (true) {
        _ = arena.reset(.retain_capacity);
        try terminal.setColor(if (ok) .yellow else .red);
        try terminal.writer.print("{s}", .{prompt});
        try terminal.setColor(.reset);

        const bytes = try stdin.interface.takeDelimiter('\n') orelse {
            try terminal.writer.print("sh: unexpected EOF\n", .{});
            std.process.exit(0);
        };
        const offset = @intFromPtr(bytes.ptr) - @intFromPtr(&cmd_buffer);
        cmd_buffer[offset + bytes.len] = 0;
        const cmd: [:0]const u8 = cmd_buffer[offset .. offset + bytes.len :0];

        ok = true;
        var err_token: ?[]const u8 = null;
        runcmd(io, allocator, cmd, &err_token) catch {
            if (err_token) |tok| {
                const position = @intFromPtr(tok.ptr) - @intFromPtr(cmd.ptr);
                std.debug.print("{s:[2]}{s:~<[3]}\n", .{ "", "^", position + prompt.len, tok.len });
                std.log.err("sh: parse error near `{s}'\n", .{tok});
            }
            ok = false;
        };
    }
}

fn script(io: Io, arena: *std.heap.ArenaAllocator, path: []const u8) !void {
    const allocator = arena.allocator();
    var cmd_buffer: [1024:0]u8 = undefined;

    const file = try Io.Dir.cwd().openFile(io, path, .{});
    defer file.close(io);

    var file_reader = file.reader(io, &cmd_buffer);
    var line_no: usize = 1;
    while (try file_reader.interface.takeDelimiter('\n')) |line| {
        const offset = @intFromPtr(line.ptr) - @intFromPtr(&cmd_buffer);
        cmd_buffer[offset + line.len] = 0;
        const cmd: [:0]const u8 = cmd_buffer[offset .. offset + line.len :0];
        _ = arena.reset(.retain_capacity);
        var err_token: ?[]const u8 = null;
        runcmd(io, allocator, cmd, &err_token) catch {
            if (err_token) |tok| {
                std.log.err("sh: {s}:{d}: parse error near `{s}'\n", .{ path, line_no, tok });
            }
        };
        line_no += 1;
    }
}

fn runcmd(io: Io, allocator: std.mem.Allocator, string: [:0]const u8, err_token: *?[]const u8) !void {
    var parse_error_token: []const u8 = undefined;
    var cmd = Command.init(io, allocator, string, &parse_error_token) catch |err| switch (err) {
        error.EmptyCmd => return,
        error.TokenizeError, error.ParseError => {
            err_token.* = parse_error_token;
            return error.ParseError;
        },
        else => return err,
    };
    defer cmd.deinit();
    try cmd.run();
}
