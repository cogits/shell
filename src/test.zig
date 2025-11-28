const std = @import("std");
const testing = std.testing;
const Io = std.Io;
const Dir = Io.Dir;
const Command = @import("Command");

const io = testing.io;
const allocator = testing.allocator;

const TmpPath = struct {
    tmp: testing.TmpDir,
    file: []const u8,

    pub fn join(tmp: testing.TmpDir, file: []const u8) TmpPath {
        return .{ .tmp = tmp, .file = file };
    }

    pub fn format(self: TmpPath, w: *Io.Writer) Io.Writer.Error!void {
        var path_buf: [std.c.PATH_MAX:0]u8 = undefined;
        const dir_len = self.tmp.dir.realPath(io, &path_buf) catch return error.WriteFailed;
        try w.print("{s}/{s}", .{ path_buf[0..dir_len], self.file });
    }

    pub fn write(self: TmpPath, bytes: []const u8) !void {
        try self.tmp.dir.writeFile(io, .{ .sub_path = self.file, .data = bytes });
    }

    pub fn read(self: TmpPath, buf: []u8) ![]const u8 {
        return self.tmp.dir.readFile(io, self.file, buf);
    }
};

fn testcmd(comptime fmt: []const u8, args: anytype) !void {
    var cmd_buf: [512:0]u8 = undefined;
    const cmd_str = try std.fmt.bufPrintSentinel(&cmd_buf, fmt, args, 0);

    var error_token: []const u8 = undefined;
    var cmd = Command.init(io, allocator, cmd_str, &error_token) catch |err| switch (err) {
        error.EmptyCmd => return,
        else => return err,
    };
    defer cmd.deinit();
    try cmd.run();
}

// ─── Exit codes ──────────────────────────────────────────────────────────

test "true returns 0" {
    try testcmd("true", .{});
}

test "false returns 1" {
    try testing.expectError(error.CmdFailed, testcmd("false", .{}));
}

test "list: semicolon separates commands" {
    try testing.expectError(error.CmdFailed, testcmd("true; false", .{}));
}

test "list: semicolon preserves last status" {
    try testcmd("false; true", .{});
}

test "empty command returns 0" {
    try testcmd("", .{});
}

test "whitespace-only returns 0" {
    try testcmd("   ", .{});
}

// ─── Logical operators ───────────────────────────────────────────────────

test "and: true && true runs both" {
    try testcmd("true && true", .{});
}

test "and: true && false short-circuits" {
    try testing.expectError(error.CmdFailed, testcmd("true && false", .{}));
}

test "and: false && true skips right" {
    try testing.expectError(error.CmdFailed, testcmd("false && true", .{}));
}

test "or: false || true runs right" {
    try testcmd("false || true", .{});
}

test "or: true || false skips right" {
    try testcmd("true || false", .{});
}

test "and/or chain" {
    try testcmd("true && true || false", .{});
}

// ─── Pipes (output redirected to avoid corrupting test runner IPC) ───────

test "simple pipe" {
    try testcmd("echo pipe | cat > /dev/null", .{});
}

test "multi-pipe: echo | cat | cat" {
    try testcmd("echo foo | cat | cat > /dev/null", .{});
}

test "4-stage pipe" {
    try testcmd("echo foo | cat | cat | cat > /dev/null", .{});
}

test "pipe exit code from last command" {
    try testing.expectError(error.CmdFailed, testcmd("true | false", .{}));
}

// ─── Single redirect ────────────────────────────────────────────────────

test "redirect stdout to file" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const redir: TmpPath = .join(tmp, "redir.txt");
    try testcmd("echo hello > {f}", .{redir});

    var redir_buf: [4096]u8 = undefined;
    const content = try redir.read(&redir_buf);
    try testing.expectEqualStrings("hello\n", content);
}

test "redirect append to new file" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const append: TmpPath = .join(tmp, "append.txt");
    try testcmd("echo first >> {f}", .{append});

    var append_buf: [4096]u8 = undefined;
    const content = try append.read(&append_buf);
    try testing.expectEqualStrings("first\n", content);
}

test "redirect append preserves existing content" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const append2: TmpPath = .join(tmp, "append2.txt");
    try append2.write("existing\n");
    try testcmd("echo added >> {f}", .{append2});

    var append2_buf: [4096]u8 = undefined;
    const content = try append2.read(&append2_buf);
    try testing.expectEqualStrings("existing\nadded\n", content);
}

// ─── Multi-IO redirect (tee) ────────────────────────────────────────────

test "multi-io: redirect to two files" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const m1: TmpPath = .join(tmp, "m1.txt");
    const m2: TmpPath = .join(tmp, "m2.txt");
    try testcmd("echo foo > {f} > {f}", .{ m1, m2 });

    var m1_buf: [4096]u8 = undefined;
    const c1 = try m1.read(&m1_buf);
    var m2_buf: [4096]u8 = undefined;
    const c2 = try m2.read(&m2_buf);

    try testing.expectEqualStrings("foo\n", c1);
    try testing.expectEqualStrings("foo\n", c2);
}

test "multi-io: three files" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const f3a: TmpPath = .join(tmp, "3a.txt");
    const f3b: TmpPath = .join(tmp, "3b.txt");
    const f3c: TmpPath = .join(tmp, "3c.txt");
    try testcmd("echo tee > {f} > {f} > {f}", .{ f3a, f3b, f3c });

    inline for (.{ f3a, f3b, f3c }) |td| {
        var td_buf: [4096]u8 = undefined;
        const content = try td.read(&td_buf);
        try testing.expectEqualStrings("tee\n", content);
    }
}

// ─── Mixed > and >> (the reported bug) ──────────────────────────────────────────

test "truncate + append to same file" {
    if (true) return error.SkipZigTest;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const ta1: TmpPath = .join(tmp, "ta1.txt");
    const ta2: TmpPath = .join(tmp, "ta2.txt");
    try testcmd("echo foo > {f} > {f} >> {f}", .{ ta1, ta2, ta2 });

    var ta1_buf: [4096]u8 = undefined;
    const c1 = try ta1.read(&ta1_buf);
    try testing.expectEqualStrings("foo\n", c1);

    var ta2_buf: [4096]u8 = undefined;
    const c2 = try ta2.read(&ta2_buf);
    try testing.expectEqualStrings("foo\nfoo\n", c2);
}

test "> and >> to different files" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const diff_a: TmpPath = .join(tmp, "diff_a.txt");
    const diff_b: TmpPath = .join(tmp, "diff_b.txt");
    try testcmd("echo hello > {f} >> {f}", .{ diff_a, diff_b });

    var da_buf: [4096]u8 = undefined;
    const ca = try diff_a.read(&da_buf);
    try testing.expectEqualStrings("hello\n", ca);

    var db_buf: [4096]u8 = undefined;
    const cb = try diff_b.read(&db_buf);
    try testing.expectEqualStrings("hello\n", cb);
}

// ─── Piped redirect (piped_redir) ───────────────────────────────────────

test "piped_redir: write to file and pipe" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const piped: TmpPath = .join(tmp, "piped_redir.txt");
    try testcmd("echo piped > {f} | cat > /dev/null", .{piped});

    var piped_buf: [4096]u8 = undefined;
    const content = try piped.read(&piped_buf);
    try testing.expectEqualStrings("piped\n", content);
}

// ─── Parse errors ───────────────────────────────────────────────────────

test "parse error: semicolon alone" {
    try testing.expectError(error.ParseError, testcmd(";", .{}));
}

test "parse error: trailing pipe" {
    try testing.expectError(error.ParseError, testcmd("ls |", .{}));
}

test "parse error: unmatched paren" {
    try testing.expectError(error.ParseError, testcmd("(", .{}));
}

// ─── Builtin commands ───────────────────────────────────────────────────

test "echo: no arguments prints newline" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const f: TmpPath = .join(tmp, "echo_empty.txt");
    try testcmd("echo > {f}", .{f});

    var buf: [4096]u8 = undefined;
    const content = try f.read(&buf);
    try testing.expectEqualStrings("\n", content);
}

test "echo: single argument" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const f: TmpPath = .join(tmp, "echo_one.txt");
    try testcmd("echo hello > {f}", .{f});

    var buf: [4096]u8 = undefined;
    const content = try f.read(&buf);
    try testing.expectEqualStrings("hello\n", content);
}

test "echo: multiple arguments" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const f: TmpPath = .join(tmp, "echo_multi.txt");
    try testcmd("echo a b c > {f}", .{f});

    var buf: [4096]u8 = undefined;
    const content = try f.read(&buf);
    try testing.expectEqualStrings("a b c\n", content);
}

test "pwd: prints working directory" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const f: TmpPath = .join(tmp, "pwd.txt");
    try testcmd("pwd > {f}", .{f});

    var buf: [4096]u8 = undefined;
    const content = try f.read(&buf);

    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd_len = try std.process.currentPath(io, &cwd_buf);
    try testing.expectEqualStrings(cwd_buf[0..cwd_len], content[0 .. content.len - 1]); // trim trailing \n
}

test "command: bypasses builtin echo" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const f: TmpPath = .join(tmp, "cmd_echo.txt");
    try testcmd("command echo foo > {f}", .{f});

    var buf: [4096]u8 = undefined;
    const content = try f.read(&buf);
    try testing.expectEqualStrings("foo\n", content);
}

test "command: forwards exit code" {
    try testing.expectError(error.CmdFailed, testcmd("command false", .{}));
}
