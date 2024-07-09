const std = @import("std");

pub const Token = struct {
    tag: Tag,
    lexeme: []const u8,

    pub const symbols = std.StaticStringMap(Tag).initComptime(.{
        .{ "(", .l_paren },
        .{ ")", .r_paren },
        .{ ";", .semicolon },
        .{ "&", .ampersand },
        .{ "|", .pipe },
        .{ "<", .stdin_redir },
        .{ ">", .stdout_redir },
        .{ ">>", .stdout_append_redir },
        .{ "2>", .stderr_redir },
        .{ "2>>", .stderr_append_redir },
    });

    pub const builtins = std.StaticStringMap(Tag).initComptime(.{
        .{ "cd", .builtin_cd },
    });

    pub const Tag = enum {
        eof,
        invalid,
        builtin_cd,
        string,
        l_paren,
        r_paren,
        semicolon,
        ampersand,
        pipe,
        stdin_redir,
        stdout_redir,
        stdout_append_redir,
        stderr_redir,
        stderr_append_redir,
    };

    pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.print("'{s}' :: {s}", .{ self.lexeme, @tagName(self.tag) });
    }
};

pub const Tokenizer = struct {
    index: usize,
    buffer: [:0]const u8,

    const whitespace = " \t\r\n\x0b";
    const symbols = "<|>&;()";

    pub fn init(buffer: [:0]const u8) Tokenizer {
        return Tokenizer{
            .buffer = buffer,
            .index = 0,
        };
    }

    pub fn next(self: *Tokenizer) Token {
        var result = Token{
            .tag = .eof,
            .lexeme = "\\n",
        };

        // skip whitespace
        while (self.index < self.buffer.len) {
            switch (self.buffer[self.index]) {
                ' ', '\n', '\t', '\r', '\x0b' => self.index += 1,
                else => break,
            }
        }

        var tag: ?Token.Tag = null;
        const end: usize = switch (self.buffer[self.index]) {
            0 => return result,
            '\'', '"' => |char| blk: {
                if (self.buffer[self.index + 1] != 0) {
                    if (std.mem.indexOfScalar(u8, self.buffer[self.index + 1 ..], char)) |end| {
                        tag = .string;
                        self.index += 1;
                        break :blk end;
                    }
                }

                tag = .invalid;
                break :blk if (std.mem.indexOfAny(u8, self.buffer[self.index + 1 ..], whitespace)) |index|
                    index + 1
                else
                    self.buffer.len - self.index;
            },
            ';', '&', '|', '<', '(', ')' => 1,
            '>' => if (self.buffer[self.index + 1] == '>') 2 else 1,
            else => |char| if (char == '2' and self.buffer[self.index + 1] == '>')
                if (self.buffer[self.index + 2] == '>')
                    3
                else
                    2
            else
                std.mem.indexOfAny(u8, self.buffer[self.index..], whitespace ++ symbols) orelse
                    self.buffer.len - self.index,
        };

        result.lexeme = self.buffer[self.index .. self.index + end];
        self.index += if (tag == .string) end + 1 else end;
        result.tag = tag orelse Token.symbols.get(result.lexeme) orelse
            Token.builtins.get(result.lexeme) orelse .string;

        return result;
    }
};

fn testTokenize(source: [:0]const u8, expected_token_tags: []const Token.Tag) !void {
    var tokenizer = Tokenizer.init(source);
    for (expected_token_tags) |expected_token_tag| {
        const token = tokenizer.next();
        std.testing.expectEqual(expected_token_tag, token.tag) catch |err| {
            std.log.err("token: {}", .{token});
            return err;
        };
    }
    const last_token = tokenizer.next();
    try std.testing.expectEqual(Token.Tag.eof, last_token.tag);
}

test "tokenizer" {
    try testTokenize("cd .", &.{ .builtin_cd, .string });
    try testTokenize("echo 2", &.{ .string, .string });
    try testTokenize(">_<", &.{ .stdout_redir, .string, .stdin_redir });

    try testTokenize(
        "echo hi > file1 2>> file2 | cat",
        &.{ .string, .string, .stdout_redir, .string, .stderr_append_redir, .string, .pipe, .string },
    );

    try testTokenize(
        "echo hi&(ls | cat);",
        &.{ .string, .string, .ampersand, .l_paren, .string, .pipe, .string, .r_paren, .semicolon },
    );

    try testTokenize(
        \\echo "xx yy"
    , &.{ .string, .string });

    try testTokenize(
        \\echo "xyz'
    , &.{ .string, .invalid });

    try testTokenize(
        \\echo "xyz ''
    , &.{ .string, .invalid, .string });
}
