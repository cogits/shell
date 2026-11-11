//! Compares the handwritten parser (Ast/Parser.zig + Ast/Tokenizer.zig) against
//! the grammar oracle generated from tests/grammar.peg.
//!
//! The oracle works on a token stream: the real tokenizer runs first, and each
//! token tag is encoded as one byte (see tagToByte). This means the oracle does
//! not check the tokenizer itself; it checks that the parser accepts exactly the
//! token sequences described by the grammar.

const std = @import("std");
const Ast = @import("Ast");
const Tokenizer = Ast.Tokenizer;
const oracle = @import("grammar_oracle");

const Tag = Tokenizer.Token.Tag;

/// One byte per token, as expected by tests/grammar.peg. Keep in sync with the
/// token table documented at the top of that file.
fn tagToByte(tag: Tag) ?u8 {
    return switch (tag) {
        .eof, .invalid => null,
        .string => 'S',
        .l_paren => '(',
        .r_paren => ')',
        .semicolon => ';',
        .ampersand => '&',
        .@"and" => 'A',
        .@"or" => 'O',
        .pipe => '|',
        .stdin => '<',
        .stdout => '>',
        .stdout_append => 'R',
        .stderr => '2',
        .stderr_append => 'E',
    };
}

/// Result the generated oracle expects for `source`.
fn oracleExpected(gpa: std.mem.Allocator, source: []const u8) !bool {
    if (source.len == 0) return true;

    var tags: std.ArrayList(u8) = .empty;
    defer tags.deinit(gpa);

    var tokenizer: Tokenizer = .init(source);
    while (true) {
        const token = tokenizer.next();
        switch (token.tag) {
            // Ast.parse aborts on the first invalid token, so the whole input
            // is rejected regardless of what comes after it.
            .invalid => return false,
            .eof => break,
            else => try tags.append(gpa, tagToByte(token.tag).?),
        }
    }

    // Ast.parse returns error.EmptyCmd when the token list contains only eof.
    if (tags.items.len == 0) return true;

    return oracle.parse(tags.items);
}

fn checkOne(source: []const u8) !void {
    const gpa = std.testing.allocator;

    const expected = oracleExpected(gpa, source) catch |err| switch (err) {
        error.MaxDepth => return error.SkipZigTest,
        else => return err,
    };

    var error_token: []const u8 = undefined;
    var tree = Ast.parse(gpa, source, &error_token) catch |err| switch (err) {
        error.EmptyCmd => {
            if (!expected) reportMismatch(source, expected, "EmptyCmd");
            try std.testing.expect(expected);
            return;
        },
        error.ParseError, error.TokenizeError => {
            if (expected) reportMismatch(source, expected, @errorName(err));
            try std.testing.expect(!expected);
            return;
        },
        else => return err,
    };
    defer tree.deinit(gpa);

    if (!expected) reportMismatch(source, expected, "parse succeeded");
    try std.testing.expect(expected);
}

fn reportMismatch(source: []const u8, oracle_accepted: bool, actual: []const u8) void {
    std.debug.print("=== parser/oracle mismatch ===\n", .{});
    std.debug.print("source ({d} bytes): {s}\n", .{ source.len, source });
    std.debug.print("oracle accepted: {}\n", .{oracle_accepted});
    std.debug.print("handwritten parser: {s}\n", .{actual});
    std.debug.print("tokens:", .{});
    var tokenizer: Tokenizer = .init(source);
    while (true) {
        const token = tokenizer.next();
        std.debug.print(" {t}", .{token.tag});
        if (token.tag == .eof) break;
    }
    std.debug.print("\n", .{});
}

const valid_cases = [_][]const u8{
    "",
    "   ",
    "ls",
    "cd exit",
    "ls; pwd",
    "sleep & echo ':)'; ls",
    "ls &",
    "ls && pwd",
    "ls || pwd",
    "ls && pwd || date",
    "ls | cat | cat",
    "ls 2> file1 >> file2 >> file3 > file4 2>> file5",
    "(ls)",
    "(ls;)",
    "(ls | cat > file; sleep 1)& echo hi",
    "echo hi 2> file1 > file2 | cat > file4; sleep 10 & ls",
    "\"a\"b",
    "a2>b",
    "2 > f",
    "'quoted string' \"another\"",
};

const invalid_cases = [_][]const u8{
    ";",
    "ls >",
    "ls |",
    "()",
    ")",
    "(",
    "echo 'file | cat",
    "&@",
    "))",
    "echo :)",
    "pwd ||| cat",
    "2>f",
    "&&&",
    "a & & b",
    "a ; ; b",
    "ls | | cat",
    "ls >>> f",
    "ls ; ; pwd",
};

const corpus: []const []const u8 = &(valid_cases ++ invalid_cases);

test "oracle accepts valid command lines" {
    for (valid_cases) |source| {
        checkOne(source) catch |err| switch (err) {
            error.SkipZigTest => continue,
            else => return err,
        };
    }
}

test "oracle rejects invalid command lines" {
    for (invalid_cases) |source| {
        checkOne(source) catch |err| switch (err) {
            error.SkipZigTest => continue,
            else => return err,
        };
    }
}

test "fuzz handwritten parser against grammar oracle" {
    try std.testing.fuzz({}, fuzzOne, .{ .corpus = corpus });
}

fn fuzzOne(_: void, smith: *std.testing.Smith) !void {
    var buffer: [1 << 14]u8 = undefined;
    const len = smith.slice(buffer[0 .. buffer.len - 1]);
    try checkOne(buffer[0..len]);
}
