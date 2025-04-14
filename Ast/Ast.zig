const Ast = @This();
const std = @import("std");
const root = @import("root");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
pub const Token = Tokenizer.Token;
pub const Error = error{ EmptyCmd, TokenizeError } || Parser.Error;

/// Reference to externally-owned data.
source: [:0]const u8,

tokens: TokenList.Slice,
/// The root AST node is assumed to be index 0.
nodes: NodeList.Slice,
extra_data: []u32,

/// Index into `tokens`.
pub const TokenIndex = u32;

/// Index into `extra_data`.
pub const ExtraIndex = enum(u32) {
    _,
};

pub const TokenRange = struct {
    start: TokenIndex,
    end: TokenIndex,
};

pub const ExtraRange = struct {
    start: ExtraIndex,
    end: ExtraIndex,
};

pub const TokenList = std.MultiArrayList(Token);
pub const NodeList = std.MultiArrayList(Node);

pub const Node = struct {
    tag: Tag,
    data: Data,

    /// Index into `nodes`.
    pub const Index = enum(u32) {
        root = 0,
        _,

        pub fn toOptional(i: Index) OptionalIndex {
            const result: OptionalIndex = @enumFromInt(@intFromEnum(i));
            assert(result != .none);
            return result;
        }
    };

    /// Index into `nodes`, or null.
    pub const OptionalIndex = enum(u32) {
        root = 0,
        none = std.math.maxInt(u32),
        _,

        pub fn unwrap(oi: OptionalIndex) ?Index {
            return if (oi == .none) null else @enumFromInt(@intFromEnum(oi));
        }
    };

    pub const Tag = enum {
        /// `cmd1 ; cmd2 ...`
        list,
        /// `exec &`
        back,
        /// `cmd1 && cmd2`
        @"and",
        /// `cmd1 || cmd2`
        @"or",
        /// `exec1 | exec2`
        pipe,
        /// `exec files* ...` (standalone)
        redir,
        /// `exec files* ... | next` (part of pipeline)
        piped_redir,
        /// (>|>>|2>..) file
        files,
        /// `cmd arg1 arg2...`
        exec,
        /// builtin command
        builtin,
    };

    /// Builtin commands that can be overridden by the root file.
    pub const Builtin = if (@hasDecl(root, "Builtin")) root.Builtin else enum { cd, exit };

    pub const Data = union {
        node: Index,
        node_and_node: struct { Index, Index },
        node_and_extra: struct { Index, ExtraIndex },
        extra_range: ExtraRange,
        token_range: TokenRange,
    };

    pub const Redirection = struct {
        stdin: OptionalIndex,
        stdout: OptionalIndex,
        stdout_append: OptionalIndex,
        stderr: OptionalIndex,
        stderr_append: OptionalIndex,
    };

    pub fn format(node: Node, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.print("{s}: ", .{@tagName(node.tag)});
        const data = node.data;
        switch (node.tag) {
            .list, .files => |tag| try w.print(
                "{s}[extra[{}..{}]]",
                .{ if (tag == .list) "nodes" else "tokens", @intFromEnum(data.extra_range.start), @intFromEnum(data.extra_range.end) },
            ),
            .redir, .piped_redir => try w.print(
                "nodes[{}] nodes[extra[{}..][0..{}]]",
                .{ @intFromEnum(data.node_and_extra[0]), @intFromEnum(data.node_and_extra[1]), std.meta.fields(Redirection).len },
            ),
            .exec, .builtin => try w.print(
                "tokens[{}..{}]",
                .{ data.token_range.start, data.token_range.end },
            ),
            .@"and", .@"or", .pipe => try w.print(
                "nodes[{}] nodes[{}]",
                .{ @intFromEnum(data.node_and_node[0]), @intFromEnum(data.node_and_node[1]) },
            ),
            .back => try w.print("nodes[{}]", .{@intFromEnum(data.node)}),
        }
    }
};

/// Result should be freed with tree.deinit() when there are
/// no more references to any of the tokens or nodes.
pub fn parse(gpa: Allocator, source: [:0]const u8, error_token: *[]const u8) Error!Ast {
    if (source.len == 0) return Error.EmptyCmd;

    var ast: Ast = .{
        .source = source,
        .tokens = undefined,
        .nodes = undefined,
        .extra_data = undefined,
    };

    var tokens: TokenList = .{};
    defer tokens.deinit(gpa);

    // 4:1 ratio of source bytes to token count.
    const estimated_token_count = source.len / 4;
    try tokens.ensureTotalCapacity(gpa, estimated_token_count);

    var tokenizer: Tokenizer = .init(source);
    while (true) {
        const token = tokenizer.next();
        if (token.tag == .invalid) {
            error_token.* = token.lexeme;
            return Error.TokenizeError;
        }
        try tokens.append(gpa, token);
        if (token.tag == .eof) break;
    }

    if (tokens.len <= 1) return Error.EmptyCmd;

    var parser: Parser = .{
        .source = source,
        .gpa = gpa,
        .tok_i = 0,
        .token_tags = tokens.items(.tag),
        .token_lexemes = tokens.items(.lexeme),
        .nodes = .{},
        .extra_data = .{},
        .scratch = .{},
    };
    defer parser.deinit();

    // 2:1 ratio of tokens to AST nodes. Make sure at least 1 so
    // we can use appendAssumeCapacity on the root node below.
    const estimated_node_count = (tokens.len + 2) / 2;
    try parser.nodes.ensureTotalCapacity(gpa, estimated_node_count);

    parser.parseRoot() catch |err| switch (err) {
        error.ParseError => {
            error_token.* = if (parser.tok_i == parser.token_tags.len - 1)
                tokens.get(parser.tok_i - 1).lexeme
            else
                tokens.get(parser.tok_i).lexeme;
            return err;
        },
        else => return err,
    };

    ast.tokens = tokens.toOwnedSlice();
    ast.nodes = parser.nodes.toOwnedSlice();
    ast.extra_data = try parser.extra_data.toOwnedSlice(gpa);
    return ast;
}

pub fn deinit(tree: *Ast, gpa: Allocator) void {
    tree.tokens.deinit(gpa);
    tree.nodes.deinit(gpa);
    gpa.free(tree.extra_data);
    tree.* = undefined;
}

pub fn format(ast: Ast, comptime fmt: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
    if (comptime std.mem.eql(u8, fmt, "r")) {
        try ast.rawFormat(w);
    } else {
        try ast.print(w, .root);
    }
}

fn print(tree: Ast, w: anytype, index: Node.Index) !void {
    const data = tree.nodeData(index);
    switch (tree.nodeTag(index)) {
        .list => {
            if (index != .root) try w.writeAll("(");
            defer if (index != .root) w.writeAll(")") catch unreachable;

            const cmds = tree.extraDataSlice(data.extra_range, Node.Index);
            try tree.print(w, cmds[0]);
            for (cmds[1..], 0..) |cmd, i| {
                try w.writeAll(if (tree.nodeTag(cmds[i]) == .back) " " else " ; ");
                try tree.print(w, cmd);
            }
        },
        .exec, .builtin => {
            const tokens = tree.tokens.items(.lexeme)[data.token_range.start..data.token_range.end];
            try w.print("{s}", .{tokens[0]});
            for (tokens[1..]) |token| {
                try w.print(" {s}", .{token});
            }
        },
        .back => {
            try tree.print(w, data.node);
            try w.writeAll(" &");
        },
        .redir, .piped_redir => {
            try tree.print(w, data.node_and_extra[0]);
            const redir = tree.extraData(data.node_and_extra[1], Node.Redirection);

            inline for (std.meta.fields(Node.Redirection), .{ "<", ">", ">>", "2>", "2>>" }) |field, symbol| {
                const redir_node = @field(redir, field.name);
                if (redir_node.unwrap()) |node| {
                    const files = tree.extraDataSlice(tree.nodeData(node).extra_range, TokenIndex);
                    const tokens = tree.tokens.items(.lexeme);
                    for (files) |file| {
                        try w.print(" {s} {s}", .{ symbol, tokens[file] });
                    }
                }
            }
        },
        .@"and", .@"or", .pipe => |tag| {
            if (index != .root) try w.writeAll("(");
            defer if (index != .root) w.writeAll(")") catch unreachable;

            try tree.print(w, data.node_and_node[0]);
            try w.print(" {s} ", .{switch (tag) {
                .@"and" => "&&",
                .@"or" => "||",
                .pipe => "|",
                else => unreachable,
            }});
            try tree.print(w, data.node_and_node[1]);
        },
        else => unreachable,
    }
}

fn rawFormat(tree: Ast, w: anytype) !void {
    // print tokens
    try w.print("tokens({}) =>\n", .{tree.tokens.len});
    for (tree.tokens.items(.tag), tree.tokens.items(.lexeme), 0..) |tag, lexeme, i| {
        try w.print("[{}] {s} :: {s}\n", .{ i, lexeme, @tagName(tag) });
    }

    // print nodes
    try w.print("nodes({}) =>\n", .{tree.nodes.len});
    for (0..tree.nodes.len) |i| {
        const node = tree.nodes.get(i);
        try w.print("[{}] {}\n", .{ i, node });
    }

    // print extra_data
    try w.print("extra({}) =>\n", .{tree.extra_data.len});
    for (tree.extra_data, 0..) |extra, idx| {
        try w.print("[{}] ", .{idx});
        if (extra == @intFromEnum(Node.OptionalIndex.none)) {
            try w.writeAll("none\n");
        } else {
            try w.print("{}\n", .{extra});
        }
    }
}

pub fn nodeTag(tree: *const Ast, node: Node.Index) Node.Tag {
    return tree.nodes.items(.tag)[@intFromEnum(node)];
}

pub fn nodeData(tree: *const Ast, node: Node.Index) Node.Data {
    return tree.nodes.items(.data)[@intFromEnum(node)];
}

pub fn extraDataSlice(tree: Ast, range: ExtraRange, comptime T: type) []const T {
    return @ptrCast(tree.extra_data[@intFromEnum(range.start)..@intFromEnum(range.end)]);
}

pub fn extraData(tree: Ast, index: ExtraIndex, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        @field(result, field.name) = switch (field.type) {
            Node.Index,
            Node.OptionalIndex,
            ExtraIndex,
            => @enumFromInt(tree.extra_data[@intFromEnum(index) + i]),
            TokenIndex => tree.extra_data[@intFromEnum(index) + i],
            else => @compileError("unexpected field type: " ++ @typeName(field.type)),
        };
    }
    return result;
}

fn testParse(source: [:0]const u8, expected: []const u8) !void {
    const allocator = std.testing.allocator;
    var error_token: []const u8 = undefined;
    var tree = parse(allocator, source, &error_token) catch |err| switch (err) {
        error.EmptyCmd => return,
        else => return err,
    };
    defer tree.deinit(allocator);

    var buffer: std.ArrayList(u8) = .init(allocator);
    defer buffer.deinit();
    const bw = buffer.writer();

    try bw.print("{s}", .{tree});
    try std.testing.expectEqualStrings(expected, buffer.items);
}

fn testError(source: [:0]const u8) !void {
    const allocator = std.testing.allocator;
    var error_token: []const u8 = undefined;
    var tree = parse(allocator, source, &error_token) catch return;
    defer tree.deinit(allocator);

    unreachable;
}

test "parse" {
    try testParse(
        "sleep & echo ':)'; ls",
        "sleep & echo :) ; ls",
    );
    try testParse(
        "ls 2> file1 >> file2 >> file3 > file4 2>> file5",
        "ls > file4 >> file2 >> file3 2> file1 2>> file5",
    );
    try testParse(
        "ls | cat | cat",
        "ls | (cat | cat)",
    );
    try testParse(
        "ls && pwd || date",
        "(ls && pwd) || date",
    );
    try testParse(
        "echo > file > file2 > file3",
        "echo > file > file2 > file3",
    );
    try testParse(
        "echo hi 2> file1 > file2 | cat > file4; sleep 10 & ls",
        "(echo hi > file2 2> file1 | cat > file4) ; sleep 10 & ls",
    );
    try testParse(
        "(ls;)",
        "ls",
    );
    try testParse(
        "(ls | cat > file; sleep 1)& echo hi",
        "((ls | cat > file) ; sleep 1) & echo hi",
    );
    try testParse(
        "cd cd",
        "cd cd",
    );
    try testParse(
        "cd exit",
        "cd exit",
    );
    try testParse(
        "   ",
        "",
    );
}

test "error" {
    try testError(";");
    try testError("ls >;");
    try testError("echo 'file | cat");
    try testError("(");
    try testError("ls |");
    try testError("&@");
    try testError("()");
    try testError("))");
    try testError("echo :)");
    try testError("pwd ||| cat");
}
