const std = @import("std");
const assert = std.debug.assert;
const Ast = @This();
const Allocator = std.mem.Allocator;
const Parser = @import("Parser.zig");
const Token = @import("tokenizer.zig").Token;
const Tokenizer = @import("tokenizer.zig").Tokenizer;

/// Reference to externally-owned data.
source: [:0]const u8,

tokens: TokenList.Slice,
/// The root AST node is assumed to be index 0. Since there can be no
/// references to the root node, this means 0 is available to indicate null.
nodes: NodeList.Slice,
extra_data: []Node.Index,
error_token: ?[]const u8 = null,

pub const TokenIndex = u32;

pub const TokenList = std.MultiArrayList(Token);
pub const NodeList = std.MultiArrayList(Node);

pub const Node = struct {
    tag: Tag,
    data: Data,

    pub const Index = u32;

    pub const Tag = enum {
        /// `cmd1 ; cmd2`
        list,
        /// `exec &`
        back,
        /// `exec1 | exec2`
        pipe,
        /// `exec files* ...`
        redir,
        /// Same as redir but there is known to be a `|` after redir.
        redir_pipe,
        /// (>|>>|2>..) file
        files,
        /// `cmd arg1 arg2...`, tokens[lhs...rhs]
        exec,
        /// builtin cmds
        builtin,
    };

    pub const Data = struct {
        lhs: Index,
        rhs: Index,
    };

    pub const SubRange = struct {
        /// Index into sub_list.
        start: Index,
        /// Index into sub_list.
        end: Index,
    };

    pub const TokenRange = struct {
        start: ?TokenIndex = null,
        end: ?TokenIndex = null,
    };

    pub const Redirection = struct {
        stdin: Index,
        stdout: Index,
        stdout_append: Index,
        stderr: Index,
        stderr_append: Index,
    };

    pub fn format(node: Node, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.print("{s}: ", .{@tagName(node.tag)});
        switch (node.tag) {
            .list, .files => try w.print("extra_data[{}..{}]", node.data),
            .redir, .redir_pipe => try w.print("nodes[{}] extra_data[{}]", node.data),
            .exec, .builtin => try w.print("tokens[{}..{}]", node.data),
            .pipe => try w.print("nodes[{}] nodes[{}]", node.data),
            .back => try w.print("nodes[{}]", .{node.data.lhs}),
        }
    }
};

/// Result should be freed with tree.deinit() when there are
/// no more references to any of the tokens or nodes.
pub fn parse(gpa: Allocator, source: [:0]const u8) Allocator.Error!?Ast {
    if (source.len == 0) return null;

    var ast = Ast{
        .source = source,
        .tokens = undefined,
        .nodes = undefined,
        .extra_data = undefined,
    };

    var tokens = Ast.TokenList{};
    defer tokens.deinit(gpa);

    // 4:1 ratio of source bytes to token count.
    const estimated_token_count = source.len / 4;
    try tokens.ensureTotalCapacity(gpa, estimated_token_count);

    var tokenizer = Tokenizer.init(source);
    while (true) {
        const token = tokenizer.next();
        if (token.tag == .invalid) {
            ast.error_token = token.lexeme;
            return ast;
        }
        try tokens.append(gpa, token);
        if (token.tag == .eof) break;
    }

    if (tokens.len <= 1) return null;

    var parser: Parser = .{
        .source = source,
        .gpa = gpa,
        .token_tags = tokens.items(.tag),
        .nodes = .{},
        .extra_data = .{},
        .scratch = .{},
        .tok_i = 0,
    };
    defer parser.deinit();

    // 2:1 ratio of tokens to AST nodes. Make sure at least 1 so
    // we can use appendAssumeCapacity on the root node below.
    const estimated_node_count = (tokens.len + 2) / 2;
    try parser.nodes.ensureTotalCapacity(gpa, estimated_node_count);

    parser.parseRoot() catch |err| switch (err) {
        error.ParseError => {
            ast.error_token = if (parser.tok_i == parser.token_tags.len - 1)
                tokens.get(parser.tok_i - 1).lexeme
            else
                tokens.get(parser.tok_i).lexeme;
            return ast;
        },
        else => |e| return e,
    };

    ast.tokens = tokens.toOwnedSlice();
    ast.nodes = parser.nodes.toOwnedSlice();
    ast.extra_data = try parser.extra_data.toOwnedSlice(gpa);
    return ast;
}

pub fn deinit(tree: *Ast, gpa: Allocator) void {
    if (tree.error_token == null) {
        tree.tokens.deinit(gpa);
        tree.nodes.deinit(gpa);
        gpa.free(tree.extra_data);
    }
    tree.* = undefined;
}

pub fn format(ast: Ast, comptime fmt: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
    if (comptime std.mem.eql(u8, fmt, "r")) {
        try ast.rawFormat(w);
    } else {
        try ast.print(w, 0);
    }
}

fn print(ast: Ast, w: anytype, index: Node.Index) !void {
    const node = ast.nodes.get(index);
    switch (node.tag) {
        .list => {
            if (index != 0) try w.writeAll("(");
            defer if (index != 0) w.writeAll(")") catch unreachable;

            const cmds = ast.extra_data[node.data.lhs..node.data.rhs];
            try ast.print(w, cmds[0]);
            for (cmds[1..], 0..) |cmd, i| {
                try w.writeAll(if (ast.nodes.get(cmds[i]).tag == .back) " " else " ; ");
                try ast.print(w, cmd);
            }
        },
        .exec, .builtin => {
            const tokens = ast.tokens.items(.lexeme)[node.data.lhs..node.data.rhs];
            try w.print("{s}", .{tokens[0]});
            for (tokens[1..]) |token| {
                try w.print(" {s}", .{token});
            }
        },
        .back => {
            try ast.print(w, node.data.lhs);
            try w.writeAll(" &");
        },
        .redir, .redir_pipe => {
            try ast.print(w, node.data.lhs);
            const extra = ast.extraData(node.data.rhs, Node.Redirection);

            inline for (std.meta.fields(Node.Redirection), .{ "<", ">", ">>", "2>", "2>>" }) |field, symbol| {
                const field_index = @field(extra, field.name);
                if (field_index != 0) {
                    const field_node = ast.nodes.get(field_index);
                    const files = ast.extra_data[field_node.data.lhs..field_node.data.rhs];
                    for (files) |file| {
                        try w.print(" {s} {s}", .{ symbol, ast.tokens.items(.lexeme)[file] });
                    }
                }
            }
        },
        .pipe => {
            try ast.print(w, node.data.lhs);
            try w.writeAll(" | ");
            try ast.print(w, node.data.rhs);
        },
        else => unreachable,
    }
}

fn rawFormat(ast: Ast, w: anytype) !void {
    // print tokens
    try w.print("tokens({}) =>\n", .{ast.tokens.len});
    for (ast.tokens.items(.tag), ast.tokens.items(.lexeme), 0..) |tag, lexeme, i| {
        try w.print("[{}] {s} :: {s}\n", .{ i, lexeme, @tagName(tag) });
    }

    // print nodes
    try w.print("nodes({}) =>\n", .{ast.nodes.len});
    for (0..ast.nodes.len) |i| {
        const node = ast.nodes.get(i);
        try w.print("[{}] {}\n", .{ i, node });
    }

    // print extra_data
    try w.print("extra_data({}) =>\n", .{ast.extra_data.len});
    for (ast.extra_data, 0..) |extra, idx| {
        try w.print("[{}] ", .{idx});
        if (extra == 0) {
            try w.writeAll("null\n");
        } else {
            try w.print("nodes[{}]\n", .{extra});
        }
    }
}

pub fn extraData(tree: Ast, index: usize, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        comptime assert(field.type == Node.Index);
        @field(result, field.name) = tree.extra_data[index + i];
    }
    return result;
}

fn testParse(source: [:0]const u8, expected: []const u8) !void {
    const allocator = std.testing.allocator;
    var tree = try parse(allocator, source) orelse return;
    defer tree.deinit(allocator);

    assert(tree.error_token == null);

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();
    const bw = buffer.writer();

    try bw.print("{s}", .{tree});
    try std.testing.expectEqualStrings(expected, buffer.items);
}

fn testError(source: [:0]const u8) !void {
    const allocator = std.testing.allocator;
    var tree = try parse(allocator, source) orelse unreachable;
    defer tree.deinit(allocator);

    assert(tree.error_token != null);
}

test "parse" {
    try testParse(
        "echo hello ; sleep & ls",
        "echo hello ; sleep & ls",
    );
    try testParse(
        "ls 2> file1 >> file2 >> file3 > file4 2>> file5",
        "ls > file4 >> file2 >> file3 2> file1 2>> file5",
    );
    try testParse(
        "ls | cat | cat",
        "ls | cat | cat",
    );
    try testParse(
        "echo > file > file2 > file3",
        "echo > file > file2 > file3",
    );
    try testParse(
        "echo hi 2> file1 > file2 | cat > file4; sleep 10 & ls",
        "echo hi > file2 2> file1 | cat > file4 ; sleep 10 & ls",
    );
    try testParse(
        "(ls;)",
        "ls",
    );
    try testParse(
        "(ls | cat > file; sleep 1)& echo hi",
        "(ls | cat > file ; sleep 1) & echo hi",
    );
    try testParse(
        "cd cd",
        "cd cd",
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
}
