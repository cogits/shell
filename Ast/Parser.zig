//! Represents in-progress parsing, will be converted to an Ast after completion.

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Parser = @This();
const Token = Ast.Token;
const TokenIndex = Ast.TokenIndex;

pub const Error = error{ParseError} || Allocator.Error;

gpa: Allocator,
source: []const u8,
tok_i: TokenIndex,
token_tags: []const Token.Tag,
token_lexemes: []const Token.Lexeme,
nodes: Ast.NodeList,
extra_data: std.ArrayListUnmanaged(u32),
scratch: std.ArrayListUnmanaged(u32),

/// grammer:
/// commandline → list (";" | "&")?
/// list        → pipeline ((";" | "&") pipeline)*
/// pipeline    → redirection ("|" redirection)*
/// redirection → command (("<" | ">" | ">>" | "2>" | "2>>") file)*
/// command     → string+ | "(" commandline ")"
pub fn parseRoot(p: *Parser) !void {
    assert(p.token_tags.len > 1);
    // Root node must be index 0.
    p.nodes.appendAssumeCapacity(undefined);

    _ = try p.parseCmdLine();
    assert(p.token_tags[p.tok_i] == .eof);

    const cmds = p.nodes.pop().?;
    p.nodes.set(0, cmds);
}

pub fn deinit(p: *Parser) void {
    p.nodes.deinit(p.gpa);
    p.extra_data.deinit(p.gpa);
    p.scratch.deinit(p.gpa);
}

fn parseCmdLine(p: *Parser) !Node.Index {
    const list_range = try p.parseList();
    const start: usize = @intFromEnum(list_range.start);
    const end: usize = @intFromEnum(list_range.end);
    switch (end - start) {
        0 => return error.ParseError,
        1 => return @enumFromInt(p.extra_data.pop().?),
        else => {},
    }

    return p.addNode(.{
        .tag = .list,
        .data = .{ .extra_range = list_range },
    });
}

fn parseList(p: *Parser) !Ast.ExtraRange {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        switch (p.token_tags[p.tok_i]) {
            .eof, .r_paren => break,
            else => {},
        }

        const pipe_node = try p.parsePipe();
        switch (p.token_tags[p.tok_i]) {
            .eof, .r_paren => {
                try p.scratch.append(p.gpa, @intFromEnum(pipe_node));
                break;
            },
            .semicolon => {
                try p.scratch.append(p.gpa, @intFromEnum(pipe_node));
                p.tok_i += 1;
                continue;
            },
            .ampersand => {
                const back_node = try p.addNode(.{
                    .tag = .back,
                    .data = .{ .node = pipe_node },
                });
                try p.scratch.append(p.gpa, @intFromEnum(back_node));
                p.tok_i += 1;
                continue;
            },
            else => break,
        }
    }

    const items = p.scratch.items[scratch_top..];
    return p.listToSpan(items);
}

fn parsePipe(p: *Parser) !Node.Index {
    const redir_node = try p.parseRedir();

    if (p.eatToken(.pipe)) |_| {
        return p.addNode(.{
            .tag = .pipe,
            .data = .{ .node_and_node = .{
                redir_node,
                try p.parsePipe(),
            } },
        });
    }

    return redir_node;
}

fn parseRedir(p: *Parser) !Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const exec_node = try p.parseExec();
    const file_range = try p.parseFiles() orelse return exec_node;

    const redir_token_tags: [5]Token.Tag = .{ .stdin, .stdout, .stdout_append, .stderr, .stderr_append };

    // splitting the file_range into five possible redirect types
    var tok_i: u32 = @intFromEnum(file_range.start);
    const tok_end: usize = @intFromEnum(file_range.end);
    for (redir_token_tags) |tag| {
        var start: ?Ast.ExtraIndex = null;
        var end: ?Ast.ExtraIndex = null;

        for (p.extra_data.items[tok_i..tok_end]) |item| {
            if (p.token_tags[item - 1] != tag) {
                if (start != null)
                    end = @enumFromInt(tok_i);
                break;
            }

            if (start == null)
                start = @enumFromInt(tok_i);
            tok_i += 1;
        }

        const file_node: ?Node.Index = if (start) |s|
            try p.addNode(.{ .tag = .files, .data = .{ .extra_range = .{
                .start = s,
                .end = end orelse @enumFromInt(tok_i),
            } } })
        else
            null;

        const redir_i: Node.OptionalIndex = if (file_node) |node| node.toOptional() else .none;
        try p.scratch.append(p.gpa, @intFromEnum(redir_i));
    }

    const items = p.scratch.items[scratch_top..];
    const range = try p.listToSpan(items);

    return p.addNode(.{
        .tag = if (p.token_tags[p.tok_i] == .pipe) .piped_redir else .redir,
        .data = .{ .node_and_extra = .{
            exec_node,
            range.start,
        } },
    });
}

fn parseFiles(p: *Parser) !?Ast.ExtraRange {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        switch (p.token_tags[p.tok_i]) {
            .stdin,
            .stdout,
            .stdout_append,
            .stderr,
            .stderr_append,
            => {
                p.tok_i += 1;
                const file = p.eatToken(.string) orelse return error.ParseError;
                try p.scratch.append(p.gpa, file);
            },
            else => break,
        }
    }

    const items = p.scratch.items[scratch_top..];
    if (items.len == 0) return null;

    const S = struct {
        pub fn lessThan(token_tags: []const Token.Tag, a: TokenIndex, b: TokenIndex) bool {
            return @intFromEnum(token_tags[a - 1]) < @intFromEnum(token_tags[b - 1]);
        }
    };

    std.sort.insertion(TokenIndex, items, p.token_tags, S.lessThan);
    return try p.listToSpan(items);
}

fn parseExec(p: *Parser) !Node.Index {
    if (p.eatToken(.l_paren)) |_| return p.parseBlock();
    const start = p.eatToken(.string) orelse return error.ParseError;

    while (true) : (p.tok_i += 1) {
        switch (p.token_tags[p.tok_i]) {
            .string => continue,
            else => break,
        }
    }

    const is_builtin = std.meta.stringToEnum(Node.Builtin, p.token_lexemes[start]) != null;
    return p.addNode(.{
        .tag = if (is_builtin) .builtin else .exec,
        .data = .{ .token_range = .{
            .start = start,
            .end = p.tok_i,
        } },
    });
}

fn parseBlock(p: *Parser) Error!Node.Index {
    const cmdline_node = try p.parseCmdLine();
    _ = p.eatToken(.r_paren) orelse return error.ParseError;
    return cmdline_node;
}

fn listToSpan(p: *Parser, list: anytype) !Ast.ExtraRange {
    try p.extra_data.appendSlice(p.gpa, @ptrCast(list));
    return .{
        .start = @enumFromInt(p.extra_data.items.len - list.len),
        .end = @enumFromInt(p.extra_data.items.len),
    };
}

fn addNode(p: *Parser, elem: Node) !Node.Index {
    const result: Node.Index = @enumFromInt(p.nodes.len);
    try p.nodes.append(p.gpa, elem);
    return result;
}

fn eatToken(p: *Parser, tag: Token.Tag) ?TokenIndex {
    return if (p.token_tags[p.tok_i] == tag) p.nextToken() else null;
}

fn nextToken(p: *Parser) TokenIndex {
    const result = p.tok_i;
    p.tok_i += 1;
    return result;
}
