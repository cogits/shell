//! Represents in-progress parsing, will be converted to an Ast after completion.

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Parser = @This();
const Token = @import("tokenizer.zig").Token;
const TokenIndex = Ast.TokenIndex;

pub const Error = error{ParseError} || Allocator.Error;

gpa: Allocator,
source: []const u8,
token_tags: []const Token.Tag,
tok_i: TokenIndex,
nodes: Ast.NodeList,
extra_data: std.ArrayListUnmanaged(Node.Index),
scratch: std.ArrayListUnmanaged(Node.Index),

/// grammer:
/// commandline → list (";" | "&")?
/// list        → pipeline ((";" | "&") pipeline)*
/// pipeline    → redirection ("|" redirection)*
/// redirection → command (("<" | ">" | ">>" | "2>" | "2>>") file)*
/// command     → string+ | "(" commandline ")"
pub fn parseRoot(p: *Parser) !void {
    assert(p.token_tags.len > 1);
    // Root node must be index 0.
    p.nodes.appendAssumeCapacity(.{
        .tag = .list,
        .data = undefined,
    });

    const cmds = try p.parseList();
    if (cmds.end == cmds.start) return error.ParseError;
    assert(p.token_tags[p.tok_i] == .eof);

    p.nodes.items(.data)[0] = .{
        .lhs = cmds.start,
        .rhs = cmds.end,
    };
}

pub fn deinit(p: *Parser) void {
    p.nodes.deinit(p.gpa);
    p.extra_data.deinit(p.gpa);
    p.scratch.deinit(p.gpa);
}

fn parseCmdLine(p: *Parser) !Node.Index {
    const list_range = try p.parseList();
    switch (list_range.end - list_range.start) {
        0 => return error.ParseError,
        1 => return p.extra_data.pop(),
        else => {},
    }

    return p.addNode(.{
        .tag = .list,
        .data = .{
            .lhs = list_range.start,
            .rhs = list_range.end,
        },
    });
}

fn parseList(p: *Parser) !Node.SubRange {
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
                try p.scratch.append(p.gpa, pipe_node);
                break;
            },
            .semicolon => {
                try p.scratch.append(p.gpa, pipe_node);
                p.tok_i += 1;
                continue;
            },
            .ampersand => {
                const back_node = try p.addNode(.{
                    .tag = .back,
                    .data = .{
                        .lhs = pipe_node,
                        .rhs = undefined,
                    },
                });
                try p.scratch.append(p.gpa, back_node);
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
            .data = .{
                .lhs = redir_node,
                .rhs = try p.parsePipe(),
            },
        });
    }

    return redir_node;
}

fn parseRedir(p: *Parser) !Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const exec_node = try p.parseExec();
    const file_range = try p.parseFiles();
    if (file_range.end == file_range.start) return exec_node;

    const redir_token_tags: [5]Token.Tag = .{
        .stdin_redir,
        .stdout_redir,
        .stdout_append_redir,
        .stderr_redir,
        .stderr_append_redir,
    };

    var token_ranges = [1]Node.TokenRange{.{}} ** 5;
    var items_index = file_range.start;
    for (redir_token_tags, 0..) |tag, i| {
        for (p.extra_data.items[items_index..file_range.end]) |item| {
            if (p.token_tags[item - 1] != tag) {
                if (token_ranges[i].start != null)
                    token_ranges[i].end = items_index;
                break;
            }

            if (token_ranges[i].start == null)
                token_ranges[i].start = items_index;
            items_index += 1;
        }

        const file_node = if (token_ranges[i].start) |start|
            try p.addNode(.{ .tag = .files, .data = .{
                .lhs = start,
                .rhs = token_ranges[i].end orelse items_index,
            } })
        else
            0;
        try p.scratch.append(p.gpa, file_node);
    }

    const items = p.scratch.items[scratch_top..];
    const range = try p.listToSpan(items);

    return p.addNode(.{
        .tag = if (p.token_tags[p.tok_i] == .pipe) .redir_pipe else .redir,
        .data = .{
            .lhs = exec_node,
            .rhs = range.start,
        },
    });
}

fn parseFiles(p: *Parser) !Node.SubRange {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        switch (p.token_tags[p.tok_i]) {
            .stdin_redir,
            .stdout_redir,
            .stdout_append_redir,
            .stderr_redir,
            .stderr_append_redir,
            => {
                p.tok_i += 1;
                try p.scratch.append(p.gpa, p.eatToken(.string) orelse return error.ParseError);
            },
            else => break,
        }
    }

    const items = p.scratch.items[scratch_top..];
    if (items.len == 0) return .{ .start = 0, .end = 0 };

    const S = struct {
        pub fn lessThan(token_tags: []const Token.Tag, a: TokenIndex, b: TokenIndex) bool {
            return @intFromEnum(token_tags[a - 1]) < @intFromEnum(token_tags[b - 1]);
        }
    };

    std.sort.insertion(TokenIndex, items, p.token_tags, S.lessThan);
    return p.listToSpan(items);
}

fn parseExec(p: *Parser) !Node.Index {
    if (p.eatToken(.l_paren)) |_| return p.parseBlock();
    const start = p.eatToken(.string) orelse
        p.eatToken(.builtin_cd) orelse
        return error.ParseError;

    while (true) : (p.tok_i += 1) {
        switch (p.token_tags[p.tok_i]) {
            .string, .builtin_cd => continue,
            else => break,
        }
    }

    return p.addNode(.{
        .tag = if (p.token_tags[start] == .builtin_cd) .builtin else .exec,
        .data = .{
            .lhs = start,
            .rhs = p.tok_i,
        },
    });
}

fn parseBlock(p: *Parser) Error!Node.Index {
    const cmdline_node = try p.parseCmdLine();
    _ = p.eatToken(.r_paren) orelse return error.ParseError;
    return cmdline_node;
}

fn listToSpan(p: *Parser, list: []const Node.Index) !Node.SubRange {
    try p.extra_data.appendSlice(p.gpa, list);
    return Node.SubRange{
        .start = @intCast(p.extra_data.items.len - list.len),
        .end = @intCast(p.extra_data.items.len),
    };
}

fn addNode(p: *Parser, elem: Node) Allocator.Error!Node.Index {
    const result: Node.Index = @intCast(p.nodes.len);
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
