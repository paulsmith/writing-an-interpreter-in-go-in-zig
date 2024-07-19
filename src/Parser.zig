const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const ast = @import("ast.zig");
const assert = std.debug.assert;

allocator: Allocator,
lexer: *Lexer,
cur_token: Token = undefined,
peek_token: Token = undefined,
error_list: [1024]u8 = undefined,
error_list_ptr: usize = 0,

const Self = @This();

pub fn init(allocator: Allocator, lexer: *Lexer) Self {
    var parser = Self{ .allocator = allocator, .lexer = lexer };
    (&parser).nextToken();
    (&parser).nextToken();
    return parser;
}

pub fn nextToken(self: *Self) void {
    self.cur_token = self.peek_token;
    self.peek_token = self.lexer.next();
}

pub fn deinit(self: *Self, program: *ast.Program) void {
    for (program.statements) |stmt| {
        switch (stmt) {
            .let => |s| {
                self.allocator.destroy(s.name);
                self.allocator.destroy(s);
            },
            .program => unreachable,
        }
    }
    self.allocator.free(program.statements);
    self.allocator.destroy(program);
}

fn fmtError(self: *Self, comptime fmt: []const u8, args: anytype) void {
    assert(self.error_list_ptr < 1024);
    const result = std.fmt.bufPrint(self.error_list[self.error_list_ptr..], fmt ++ "\n", args) catch unreachable;
    self.error_list_ptr += result.len;
}

pub fn parseProgram(self: *Self) !*ast.Program {
    var program = try self.allocator.create(ast.Program);
    errdefer self.allocator.destroy(program);

    var statements = std.ArrayList(ast.Statement).init(self.allocator);
    errdefer {
        for (statements.items) |stmt| {
            switch (stmt) {
                .let => |s| {
                    self.allocator.destroy(s.name);
                    self.allocator.destroy(s);
                },
                .program => unreachable,
            }
        }
        statements.deinit();
    }

    while (self.cur_token.token_type != .eof) {
        const stmt = try self.parseStatement();
        if (stmt) |s| {
            try statements.append(s);
        }
        self.nextToken();
    }
    program.statements = try statements.toOwnedSlice();
    return program;
}

fn parseStatement(self: *Self) !?ast.Statement {
    switch (self.cur_token.token_type) {
        .let => {
            return .{ .let = try self.parseLetStatement() orelse return null };
        },
        else => {
            return null;
        },
    }
}

fn parseLetStatement(self: *Self) !?*ast.LetStatement {
    var stmt = try self.allocator.create(ast.LetStatement);
    errdefer self.allocator.destroy(stmt);

    stmt.token = self.cur_token;

    if (!self.expectPeek(.ident)) {
        self.allocator.destroy(stmt); // !!
        return null;
    }

    stmt.name = try self.allocator.create(ast.Identifier);
    errdefer {
        self.allocator.destroy(stmt.name); // !!
        self.allocator.destroy(stmt); // !!
    }
    stmt.name.* = .{ .token = self.cur_token, .value = self.cur_token.literal };

    if (!self.expectPeek(.assign)) {
        self.allocator.destroy(stmt.name); // !!
        self.allocator.destroy(stmt); // !!
        return null;
    }

    while (!self.curTokenIs(.semicolon)) {
        self.nextToken();
    }

    return stmt;
}

fn curTokenIs(self: *Self, t: Token.Type) bool {
    return self.cur_token.token_type == t;
}

fn peekTokenIs(self: *Self, t: Token.Type) bool {
    return self.peek_token.token_type == t;
}

fn expectPeek(self: *Self, t: Token.Type) bool {
    if (self.peekTokenIs(t)) {
        self.nextToken();
        return true;
    }
    self.peekError(t);
    return false;
}

pub fn hasErrors(self: *Self) bool {
    return self.error_list_ptr > 0;
}

pub fn peekError(self: *Self, expected: Token.Type) void {
    self.fmtError("next token: expected {s}, got {s}", .{ expected.name(), self.peek_token.token_type.name() });
}

fn testLetStatement(stmt: *ast.LetStatement, name: []const u8) !void {
    try std.testing.expectEqualStrings(stmt.tokenLit(), "let");
    try std.testing.expectEqualStrings(stmt.name.value, name);
}

fn checkParserErrors(parser: *Self) !void {
    if (parser.hasErrors()) {
        std.debug.print("error(s):\n{s}\n", .{parser.error_list});
        try std.testing.expect(false);
    }
}

test {
    var lexer = Lexer.init(
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 8383;
    );

    var parser = init(std.testing.allocator, &lexer);

    const program = try (&parser).parseProgram();
    defer parser.deinit(program);

    try checkParserErrors(&parser);
    try std.testing.expectEqual(program.statements.len, 3);

    const expectedIdents = [_][]const u8{ "x", "y", "foobar" };
    for (expectedIdents, 0..) |ident, i| {
        const stmt = program.statements[i];
        try testLetStatement(stmt.let, ident);
    }
}
