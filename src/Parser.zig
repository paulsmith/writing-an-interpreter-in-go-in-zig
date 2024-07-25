const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const ast = @import("ast.zig");
const assert = std.debug.assert;

const PrefixParseFn = *const fn (*Self) anyerror!ast.Expression;
const InfixParseFn = *const fn (*Self, ast.Expression) anyerror!ast.Expression;
const PrefixParseFnMap = std.AutoHashMap(Token.Type, PrefixParseFn);
const InfixParseFnMap = std.AutoHashMap(Token.Type, InfixParseFn);

allocator: Allocator,
lexer: *Lexer,
cur_token: Token = undefined,
peek_token: Token = undefined,
error_list: [1024]u8 = undefined,
error_list_ptr: usize = 0,
prefixParseFns: PrefixParseFnMap,
infixParseFns: InfixParseFnMap,

const Self = @This();

pub fn init(allocator: Allocator, lexer: *Lexer) Self {
    var parser = Self{
        .allocator = allocator,
        .lexer = lexer,
        .prefixParseFns = PrefixParseFnMap.init(allocator),
        .infixParseFns = InfixParseFnMap.init(allocator),
    };

    parser.registerPrefix(.ident, parseIdentifier) catch unreachable;
    parser.registerPrefix(.int, parseIntegerLiteral) catch unreachable;

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
            .@"return" => |s| {
                self.allocator.destroy(s);
            },
            .expr => |s| {
                switch (s.expr) {
                    .ident => |e| {
                        self.allocator.destroy(e);
                    },
                    .int => |e| {
                        self.allocator.destroy(e);
                    },
                }
                self.allocator.destroy(s);
            },
            .program => unreachable,
        }
    }
    self.allocator.free(program.statements);
    self.allocator.destroy(program);
    self.prefixParseFns.deinit();
    self.infixParseFns.deinit();
}

fn registerPrefix(self: *Self, token_type: Token.Type, parse_fn: PrefixParseFn) !void {
    try self.prefixParseFns.put(token_type, parse_fn);
}

fn registerInfix(self: *Self, token_type: Token.Type, parse_fn: InfixParseFn) !void {
    try self.infixParseFns.put(token_type, parse_fn);
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
                .@"return" => |s| {
                    self.allocator.destroy(s);
                },
                .program => unreachable,
                .expr => |s| {
                    self.allocator.destroy(s);
                },
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
        .@"return" => {
            return .{ .@"return" = try self.parseReturnStatement() orelse return null };
        },
        else => {
            return .{ .expr = try self.parseExpressionStatement() orelse return null };
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

fn parseReturnStatement(self: *Self) !?*ast.ReturnStatement {
    var stmt = try self.allocator.create(ast.ReturnStatement);
    errdefer self.allocator.destroy(stmt);

    stmt.token = self.cur_token;

    while (!self.curTokenIs(.semicolon)) {
        self.nextToken();
    }

    return stmt;
}

fn parseExpressionStatement(self: *Self) !?*ast.ExpressionStatement {
    const stmt = try self.allocator.create(ast.ExpressionStatement);
    errdefer self.allocator.destroy(stmt);

    stmt.* = .{
        .token = self.cur_token,
        .expr = try self.parseExpression(.lowest) orelse return null,
    };

    if (self.peekTokenIs(.semicolon)) {
        self.nextToken();
    }

    return stmt;
}

const Precedence = enum(u3) {
    lowest,
    equals, // ==
    lessgreater, // < or >
    sum, // +
    product, // *
    prefix, // -X or !X
    call, // myFunc(X)
};

fn parseExpression(self: *Self, prec: Precedence) !?ast.Expression {
    _ = prec; // autofix
    if (self.prefixParseFns.get(self.cur_token.token_type)) |prefix| {
        const left_expr = try prefix(self);
        return left_expr;
    }
    return null;
}

fn parseIdentifier(self: *Self) !ast.Expression {
    const ident = try self.allocator.create(ast.Identifier);
    ident.* = .{ .token = self.cur_token, .value = self.cur_token.literal };
    return .{ .ident = ident };
}

fn parseIntegerLiteral(self: *Self) !ast.Expression {
    const int_lit = try self.allocator.create(ast.IntegerLiteral);
    int_lit.* = .{ .token = self.cur_token, .value = try std.fmt.parseInt(i64, self.cur_token.literal, 10) };
    return .{ .int = int_lit };
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

test "let" {
    var lexer = Lexer.init(
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 8383;
    );

    var parser = init(std.testing.allocator, &lexer);

    const program = try (&parser).parseProgram();
    defer parser.deinit(program);

    try checkParserErrors(&parser);
    try std.testing.expectEqual(3, program.statements.len);

    const expectedIdents = [_][]const u8{ "x", "y", "foobar" };
    for (expectedIdents, 0..) |ident, i| {
        const stmt = program.statements[i];
        try testLetStatement(stmt.let, ident);
    }
}

test "return" {
    var lexer = Lexer.init(
        \\return 5;
        \\return 10;
    );

    var parser = init(std.testing.allocator, &lexer);

    const program = try (&parser).parseProgram();
    defer parser.deinit(program);

    try checkParserErrors(&parser);
    try std.testing.expectEqual(2, program.statements.len);
}

test "identifier expression" {
    const input = "foobar;";
    var lexer = Lexer.init(input);

    var parser = init(std.testing.allocator, &lexer);

    const program = try (&parser).parseProgram();
    defer parser.deinit(program);

    try checkParserErrors(&parser);
    try std.testing.expectEqual(1, program.statements.len);

    const stmt = program.statements[0];
    try std.testing.expect(stmt == .expr);
    try std.testing.expect(stmt.expr.expr == .ident);
    try std.testing.expectEqualStrings("foobar", stmt.expr.expr.ident.value);
    try std.testing.expectEqualStrings("foobar", stmt.expr.expr.ident.tokenLit());
}

test "integer literal expression" {
    const input = "5;";
    var lexer = Lexer.init(input);

    var parser = init(std.testing.allocator, &lexer);

    const program = try (&parser).parseProgram();
    defer parser.deinit(program);

    try checkParserErrors(&parser);
    try std.testing.expectEqual(1, program.statements.len);

    const stmt = program.statements[0];
    try std.testing.expect(stmt == .expr);
    try std.testing.expect(stmt.expr.expr == .int);
    try std.testing.expectEqual(5, stmt.expr.expr.int.value);
    try std.testing.expectEqualStrings("5", stmt.expr.expr.int.tokenLit());
}
