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

const PrecedenceTable = std.AutoHashMap(Token.Type, Precedence);

allocator: Allocator,
lexer: *Lexer,
cur_token: Token = undefined,
peek_token: Token = undefined,
errors: std.ArrayList([]const u8),
prefixParseFns: PrefixParseFnMap,
infixParseFns: InfixParseFnMap,
precedenceTable: PrecedenceTable,

const Self = @This();

pub fn init(allocator: Allocator, lexer: *Lexer) !Self {
    var parser = Self{
        .allocator = allocator,
        .lexer = lexer,
        .errors = std.ArrayList([]const u8).init(allocator),
        .prefixParseFns = PrefixParseFnMap.init(allocator),
        .infixParseFns = InfixParseFnMap.init(allocator),
        .precedenceTable = PrecedenceTable.init(allocator),
    };

    try parser.registerPrefix(.ident, parseIdentifier);
    try parser.registerPrefix(.int, parseIntegerLiteral);
    try parser.registerPrefix(.bang, parsePrefixExpression);
    try parser.registerPrefix(.minus, parsePrefixExpression);
    try parser.registerInfix(.plus, parseInfixExpression);
    try parser.registerInfix(.minus, parseInfixExpression);
    try parser.registerInfix(.slash, parseInfixExpression);
    try parser.registerInfix(.asterisk, parseInfixExpression);
    try parser.registerInfix(.eq, parseInfixExpression);
    try parser.registerInfix(.not_eq, parseInfixExpression);
    try parser.registerInfix(.lt, parseInfixExpression);
    try parser.registerInfix(.gt, parseInfixExpression);

    try parser.precedenceTable.put(.eq, .equals);
    try parser.precedenceTable.put(.not_eq, .equals);
    try parser.precedenceTable.put(.lt, .lessgreater);
    try parser.precedenceTable.put(.gt, .lessgreater);
    try parser.precedenceTable.put(.plus, .sum);
    try parser.precedenceTable.put(.minus, .sum);
    try parser.precedenceTable.put(.asterisk, .product);
    try parser.precedenceTable.put(.slash, .product);

    (&parser).nextToken();
    (&parser).nextToken();

    return parser;
}

fn destroyExpr(allocator: Allocator, expr: ast.Expression) void {
    switch (expr) {
        .ident => |e| {
            allocator.destroy(e);
        },
        .int => |e| {
            allocator.destroy(e);
        },
        .prefix => |e| {
            destroyExpr(allocator, e.right);
            allocator.destroy(e);
        },
        .infix => |e| {
            destroyExpr(allocator, e.left);
            destroyExpr(allocator, e.right);
            allocator.destroy(e);
        },
    }
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
                destroyExpr(self.allocator, s.expr);
                self.allocator.destroy(s);
            },
            .program => unreachable,
        }
    }
    self.allocator.free(program.statements);
    self.allocator.destroy(program);
    for (self.errors.items) |error_str| {
        self.allocator.free(error_str);
    }
    self.errors.deinit();
    self.precedenceTable.deinit();
    self.prefixParseFns.deinit();
    self.infixParseFns.deinit();
}

pub fn nextToken(self: *Self) void {
    self.cur_token = self.peek_token;
    self.peek_token = self.lexer.next();
}

fn registerPrefix(self: *Self, token_type: Token.Type, parse_fn: PrefixParseFn) !void {
    try self.prefixParseFns.put(token_type, parse_fn);
}

fn registerInfix(self: *Self, token_type: Token.Type, parse_fn: InfixParseFn) !void {
    try self.infixParseFns.put(token_type, parse_fn);
}

fn fmtError(self: *Self, comptime fmt: []const u8, args: anytype) void {
    const error_str = std.fmt.allocPrint(self.allocator, fmt, args) catch unreachable;
    self.errors.append(error_str) catch unreachable;
}

pub fn parseProgram(self: *Self) !*ast.Program {
    var program = try self.allocator.create(ast.Program);
    errdefer {
        for (self.errors.items) |error_str| {
            self.allocator.free(error_str);
        }
        self.errors.deinit();
        self.precedenceTable.deinit();
        self.prefixParseFns.deinit();
        self.infixParseFns.deinit();
        self.allocator.destroy(program);
    }

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
                    destroyExpr(self.allocator, s.expr);
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

    const expr = try self.parseExpression(.lowest);
    stmt.* = .{
        .token = self.cur_token,
        .expr = expr,
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

    fn islower(self: @This(), other: @This()) bool {
        return @intFromEnum(self) < @intFromEnum(other);
    }
};

fn peekPrecedence(self: *Self) Precedence {
    if (self.precedenceTable.get(self.peek_token.token_type)) |prec| {
        return prec;
    }
    return .lowest;
}

fn curPrecedence(self: *Self) Precedence {
    if (self.precedenceTable.get(self.cur_token.token_type)) |prec| {
        return prec;
    }
    return .lowest;
}

fn parseExpression(self: *Self, prec: Precedence) !ast.Expression {
    const prefixFn = self.prefixParseFns.get(self.cur_token.token_type) orelse {
        self.fmtError("no prefix parse function for token type {s}", .{self.cur_token.token_type.name()});
        return error.PrefixParseFnNotFound;
    };

    var left_expr = try prefixFn(self);

    while (!self.peekTokenIs(.semicolon) and prec.islower(self.peekPrecedence())) {
        const infixFn = self.infixParseFns.get(self.peek_token.token_type) orelse {
            return left_expr;
        };
        self.nextToken();
        left_expr = try infixFn(self, left_expr);
    }

    return left_expr;
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

fn parsePrefixExpression(self: *Self) !ast.Expression {
    const expr = try self.allocator.create(ast.PrefixExpression);
    errdefer self.allocator.destroy(expr);
    expr.* = .{
        .token = self.cur_token,
        .op = try ast.Operator.fromString(self.cur_token.literal),
        .right = undefined,
    };
    self.nextToken();
    expr.right = try self.parseExpression(.prefix);
    return .{ .prefix = expr };
}

fn parseInfixExpression(self: *Self, left: ast.Expression) !ast.Expression {
    const expr = try self.allocator.create(ast.InfixExpression);
    errdefer self.allocator.destroy(expr);
    expr.* = .{
        .token = self.cur_token,
        .op = try ast.Operator.fromString(self.cur_token.literal),
        .left = left,
        .right = undefined,
    };
    const prec = self.curPrecedence();
    self.nextToken();
    expr.right = try self.parseExpression(prec);
    return .{ .infix = expr };
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
    return self.errors.items.len > 0;
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
        for (parser.errors.items) |error_str| {
            std.debug.print("\x1b[41merror: {s}\x1b[0m\n", .{error_str});
        }
        try std.testing.expect(false);
    }
}

test "let" {
    var lexer = Lexer.init(
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 8383;
    );

    var parser = try init(std.testing.allocator, &lexer);

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

    var parser = try init(std.testing.allocator, &lexer);

    const program = try (&parser).parseProgram();
    defer parser.deinit(program);

    try checkParserErrors(&parser);
    try std.testing.expectEqual(2, program.statements.len);
}

test "identifier expression" {
    const input = "foobar;";
    var lexer = Lexer.init(input);

    var parser = try init(std.testing.allocator, &lexer);

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

    var parser = try init(std.testing.allocator, &lexer);

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

test "prefix expression" {
    const test_cases = [_]struct { input: []const u8, op: ast.Operator, value: i64 }{
        .{ .input = "!5;", .op = .bang, .value = 5 },
        .{ .input = "-15;", .op = .minus, .value = 15 },
    };

    for (test_cases) |case| {
        var lexer = Lexer.init(case.input);
        var parser = try init(std.testing.allocator, &lexer);

        const program = try (&parser).parseProgram();
        defer parser.deinit(program);

        try checkParserErrors(&parser);
        try std.testing.expectEqual(1, program.statements.len);

        const stmt = program.statements[0];
        try std.testing.expect(stmt == .expr);
        try std.testing.expect(stmt.expr.expr == .prefix);
        try std.testing.expectEqual(case.op, stmt.expr.expr.prefix.op);
        try std.testing.expect(stmt.expr.expr.prefix.right == .int);
        try std.testing.expectEqual(case.value, stmt.expr.expr.prefix.right.int.value);
    }
}

test "infix operators" {
    const test_cases = [_]struct { input: []const u8, lval: i64, op: ast.Operator, rval: i64 }{
        .{ .input = "5 + 5;", .lval = 5, .op = .plus, .rval = 5 },
        .{ .input = "5 - 5;", .lval = 5, .op = .minus, .rval = 5 },
        .{ .input = "5 * 5;", .lval = 5, .op = .mul, .rval = 5 },
        .{ .input = "5 / 5;", .lval = 5, .op = .div, .rval = 5 },
        .{ .input = "5 < 5;", .lval = 5, .op = .lt, .rval = 5 },
        .{ .input = "5 > 5;", .lval = 5, .op = .gt, .rval = 5 },
        .{ .input = "5 == 5;", .lval = 5, .op = .eq, .rval = 5 },
        .{ .input = "5 != 5;", .lval = 5, .op = .ne, .rval = 5 },
    };

    for (test_cases) |case| {
        var lexer = Lexer.init(case.input);
        var parser = try init(std.testing.allocator, &lexer);

        const program = try (&parser).parseProgram();
        defer parser.deinit(program);

        try checkParserErrors(&parser);
        try std.testing.expectEqual(1, program.statements.len);

        const stmt = program.statements[0];
        try std.testing.expect(stmt == .expr);
        try std.testing.expect(stmt.expr.expr == .infix);
        try std.testing.expectEqual(case.op, stmt.expr.expr.infix.op);
        try std.testing.expect(stmt.expr.expr.infix.left == .int);
        try std.testing.expect(stmt.expr.expr.infix.right == .int);
        try std.testing.expectEqual(case.lval, stmt.expr.expr.infix.left.int.value);
        try std.testing.expectEqual(case.rval, stmt.expr.expr.infix.right.int.value);
    }
}

test "operator precedence" {
    const test_cases = [_]struct { input: []const u8, want: []const u8 }{
        .{ .input = "-a * b", .want = "((-a) * b)" },
        .{ .input = "!-a", .want = "(!(-a))" },
        .{ .input = "a + b + c", .want = "((a + b) + c)" },
        .{ .input = "a * b * c", .want = "((a * b) * c)" },
        .{ .input = "a * b / c", .want = "((a * b) / c)" },
        .{ .input = "a + b / c", .want = "(a + (b / c))" },
        .{ .input = "a + b * c + d / e - f", .want = "(((a + (b * c)) + (d / e)) - f)" },
        .{ .input = "3 + 4; -5 * 5", .want = "(3 + 4)((-5) * 5)" },
        .{ .input = "5 > 4 == 3 < 4", .want = "((5 > 4) == (3 < 4))" },
        .{ .input = "5 < 4 != 3 > 4", .want = "((5 < 4) != (3 > 4))" },
        .{ .input = "3 + 4 * 5 == 3 * 1 + 4 * 5", .want = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
    };

    for (test_cases) |case| {
        var lexer = Lexer.init(case.input);
        var parser = try init(std.testing.allocator, &lexer);

        const program = try (&parser).parseProgram();
        defer parser.deinit(program);

        try checkParserErrors(&parser);

        const got = try program.toString(std.testing.allocator);
        defer std.testing.allocator.free(got);
        try std.testing.expectEqualStrings(case.want, got);
    }
}
