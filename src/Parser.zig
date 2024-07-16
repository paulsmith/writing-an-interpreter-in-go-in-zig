const std = @import("std");
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const ast = @import("ast.zig");

lexer: *Lexer,
curToken: Token = undefined,
peekToken: Token = undefined,

const Self = @This();

pub fn init(lexer: *Lexer) Self {
    var parser = Self{ .lexer = lexer };
    (&parser).nextToken();
    (&parser).nextToken();
    return parser;
}

pub fn nextToken(self: *Self) void {
    self.curToken = self.peekToken;
    self.peekToken = self.lexer.next();
}

pub fn parseProgram(self: *Self) *ast.Program {
    _ = self; // autofix
}

test {
    var lexer = Lexer.init("let x = 5;");
    const parser = init(&lexer);
    try std.testing.expectEqualStrings(parser.curToken.literal, "let");
    try std.testing.expectEqualStrings(parser.peekToken.literal, "x");
}
