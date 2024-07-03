const std = @import("std");
const assert = std.debug.assert;
const ascii = std.ascii;
const Token = @import("Token.zig");
const lookupIdent = Token.lookupIdent;

input: []const u8,
position: usize, // current position in input (current char)
readPosition: usize, // current reading position in input (after current char)
ch: u8,

const Self = @This();

pub fn init(input: []const u8) Self {
    var lexer = Self{
        .input = input,
        .position = 0,
        .readPosition = 0,
        .ch = 0,
    };
    lexer.readChar();
    return lexer;
}

fn newToken(token_type: Token.Type, literal: []const u8) Token {
    return Token{ .token_type = token_type, .literal = literal };
}

pub fn next(self: *Self) Token {
    self.skipWhitespace();
    const token = switch (self.ch) {
        '=' => newToken(.assign, "="),
        ';' => newToken(.semicolon, ";"),
        '(' => newToken(.lparen, "("),
        ')' => newToken(.rparen, ")"),
        '{' => newToken(.lbrace, "{"),
        '}' => newToken(.rbrace, "}"),
        ',' => newToken(.comma, ","),
        '+' => newToken(.plus, "+"),
        0 => newToken(.eof, ""),
        else => brk: {
            if (ascii.isAlphabetic(self.ch) or self.ch == '_') {
                var token: Token = undefined;
                token.literal = self.readIdentifier();
                token.token_type = lookupIdent(token.literal);
                return token;
            } else if (ascii.isDigit(self.ch)) {
                var token: Token = undefined;
                token.literal = self.readNumber();
                token.token_type = .int;
                return token;
            }
            break :brk newToken(.illegal, "");
        },
    };
    self.readChar();
    return token;
}

fn skipWhitespace(self: *Self) void {
    while (ascii.isWhitespace(self.ch)) {
        self.readChar();
    }
}

fn readIdentifier(self: *Self) []const u8 {
    const p = self.position;
    while (ascii.isAlphabetic(self.ch) or self.ch == '_') {
        self.readChar();
    }
    return self.input[p..self.position];
}

fn readNumber(self: *Self) []const u8 {
    const p = self.position;
    while (ascii.isDigit(self.ch)) {
        self.readChar();
    }
    return self.input[p..self.position];
}

fn readChar(self: *Self) void {
    if (self.readPosition >= self.input.len) {
        self.ch = 0;
    } else {
        self.ch = self.input[self.readPosition];
    }
    self.position = self.readPosition;
    self.readPosition += 1;
}

const t = std.testing;

test "basic lexer" {
    const input = "=+(){},;";
    const tokens = [_]Token{
        Token{ .token_type = .assign, .literal = "=" },
        Token{ .token_type = .plus, .literal = "+" },
        Token{ .token_type = .lparen, .literal = "(" },
        Token{ .token_type = .rparen, .literal = ")" },
        Token{ .token_type = .lbrace, .literal = "{" },
        Token{ .token_type = .rbrace, .literal = "}" },
        Token{ .token_type = .comma, .literal = "," },
        Token{ .token_type = .semicolon, .literal = ";" },
        Token{ .token_type = .eof, .literal = "" },
    };
    var lexer = init(input);
    for (tokens) |want| {
        const got = (&lexer).next();
        try t.expectEqualDeep(want, got);
    }
}

test "readChar" {
    const input = "Hello, world";
    var lexer = init(input);
    try t.expectEqual(lexer.ch, 'H');
    lexer.readChar();
    try t.expectEqual(lexer.ch, 'e');
    lexer.readChar();
    try t.expectEqual(lexer.ch, 'l');
    lexer.readChar();
    try t.expectEqual(lexer.ch, 'l');
    lexer.readChar();
    try t.expectEqual(lexer.ch, 'o');
    lexer.readChar();
    try t.expectEqual(lexer.ch, ',');
    lexer.readChar();
    try t.expectEqual(lexer.ch, ' ');
    lexer.readChar();
    try t.expectEqual(lexer.ch, 'w');
    lexer.readChar();
    try t.expectEqual(lexer.ch, 'o');
    lexer.readChar();
    try t.expectEqual(lexer.ch, 'r');
    lexer.readChar();
    try t.expectEqual(lexer.ch, 'l');
    lexer.readChar();
    try t.expectEqual(lexer.ch, 'd');
    lexer.readChar();
    try t.expectEqual(lexer.ch, 0);
    lexer.readChar();
    try t.expectEqual(lexer.ch, 0);
}

test "lex Monkey source code" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\
        \\let result = add(five, ten);
    ;
    const tokens = [_]Token{
        Token{ .token_type = .let, .literal = "let" },
        Token{ .token_type = .ident, .literal = "five" },
        Token{ .token_type = .assign, .literal = "=" },
        Token{ .token_type = .int, .literal = "5" },
        Token{ .token_type = .semicolon, .literal = ";" },
        Token{ .token_type = .let, .literal = "let" },
        Token{ .token_type = .ident, .literal = "ten" },
        Token{ .token_type = .assign, .literal = "=" },
        Token{ .token_type = .int, .literal = "10" },
        Token{ .token_type = .semicolon, .literal = ";" },
        Token{ .token_type = .let, .literal = "let" },
        Token{ .token_type = .ident, .literal = "add" },
        Token{ .token_type = .assign, .literal = "=" },
        Token{ .token_type = .function, .literal = "fn" },
        Token{ .token_type = .lparen, .literal = "(" },
        Token{ .token_type = .ident, .literal = "x" },
        Token{ .token_type = .comma, .literal = "," },
        Token{ .token_type = .ident, .literal = "y" },
        Token{ .token_type = .rparen, .literal = ")" },
        Token{ .token_type = .lbrace, .literal = "{" },
        Token{ .token_type = .ident, .literal = "x" },
        Token{ .token_type = .plus, .literal = "+" },
        Token{ .token_type = .ident, .literal = "y" },
        Token{ .token_type = .semicolon, .literal = ";" },
        Token{ .token_type = .rbrace, .literal = "}" },
        Token{ .token_type = .semicolon, .literal = ";" },
        Token{ .token_type = .let, .literal = "let" },
        Token{ .token_type = .ident, .literal = "result" },
        Token{ .token_type = .assign, .literal = "=" },
        Token{ .token_type = .ident, .literal = "add" },
        Token{ .token_type = .lparen, .literal = "(" },
        Token{ .token_type = .ident, .literal = "five" },
        Token{ .token_type = .comma, .literal = "," },
        Token{ .token_type = .ident, .literal = "ten" },
        Token{ .token_type = .rparen, .literal = ")" },
        Token{ .token_type = .semicolon, .literal = ";" },
        Token{ .token_type = .eof, .literal = "" },
    };
    var lexer = init(input);
    for (tokens) |want| {
        const got = (&lexer).next();
        try t.expectEqualDeep(want, got);
    }
}
