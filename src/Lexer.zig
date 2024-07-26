const std = @import("std");
const assert = std.debug.assert;
const ascii = std.ascii;
const Token = @import("Token.zig");
const lookupIdent = Token.lookupIdent;

input: []const u8,
position: usize, // current position in input (current char)
readPosition: usize, // current reading position in input (after current char)
ch: ?u8,

const Self = @This();

pub fn init(input: []const u8) Self {
    var lexer = Self{
        .input = input,
        .position = 0,
        .readPosition = 0,
        .ch = null,
    };
    lexer.readChar();
    return lexer;
}

fn newToken(token_type: Token.Type, literal: []const u8) Token {
    return Token{ .token_type = token_type, .literal = literal };
}

pub fn next(self: *Self) Token {
    self.skipWhitespace();
    if (self.ch) |ch| {
        const token = switch (ch) {
            '=' => blk: {
                if (self.peekChar() == '=') {
                    self.readChar();
                    break :blk Token{ .token_type = .eq, .literal = "==" };
                }
                break :blk newToken(.assign, "=");
            },
            '+' => newToken(.plus, "+"),
            '-' => newToken(.minus, "-"),
            '!' => blk: {
                if (self.peekChar() == '=') {
                    self.readChar();
                    break :blk Token{ .token_type = .not_eq, .literal = "!=" };
                }
                break :blk newToken(.bang, "!");
            },
            '/' => newToken(.slash, "/"),
            '*' => newToken(.asterisk, "*"),
            '<' => newToken(.lt, "<"),
            '>' => newToken(.gt, ">"),
            ';' => newToken(.semicolon, ";"),
            '(' => newToken(.lparen, "("),
            ')' => newToken(.rparen, ")"),
            '{' => newToken(.lbrace, "{"),
            '}' => newToken(.rbrace, "}"),
            ',' => newToken(.comma, ","),
            else => blk: {
                if (ascii.isAlphabetic(ch) or ch == '_') {
                    var token: Token = undefined;
                    token.literal = self.readIdentifier();
                    token.token_type = lookupIdent(token.literal);
                    return token;
                } else if (ascii.isDigit(ch)) {
                    var token: Token = undefined;
                    token.literal = self.readNumber();
                    token.token_type = .int;
                    return token;
                }
                break :blk newToken(.illegal, "");
            },
        };
        self.readChar();
        return token;
    }
    return newToken(.eof, "");
}

fn skipWhitespace(self: *Self) void {
    while (self.ch != null and ascii.isWhitespace(self.ch.?)) {
        self.readChar();
    }
}

fn readIdentifier(self: *Self) []const u8 {
    const p = self.position;
    while (self.ch != null and (ascii.isAlphabetic(self.ch.?) or self.ch.? == '_')) {
        self.readChar();
    }
    return self.input[p..self.position];
}

fn readNumber(self: *Self) []const u8 {
    const p = self.position;
    while (self.ch != null and ascii.isDigit(self.ch.?)) {
        self.readChar();
    }
    return self.input[p..self.position];
}

fn readChar(self: *Self) void {
    if (self.readPosition >= self.input.len) {
        self.ch = null;
    } else {
        self.ch = self.input[self.readPosition];
    }
    self.position = self.readPosition;
    self.readPosition += 1;
}

fn peekChar(self: *Self) ?u8 {
    if (self.readPosition >= self.input.len) {
        return null;
    }
    return self.input[self.readPosition];
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
    try t.expectEqual(lexer.ch, null);
    lexer.readChar();
    try t.expectEqual(lexer.ch, null);
}

test "next token" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\    return true;
        \\} else {
        \\    return false;
        \\}
        \\
        \\ 10 == 10;
        \\ 10 != 9;
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
        Token{ .token_type = .bang, .literal = "!" },
        Token{ .token_type = .minus, .literal = "-" },
        Token{ .token_type = .slash, .literal = "/" },
        Token{ .token_type = .asterisk, .literal = "*" },
        Token{ .token_type = .int, .literal = "5" },
        Token{ .token_type = .semicolon, .literal = ";" },
        Token{ .token_type = .int, .literal = "5" },
        Token{ .token_type = .lt, .literal = "<" },
        Token{ .token_type = .int, .literal = "10" },
        Token{ .token_type = .gt, .literal = ">" },
        Token{ .token_type = .int, .literal = "5" },
        Token{ .token_type = .semicolon, .literal = ";" },
        Token{ .token_type = .@"if", .literal = "if" },
        Token{ .token_type = .lparen, .literal = "(" },
        Token{ .token_type = .int, .literal = "5" },
        Token{ .token_type = .lt, .literal = "<" },
        Token{ .token_type = .int, .literal = "10" },
        Token{ .token_type = .rparen, .literal = ")" },
        Token{ .token_type = .lbrace, .literal = "{" },
        Token{ .token_type = .@"return", .literal = "return" },
        Token{ .token_type = .true, .literal = "true" },
        Token{ .token_type = .semicolon, .literal = ";" },
        Token{ .token_type = .rbrace, .literal = "}" },
        Token{ .token_type = .@"else", .literal = "else" },
        Token{ .token_type = .lbrace, .literal = "{" },
        Token{ .token_type = .@"return", .literal = "return" },
        Token{ .token_type = .false, .literal = "false" },
        Token{ .token_type = .semicolon, .literal = ";" },
        Token{ .token_type = .rbrace, .literal = "}" },
        Token{ .token_type = .int, .literal = "10" },
        Token{ .token_type = .eq, .literal = "==" },
        Token{ .token_type = .int, .literal = "10" },
        Token{ .token_type = .semicolon, .literal = ";" },
        Token{ .token_type = .int, .literal = "10" },
        Token{ .token_type = .not_eq, .literal = "!=" },
        Token{ .token_type = .int, .literal = "9" },
        Token{ .token_type = .semicolon, .literal = ";" },
        Token{ .token_type = .eof, .literal = "" },
    };
    var lexer = init(input);
    for (tokens) |want| {
        const got = (&lexer).next();
        try t.expectEqualDeep(want, got);
    }
}
