const std = @import("std");

pub const Type = enum {
    illegal,
    eof,
    ident,
    int,

    // operators
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    lt,
    gt,
    eq,
    not_eq,

    comma,
    semicolon,
    lparen,
    rparen,
    lbrace,
    rbrace,

    // keywords
    function,
    let,
    @"if",
    @"else",
    @"return",
    true,
    false,

    fn name(self: Type) []const u8 {
        return switch (self) {
            .illegal => "ILLEGAL",
            .eof => "EOF",
            .ident => "IDENT",
            .int => "INT",
            .assign => "=",
            .plus => "+",
            .minus => "-",
            .bang => "!",
            .asterisk => "*",
            .slash => "/",
            .lt => "<",
            .gt => ">",
            .eq => "==",
            .not_eq => "!=",
            .comma => ",",
            .semicolon => ";",
            .lparen => "(",
            .rparen => ")",
            .lbrace => "{",
            .rbrace => "}",
            .function => "FUNCTION",
            .let => "LET",
            .@"if" => "IF",
            .@"else" => "ELSE",
            .@"return" => "RETURN",
            .true => "TRUE",
            .false => "FALSE",
        };
    }
};

token_type: Type,
literal: []const u8,

const keywords = std.StaticStringMap(Type).initComptime([_]struct { []const u8, Type }{
    .{ "let", .let },
    .{ "fn", .function },
    .{ "if", .@"if" },
    .{ "else", .@"else" },
    .{ "return", .@"return" },
    .{ "true", .true },
    .{ "false", .false },
});

pub fn lookupIdent(ident: []const u8) Type {
    if (keywords.get(ident)) |kw| {
        return kw;
    }
    return .ident;
}
