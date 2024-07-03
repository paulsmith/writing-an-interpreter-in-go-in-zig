const std = @import("std");

pub const Type = enum {
    illegal,
    eof,
    ident,
    int,
    assign,
    plus,
    comma,
    semicolon,
    lparen,
    rparen,
    lbrace,
    rbrace,
    function,
    let,

    fn name(self: Type) []const u8 {
        return switch (self) {
            .illegal => "ILLEGAL",
            .eof => "EOF",
            .ident => "IDENT",
            .int => "INT",
            .assign => "=",
            .plus => "+",
            .comma => ",",
            .semicolon => ";",
            .lparen => "(",
            .rparen => ")",
            .lbrace => "{",
            .rbrace => "}",
            .function => "FUNCTION",
            .let => "LET",
        };
    }
};

token_type: Type,
literal: []const u8,

const keywords = std.StaticStringMap(Type).initComptime([_]struct { []const u8, Type }{
    .{ "let", .let },
    .{ "fn", .function },
});

pub fn lookupIdent(ident: []const u8) Type {
    if (keywords.get(ident)) |kw| {
        return kw;
    }
    return .ident;
}
