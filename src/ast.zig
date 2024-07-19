const std = @import("std");
const assert = std.debug.assert;
const Token = @import("Token.zig");

pub const Expression = union(enum) {};

pub const Statement = union(enum) {
    program: *Program,
    let: *LetStatement,
    @"return": *ReturnStatement,

    pub fn node(self: Statement) Node {
        return switch (self) {
            .program => |s| s.node(),
            .let => |s| s.node(),
            .@"return" => |s| s.node(),
        };
    }
};

pub const Node = struct {
    const Self = @This();

    ptr: *anyopaque,
    tokenLitFn: *const fn (*anyopaque) []const u8,

    pub fn init(pointer: anytype) Self {
        const Ptr = @TypeOf(pointer);
        assert(@typeInfo(Ptr) == .Pointer);
        assert(@typeInfo(Ptr).Pointer.size == .One);
        assert(@typeInfo(@typeInfo(Ptr).Pointer.child) == .Struct);

        const gen = struct {
            pub fn tokenLit(ptr: *anyopaque) []const u8 {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return @call(.always_inline, @typeInfo(Ptr).Pointer.child.tokenLit, .{self});
            }
        };

        return .{
            .ptr = pointer,
            .tokenLitFn = gen.tokenLit,
        };
    }

    pub inline fn tokenLit(self: Self) []const u8 {
        return self.tokenLitFn(self.ptr);
    }
};

pub const Program = struct {
    statements: []Statement,

    fn node(self: *Program) Node {
        return Node.init(self);
    }

    pub fn tokenLit(self: *Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].node().tokenLit();
        }
        return "";
    }
};

pub const LetStatement = struct {
    token: Token,
    name: *Identifier,
    value: Expression,

    fn node(self: *LetStatement) Node {
        return Node.init(self);
    }

    pub fn tokenLit(self: *LetStatement) []const u8 {
        return self.token.literal;
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    fn node(self: Identifier) Node {
        return Node.init(self);
    }

    pub fn tokenLit(self: *Identifier) []const u8 {
        return self.token.literal;
    }
};

pub const ReturnStatement = struct {
    token: Token,
    value: Expression,

    fn node(self: *ReturnStatement) Node {
        return Node.init(self);
    }

    pub fn tokenLit(self: *ReturnStatement) []const u8 {
        return self.token.literal;
    }
};

test {
    const Lexer = @import("Lexer.zig");
    var lexer = Lexer.init("let foo = 5;");
    const tokLet = lexer.next();
    const tokIdent = lexer.next();
    var ident = Identifier{ .token = tokIdent, .value = tokIdent.literal };
    var letStmt = LetStatement{ .token = tokLet, .name = &ident, .value = undefined };
    var stmts = [_]Statement{.{ .let = &letStmt }};
    var prog = Program{ .statements = &stmts };
    try std.testing.expectEqualStrings((&prog).tokenLit(), "let");
}
