const std = @import("std");
const assert = std.debug.assert;
const Token = @import("Token.zig");

const Node = struct {
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

const Program = struct {
    statements: []Node,

    fn node(self: *Program) Node {
        return Node.init(self);
    }

    fn tokenLit(self: *Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLit();
        }
        return "";
    }
};

const LetStatement = struct {
    token: Token,
    //name: *Identifier,
    //value: Expression,

    fn node(self: *LetStatement) Node {
        return Node.init(self);
    }

    fn tokenLit(self: *LetStatement) []const u8 {
        return self.token.literal;
    }
};

test {
    const Lexer = @import("Lexer.zig");
    var lexer = Lexer.init("let foo = 5;");
    const token = lexer.next();
    var letStmt = LetStatement{ .token = token };
    var stmts = [_]Node{(&letStmt).node()};
    var prog = Program{ .statements = &stmts };
    try std.testing.expectEqualStrings((&prog).tokenLit(), "let");
}
