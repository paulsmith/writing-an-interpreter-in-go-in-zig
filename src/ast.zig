const std = @import("std");
const assert = std.debug.assert;
const Token = @import("Token.zig");

const Node = union(enum) {
    prog: Program,
    let_stmt: LetStatement,

    pub fn tokenLit(self: Node) []const u8 {
        switch (self) {
            inline else => |impl| return impl.tokenLit(),
        }
    }
};

const Program = struct {
    statements: []Node,

    fn node(self: Program) Node {
        return .{ .prog = self };
    }

    fn tokenLit(self: Program) []const u8 {
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

    fn node(self: LetStatement) Node {
        return .{ .let_stmt = self };
    }

    fn tokenLit(self: LetStatement) []const u8 {
        return self.token.literal;
    }
};

test {
    const Lexer = @import("Lexer.zig");
    var lexer = Lexer.init("let foo = 5;");
    const token = lexer.next();
    const letStmt = LetStatement{ .token = token };
    var stmts = [_]Node{letStmt.node()};
    const prog = Program{ .statements = &stmts };
    try std.testing.expectEqualStrings(prog.tokenLit(), "let");
}

// const Program = struct {
//     statements: []Node,
//
//     fn tokenLit(self: *Program) []const u8 {
//         if (self.statements.len > 0) {
//             return self.statements[0].tokenLit();
//         } else {
//             return "";
//         }
//     }
//
//     fn node(self: *Program) Node {
//         return Node.init(self, tokenLit);
//     }
// };
//
// const Node = struct {
//     const Self = @This();
//
//     ptr: *anyopaque,
//     tokenLitFn: *const fn (*anyopaque) []const u8,
//
//     pub fn init(pointer: anytype, comptime tokenLitFn: fn (ptr: @TypeOf(pointer)) []const u8) Self {
//         const Ptr = @TypeOf(pointer);
//         assert(@typeInfo(Ptr) == .Pointer);
//         assert(@typeInfo(Ptr).Pointer.size == .One);
//         assert(@typeInfo(@typeInfo(Ptr).Pointer.child) == .Struct);
//
//         const gen = struct {
//             pub fn tokenLit(ptr: *anyopaque) []const u8 {
//                 const self: Ptr = @ptrCast(@alignCast(ptr));
//                 tokenLitFn(self);
//             }
//         };
//
//         return .{
//             .ptr = pointer,
//             .tokenLitFn = gen.tokenLit,
//         };
//     }
// };
