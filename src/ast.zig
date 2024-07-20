const std = @import("std");
const assert = std.debug.assert;
const Token = @import("Token.zig");
const Allocator = std.mem.Allocator;
const fmt = std.fmt;

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
    toStringFn: *const fn (*anyopaque, Allocator) anyerror![]const u8,

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

            pub fn toString(ptr: *anyopaque, allocator: Allocator) ![]const u8 {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return @call(.always_inline, @typeInfo(Ptr).Pointer.child.toString, .{ self, allocator });
            }
        };

        return .{
            .ptr = pointer,
            .tokenLitFn = gen.tokenLit,
            .toStringFn = gen.toString,
        };
    }

    pub inline fn tokenLit(self: Self) []const u8 {
        return self.tokenLitFn(self.ptr);
    }

    pub inline fn toString(self: Self, allocator: Allocator) ![]const u8 {
        return try self.toStringFn(self.ptr, allocator);
    }
};

pub const Program = struct {
    statements: []Statement,

    fn node(self: *Program) Node {
        return Node.init(self);
    }

    fn toString(self: *Program, allocator: Allocator) ![]const u8 {
        var str_buf = std.ArrayList(u8).init(allocator);
        for (self.statements) |stmt| {
            const str = try stmt.node().toString(allocator);
            try str_buf.appendSlice(str);
            allocator.free(str);
        }
        return try str_buf.toOwnedSlice();
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

    fn toString(self: *LetStatement, allocator: Allocator) ![]const u8 {
        return try fmt.allocPrint(allocator, "{s} {s} = {s};", .{ self.tokenLit(), self.name.tokenLit(), "TKTK" });
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

    fn toString(self: *Identifier, allocator: Allocator) ![]const u8 {
        return try fmt.allocPrint(allocator, "{s}", .{self.tokenLit()});
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

    fn toString(self: *ReturnStatement, allocator: Allocator) ![]const u8 {
        _ = self; // autofix
        return try fmt.allocPrint(allocator, "return {s};", .{"TKTK"});
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
    try std.testing.expectEqualStrings("let", (&prog).tokenLit());
}

test "toString" {
    var name: Identifier = .{
        .token = .{ .token_type = .ident, .literal = "foo" },
        .value = "foo",
    };
    var let: LetStatement = .{
        .token = .{ .token_type = .let, .literal = "let" },
        .name = &name,
        .value = undefined,
    };
    var statements = [_]Statement{.{ .let = &let }};
    var prog: Program = .{
        .statements = &statements,
    };
    const str = try (&prog).toString(std.testing.allocator);
    defer std.testing.allocator.free(str);
    try std.testing.expectEqualStrings(
        "let foo = TKTK;",
        str,
    );
}
