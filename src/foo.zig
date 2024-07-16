const std = @import("std");
const assert = std.debug.assert;

const Node = struct {
    ptr: *anyopaque,
    fooFn: *const fn (*anyopaque) void,

    fn init(pointer: anytype) Node {
        const Ptr = @TypeOf(pointer);
        assert(@typeInfo(Ptr) == .Pointer);
        assert(@typeInfo(Ptr).Pointer.size == .One);
        assert(@typeInfo(@typeInfo(Ptr).Pointer.child) == .Struct);

        const gen = struct {
            fn foo(ptr: *anyopaque) void {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                @call(.always_inline, @typeInfo(Ptr).Pointer.child.foo, .{self});
            }
        };

        return .{
            .ptr = pointer,
            .fooFn = gen.foo,
        };
    }

    inline fn foo(node: Node) void {
        node.fooFn(node.ptr);
    }
};

const A = struct {
    fn foo(a: *A) void {
        _ = a; // autofix
        std.debug.print("A\n", .{});
    }
};

const B = struct {
    fn foo(b: *B) void {
        _ = b; // autofix
        std.debug.print("B\n", .{});
    }
};

test {
    var a: A = undefined;
    var b: B = undefined;
    const nodes = [_]Node{
        Node.init(&a),
        Node.init(&b),
    };
    for (nodes) |node| {
        //std.debug.print("node: {}\n", .{node});
        node.foo();
    }
}
