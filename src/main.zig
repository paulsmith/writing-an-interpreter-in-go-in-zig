const std = @import("std");
const repl = @import("repl.zig");
const c = @cImport({
    @cInclude("user.h");
});

pub fn main() !void {
    const in = std.io.getStdIn();
    const out = std.io.getStdOut();
    const username = c.getusername();
    try out.writer().print("Hello, {s}! This is the Monkey programming language!\n", .{username});
    try out.writer().print("Feel free to type in commands\n", .{});
    repl.start(in.reader(), out.writer());
}

test {
    _ = @import("test.zig");
}
