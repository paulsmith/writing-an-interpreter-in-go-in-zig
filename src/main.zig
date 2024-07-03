const std = @import("std");

pub fn main() !void {
    std.debug.print("Eventually: run the interpreter ...", .{});
}

test {
    _ = @import("Token.zig");
    _ = @import("Lexer.zig");
}
