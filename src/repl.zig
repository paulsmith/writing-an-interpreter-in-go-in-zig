const std = @import("std");
const Lexer = @import("Lexer.zig");

const PROMPT = ">> ";

var line_buf: [4096]u8 = undefined;

pub fn start(reader: anytype, writer: anytype) void {
    while (true) {
        writer.print("{s}", .{PROMPT}) catch unreachable;
        const line = (reader.readUntilDelimiterOrEof(&line_buf, '\n') catch unreachable) orelse break;
        var lexer = Lexer.init(line);
        while (true) {
            const token = lexer.next();
            if (token.token_type == .eof) {
                break;
            }
            writer.print("{any}\n", .{token}) catch unreachable;
        }
    }
}

test {}
