const std = @import("std");
const Allocator = std.mem.Allocator;
const Reader = std.fs.File.Reader;
const Writer = std.fs.File.Writer;
const ArrayList = std.ArrayList;

const Lexer = @import("lexter.zig").Lexer;
const Token = @import("token.zig").Token;

const PROMPT = ">> ";

pub fn start(in: Reader, out: Writer, allocator: Allocator) !void {
    var buf = ArrayList(u8).init(allocator);
    defer buf.deinit();
    while (true) {
        defer buf.clearRetainingCapacity();
        try out.writeAll(PROMPT);
        try streamUntilEof(in, buf.writer(), '\n');
        var lexer = try Lexer.init(buf.items);
        while (lexer.nextToken()) |tok| {
            try std.fmt.format(out, "{}\n", .{tok});
        }
    }
}

fn streamUntilEof(
    self: Reader,
    writer: anytype,
    delimiter: u8,
) !void {
    while (true) {
        const byte: u8 = self.readByte() catch |err| switch (err) {
            error.EndOfStream => return,
            else => return err,
        };
        if (byte == delimiter) return;
        try writer.writeByte(byte);
    }
}
