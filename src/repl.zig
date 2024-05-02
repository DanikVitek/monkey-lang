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
    while (true) {
        defer buf.clearRetainingCapacity();
        try out.writeAll(PROMPT);
        try streamUntilEof(in, buf.writer(), '\n', null);
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
    optional_max_size: ?usize,
) !void {
    if (optional_max_size) |max_size| {
        for (0..max_size) |_| {
            const byte: u8 = self.readByte() catch |err| switch (err) {
                error.EndOfStream => return,
                else => return err,
            };
            if (byte == delimiter) return;
            try writer.writeByte(byte);
        }
        return error.StreamTooLong;
    } else {
        while (true) {
            const byte: u8 = try self.readByte();
            if (byte == delimiter) return;
            try writer.writeByte(byte);
        }
        // Can not throw `error.StreamTooLong` since there are no boundary.
    }
}
