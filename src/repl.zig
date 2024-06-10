const builtin = @import("builtin");

const std = @import("std");
const Allocator = std.mem.Allocator;
const Reader = std.fs.File.Reader;
const Writer = std.fs.File.Writer;
const ArrayList = std.ArrayList;

const Token = @import("token.zig").Token;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const evaluator = @import("eval.zig");

const pretty = @import("pretty");

const PROMPT = ">> ";

pub fn start(arena: *std.heap.ArenaAllocator, in: Reader, out: Writer, err: Writer) !void {
    const alloc = arena.allocator();

    while (true) {
        defer _ = arena.reset(.retain_capacity);

        var buf = ArrayList(u8).init(alloc);
        try err.writeAll(PROMPT);

        try streamUntilEof(in, buf.writer(), '\n');
        if (std.mem.indexOfScalar(u8, buf.items, '\r')) |index| {
            buf.items.len = index + 1;
        }
        try buf.append(';');

        var lexer = try Lexer.init(buf.items);
        var parser = Parser.init(&lexer);
        const ast = parser.parseProgram(alloc) catch |e| {
            try err.print("Error: {s}\n", .{@errorName(e)});
            continue;
        };

        const evaluated = try evaluator.execute(alloc, ast);
        const str = try evaluated.inspect(alloc);
        try out.print("{s}\n\n", .{str.value()});
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
            else => return @as(Reader.Error, @errorCast(err)),
        };
        if (byte == delimiter) return;
        try writer.writeByte(byte);
    }
}
