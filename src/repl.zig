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
const Environment = @import("object.zig").Environment;

const pretty = @import("pretty");

const PROMPT = ">> ";

pub fn start(alloc: Allocator, in: Reader, out: Writer, err: Writer) !void {
    var buf = ArrayList(u8).init(alloc);
    var line_start: usize = 0;
    var line_end: usize = 0;

    var env = Environment{};

    while (true) {
        try err.writeAll(PROMPT);

        const len_read = try streamUntilEof(in, buf.writer(), '\n');
        try buf.append(';');
        const len = len_read + 1;

        line_start = line_end;
        line_end += len;

        var lexer = try Lexer.init(buf.items[line_start..line_end]);
        var parser = Parser.init(&lexer);
        const ast = parser.parseProgram(alloc) catch |e| {
            try err.print("Error: {s}\n", .{@errorName(e)});
            continue;
        };

        const evaluated = try evaluator.execute(alloc, ast, &env);
        const str = try evaluated.inspect(alloc);
        try out.print("{s}\n\n", .{str.value()});
    }
}

fn streamUntilEof(
    self: Reader,
    writer: anytype,
    delimiter: u8,
) !usize {
    var count: usize = 0;

    while (true) : (count += 1) {
        const byte: u8 = self.readByte() catch |err| switch (err) {
            error.EndOfStream => return count,
            else => return @as(Reader.Error, @errorCast(err)),
        };
        if (byte == delimiter) return count;
        try writer.writeByte(byte);
    }
}
