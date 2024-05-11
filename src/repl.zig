const builtin = @import("builtin");

const std = @import("std");
const Allocator = std.mem.Allocator;
const Reader = std.fs.File.Reader;
const Writer = std.fs.File.Writer;
const ArrayList = std.ArrayList;

const Token = @import("token.zig").Token;
const Lexer = @import("Lexter.zig");
const Parser = @import("Parser.zig");

const pretty = @import("pretty");

const PROMPT = ">> ";

pub fn start(in: Reader, out: Writer, err: Writer, alloc: Allocator) !void {
    var buf = ArrayList(u8).init(alloc);
    // defer buf.deinit();

    while (true) {
        defer buf.clearRetainingCapacity();
        try err.writeAll(PROMPT);

        try streamUntilEof(in, buf.writer(), '\n');
        const input = std.mem.trimRight(u8, buf.items, "\r");

        var lexer = try Lexer.init(input);
        var parser = Parser.init(&lexer);
        const ast = try parser.parseProgram(alloc);

        const slice = ast.program.slice();

        const opts: pretty.Options = .{
            .max_depth = 10,
            // .show_type_names = false,
            .empty_line_at_end = true,
            .slice_u8_is_str = true,
        };

        for (0..slice.len) |i| {
            var output = try pretty.dumpAsList(alloc, slice.get(i), opts);
            defer output.deinit();

            // [Option] If custom formatting is not specified
            if (opts.fmt.len == 0) {
                // [Option] Insert an extra newline (to stack up multiple outputs)
                try output.appendSlice(if (opts.empty_line_at_end) "\n\n" else "\n");
            }

            try std.fmt.format(out, "{s}", .{output.items});
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
            else => return @as(Reader.Error, @errorCast(err)),
        };
        if (byte == delimiter) return;
        try writer.writeByte(byte);
    }
}
