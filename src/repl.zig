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
const Scope = @import("object.zig").Environment;

const pretty = @import("pretty");

const PROMPT = ">> ";

pub fn start(
    alloc: Allocator,
    in: Reader,
    out: Writer,
    err: Writer,
    // query_capacity_ctx: anytype,
    // queryCapacity: fn (@typeInfo(@TypeOf(query_capacity_ctx)).Pointer.child) usize,
) !void {
    var source = ArrayList(u8).init(alloc);
    var line_start: usize = 0;
    var line_end: usize = 0;

    var scope = try Scope.init(alloc);

    while (true) {
        try err.writeAll(PROMPT);

        var len_read = try streamUntilEof(in, source.writer(), '\n');
        if (source.getLastOrNull()) |last| {
            if (last == '\r') {
                _ = source.pop();
                len_read -= 1;
            }
        }
        try source.append(';');
        len_read += 1;

        line_start = line_end;
        line_end += len_read;

        var lexer = try Lexer.init(source.items[line_start..]);
        var parser = Parser.init(&lexer);
        const ast = parser.parseProgram(alloc) catch |e| {
            try err.print("Parse error: {s}\n", .{@errorName(e)});
            continue;
        };

        const evaluated, const new_scope = try evaluator.execute(alloc, ast, scope);
        if (!std.meta.eql(scope, new_scope)) {
            scope.deinit();
            scope = new_scope;
        }

        std.debug.print("Env {{\n", .{});
        var scope_iter = scope.store.iterator();
        while (scope_iter.next()) |entry| {
            const value_str = try entry.value.inspect(alloc);
            defer value_str.deinit(alloc);
            try out.print("    {s}: {s}\n", .{ entry.key, value_str });
        }
        std.debug.print("}}\n", .{});

        // std.debug.print(
        //     "Memory allocated: {d} (for sources: {d})\n",
        //     .{ queryCapacity(query_capacity_ctx.*), source.capacity },
        // );

        const str = try evaluated.inspect(alloc);
        defer str.deinit(alloc);
        try out.print("{s}\n\n", .{str});
        if (evaluated.isError()) {
            evaluated.deinit(alloc);
        }
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
