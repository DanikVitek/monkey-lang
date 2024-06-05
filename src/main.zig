const std = @import("std");

const repl = @import("repl.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // defer arena.deinit();
    const allocator = arena.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    try repl.start(stdin, stdout, stderr, allocator);
}

test {
    _ = @import("token.zig");
    _ = @import("Lexer.zig");
    _ = @import("Parser.zig");
    _ = @import("eval.zig");
}
