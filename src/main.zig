const std = @import("std");

const repl = @import("repl.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const alloc = arena.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    try repl.start(alloc, stdin, stdout, stderr, &arena, std.heap.ArenaAllocator.queryCapacity);
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
