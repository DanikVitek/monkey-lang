const std = @import("std");

const repl = @import("repl.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    try repl.start(alloc, stdin, stdout, stderr); //, &gpa, std.heap.GeneralPurposeAllocator(.{}).queryCapacity);
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
