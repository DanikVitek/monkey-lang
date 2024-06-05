const std = @import("std");

pub const trait = @import("trait.zig");

pub fn MaybeOwnedSlice(comptime T: type, comptime sentinel: ?T) type {
    const Slice = if (sentinel) |t| [:t]const T else []const T;
    return union(enum) {
        borrowed: Slice,
        owned: Slice,

        pub fn deinit(self: @This(), alloc: std.mem.Allocator) void {
            switch (self) {
                .owned => |slice| alloc.free(slice),
                .borrowed => {},
            }
        }

        pub fn value(self: @This()) Slice {
            return switch (self) {
                inline else => |val| val,
            };
        }
    };
}
