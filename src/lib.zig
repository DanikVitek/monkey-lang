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

pub const String = MaybeOwnedSlice(u8, null);

pub const MaybeSmallString = packed union {
    small: SmallString,
    large: LargeString,

    const SmallString = extern struct {
        len: std.math.IntFittingRange(0, max_size) align(1),
        data: [max_size]u8,

        pub const max_size = @sizeOf(LargeString) - 1;
    };

    const LargeString = extern struct {
        data: [*]const u8,
        len: usize,
    };
};
