const std = @import("std");

pub fn Cell(comptime T: type) type {
    return struct {
        value: UnsafeCell(T),

        const Self = @This();

        pub inline fn init(value: T) Self {
            return Self{ .value = UnsafeCell(T).init(value) };
        }

        pub inline fn get(self: *const Self) T {
            return self.value.get().*;
        }

        pub inline fn getMut(self: *Self) *T {
            return self.value.getMut();
        }

        pub inline fn set(self: *const Self, value: T) void {
            _ = self.replace(value);
        }

        pub inline fn replace(self: *const Self, value: T) T {
            return mem.replace(T, self.value.get(), value);
        }

        pub fn update(self: *const Self, ctx: anytype, f: fn (@TypeOf(ctx), T) T) T {
            const old = self.get();
            const new = f(ctx, old);
            self.set(new);
            return new;
        }
    };
}

const mem = struct {
    inline fn replace(comptime T: type, dest: *T, value: T) T {
        const result = dest.*;
        dest.* = value;
        return result;
    }
};

pub fn UnsafeCell(comptime T: type) type {
    return struct {
        value: T,

        const Self = @This();

        pub inline fn init(value: T) Self {
            return Self{ .value = value };
        }

        pub inline fn get(self: *const Self) *T {
            return @constCast(&self.value);
        }

        pub inline fn getMut(self: *Self) *T {
            return &self.value;
        }
    };
}
