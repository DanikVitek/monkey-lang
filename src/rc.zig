const std = @import("std");
const Allocator = std.mem.Allocator;

const Cell = @import("cell.zig").Cell;

pub fn Rc(comptime T: type) type {
    return struct {
        ptr: *RcBox(T),

        const Self = @This();

        pub fn init(allocator: Allocator, value: T) !Self {
            const ptr = try allocator.create(RcBox(T));
            ptr.strong = 1;
            ptr.weak = 1;
            ptr.value = value;
            return .{ .ptr = ptr };
        }

        pub inline fn copy(self: *const Self) Self {
            self.inner().rcInnerPtr().incStrong();
            return .{ .ptr = self.ptr };
        }

        pub fn deinit(self: *Self, allocator: Allocator) void {
            const rc_inner = self.inner().rcInnerPtr();

            rc_inner.decStrong();
            if (rc_inner.strong() == 0) {
                // remove the implicit "strong weak" pointer
                rc_inner.decWeak();

                if (rc_inner.weak() == 0) {
                    allocator.destroy(self.ptr);
                }
            }
        }

        pub fn asPtr(self: *const Self) *const T {
            return &self.ptr.value;
        }

        pub fn downgrade(self: Rc(T)) Weak(T) {
            self.inner().rcInnerPtr().incWeak();
            std.debug.assert(!isDangling(self.ptr));
            return .{ .ptr = self.ptr };
        }

        pub inline fn strongCount(self: *const Self) usize {
            return self.inner().rcInnerPtr().strong();
        }

        pub inline fn weakCount(self: *const Self) usize {
            return self.inner().rcInnerPtr().weak() - 1;
        }

        inline fn isUnique(self: *const Self) bool {
            return self.strongCount() == 1;
        }

        inline fn inner(self: *const Self) *const RcBox(T) {
            return self.ptr;
        }
    };
}

fn RcBox(comptime T: type) type {
    return extern struct {
        strong: Cell(usize),
        weak: Cell(usize),
        value: T,

        const Self = @This();

        pub inline fn rcInnerPtr(self: *const Self) RcInnerPtr(*const Self, "weak", "strong") {
            return .{ .impl = self };
        }
    };
}

pub fn Weak(comptime T: type) type {
    return struct {
        ptr: *RcBox(T),

        const Self = @This();

        pub fn init() Self {
            return Weak(T){ .ptr = @ptrFromInt(std.math.maxInt(usize)) };
        }

        pub inline fn copy(self: *const Self) Self {
            if (self.inner()) |weak_inner| {
                weak_inner.rcInnerPtr().incWeak();
            }
            return .{ .ptr = self.ptr };
        }

        pub fn deinit(self: *Self, allocator: Allocator) void {
            const weak_inner = (self.inner() orelse return).rcInnerPtr();

            weak_inner.decWeak();
            // the weak count starts at 1, and will only go to zero if all
            // the strong pointers have disappeared.
            if (weak_inner.weak() == 0) {
                allocator.destroy(self.ptr);
            }
        }

        pub fn asPtr(self: *const Self) *const T {
            return if (isDangling(self.ptr))
                @ptrCast(@alignCast(self.ptr))
            else
                &self.ptr.value;
        }

        pub fn upgrade(self: Self) ?Rc(T) {
            const weak_inner = (self.inner() orelse return null).rcInnerPtr();

            return if (weak_inner.strong() == 0)
                null
            else b: {
                weak_inner.incStrong();
                break :b Rc(T){ .ptr = self.ptr };
            };
        }

        pub fn strongCount(self: Self) usize {
            return if (self.inner()) |weak_inner|
                weak_inner.rcInnerPtr().strong()
            else
                0;
        }

        pub fn weakCount(self: Self) usize {
            return if (self.inner()) |weak_inner|
                if (weak_inner.rcInnerPtr().strong() > 0)
                    weak_inner.rcInnerPtr().weak() - 1 // subtract the implicit weak ptr
                else
                    0
            else
                0;
        }

        inline fn inner(self: *const Self) ?WeakInner {
            return if (isDangling(self.ptr))
                null
            else
                WeakInner{
                    .weak = &self.ptr.weak,
                    .strong = &self.ptr.strong,
                };
        }
    };
}

fn isDangling(ptr: *const anyopaque) bool {
    return @intFromPtr(ptr) == std.math.maxInt(usize);
}

const WeakInner = struct {
    weak: *const Cell(usize),
    strong: *const Cell(usize),

    const Self = @This();

    pub inline fn rcInnerPtr(self: *const Self) RcInnerPtr(Self, "weak", "strong") {
        return .{ .impl = self };
    }
};

fn RcInnerPtr(
    comptime T: type,
    comptime weak_field: []const u8,
    comptime strong_field: []const u8,
) type {
    return struct {
        impl: T,

        const Self = @This();

        pub inline fn weakRef(self: *const Self) *const Cell(usize) {
            return @field(self.impl, weak_field);
        }

        pub inline fn strongRef(self: *const Self) *const Cell(usize) {
            return @field(self.impl, strong_field);
        }

        pub inline fn strong(self: *const Self) usize {
            return self.strongRef().get();
        }

        pub inline fn incStrong(self: *const Self) void {
            const strong_value = self.strong();

            std.debug.assert(strong_value != 0);

            const new_strong_value = strong_value +% 1;
            self.strongRef().set(new_strong_value);

            if (new_strong_value == 0) {
                std.process.abort();
            }
        }

        pub inline fn decStrong(self: *const Self) void {
            self.strongRef().set(self.strong() - 1);
        }

        pub inline fn weak(self: *const Self) usize {
            return self.weakRef().get();
        }

        pub inline fn incWeak(self: *const Self) void {
            const weak_value = self.weak();

            std.debug.assert(weak_value != 0);

            const new_weak_value = weak_value +% 1;
            self.weakRef().set(new_weak_value);

            if (new_weak_value == 0) {
                std.process.abort();
            }
        }

        pub inline fn decWeak(self: *const Self) void {
            self.weakRef().set(self.weak() - 1);
        }
    };
}

const testing = std.testing;

test Rc {
    const alloc = testing.allocator;

    var rc = try Rc(u32).init(alloc, 42);

    try testing.expect(rc.strongCount() == 1);

    {
        var rc2 = rc.copy();
        defer rc2.deinit(alloc);

        try testing.expect(rc.strongCount() == 2);
        try testing.expect(rc2.strongCount() == 2);

        try testing.expectEqualDeep(rc.asPtr(), rc2.asPtr());
    }

    try testing.expect(rc.strongCount() == 1);

    {
        var weak = rc.downgrade();
        defer weak.deinit(alloc);

        try testing.expect(rc.strongCount() == 1);
        try testing.expect(rc.weakCount() == 2);

        try testing.expectEqualDeep(rc.asPtr(), weak.asPtr());

        var rc2 = try weak.upgrade();
        try testing.expect(rc2 != null);
        defer rc2.?.deinit(alloc);

        try testing.expect(rc.strongCount() == 2);
        try testing.expect(rc.weakCount() == 2);

        try testing.expectEqualDeep(rc.asPtr(), rc2.?.asPtr());
    }

    try testing.expect(rc.strongCount() == 1);
    try testing.expect(rc.weakCount() == 1);

    rc.deinit(alloc);

    try testing.expect(rc.strongCount() == 0);
    try testing.expect(rc.weakCount() == 0);
}
