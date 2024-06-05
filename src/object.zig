const std = @import("std");
const Allocator = std.mem.Allocator;

const trait = @import("lib.zig").trait;
const MaybeOwnedSlice = @import("lib.zig").MaybeOwnedSlice;

const String = MaybeOwnedSlice(u8, null);

pub const Object = struct {
    ptr: *const anyopaque,
    vtable: *const VTable,

    pub const NULL: Object = (&Null{}).object();
    pub const FALSE: Object = (&Boolean{ .value = false }).object();
    pub const TRUE: Object = (&Boolean{ .value = true }).object();

    const VTable = struct {
        inspectFn: *const fn (ctx: *const anyopaque, alloc: Allocator) Allocator.Error!String,
        object_type: ObjectType,
    };

    pub inline fn inspect(self: Object, alloc: Allocator) Allocator.Error!String {
        return self.vtable.inspectFn(self.ptr, alloc);
    }

    pub inline fn objectType(self: Object) ObjectType {
        return self.vtable.object_type;
    }

    pub fn cast(self: Object, comptime T: type) *const T {
        std.debug.assert(self.objectType() == T.object_type);
        return @ptrCast(@alignCast(self.ptr));
    }
};
pub const ObjectType = enum {
    null,
    integer,
    boolean,
};

pub const Integer = struct {
    sign: Sign = .plus,
    value: u64,

    pub const object_type: ObjectType = ObjectType.integer;

    pub const Sign = enum(u1) {
        plus,
        minus,

        pub fn toStr(self: Sign) []const u8 {
            return switch (self) {
                .plus => "",
                .minus => "-",
            };
        }

        pub fn opposite(self: Sign) Sign {
            return switch (self) {
                .plus => .minus,
                .minus => .plus,
            };
        }
    };

    pub fn inspect(ctx: *const anyopaque, alloc: Allocator) !String {
        const self: *const Integer = @ptrCast(@alignCast(ctx));
        return .{ .owned = try std.fmt.allocPrint(
            alloc,
            "{s}{d}",
            .{ self.sign.toStr(), self.value },
        ) };
    }

    pub fn object(self: *const Integer) Object {
        return .{
            .ptr = self,
            .vtable = &.{
                .inspectFn = inspect,
                .object_type = object_type,
            },
        };
    }
};

pub const Boolean = struct {
    value: bool,

    pub const object_type: ObjectType = ObjectType.boolean;

    pub fn inspect(ctx: *const anyopaque, _: Allocator) !String {
        const self: *const Boolean = @ptrCast(@alignCast(ctx));
        return .{ .borrowed = if (self.value) "true" else "false" };
    }

    pub fn object(self: *const Boolean) Object {
        return .{
            .ptr = self,
            .vtable = &.{
                .inspectFn = inspect,
                .object_type = object_type,
            },
        };
    }
};

pub const Null = struct {
    pub const object_type: ObjectType = ObjectType.null;

    pub fn inspect(_: *const anyopaque, _: Allocator) !String {
        return .{ .borrowed = "null" };
    }

    pub fn object(self: *const Null) Object {
        return .{
            .ptr = self,
            .vtable = &.{
                .inspectFn = inspect,
                .object_type = object_type,
            },
        };
    }
};
