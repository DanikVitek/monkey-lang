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
        eqlFn: *const fn (lhs: *const anyopaque, rhs: *const anyopaque) bool,
        object_type: ObjectType,
    };

    pub inline fn inspect(self: Object, alloc: Allocator) Allocator.Error!String {
        return self.vtable.inspectFn(self.ptr, alloc);
    }

    pub inline fn objectType(self: Object) ObjectType {
        return self.vtable.object_type;
    }

    pub inline fn eql(self: Object, other: Object) bool {
        return self.objectType() == other.objectType() and self.vtable.eqlFn(self.ptr, other.ptr);
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
    value: i65,

    pub const object_type: ObjectType = ObjectType.integer;

    pub fn inspect(ctx: *const anyopaque, alloc: Allocator) !String {
        const self: *const Integer = @ptrCast(@alignCast(ctx));
        return .{ .owned = try std.fmt.allocPrint(alloc, "{d}", .{self.value}) };
    }

    pub fn eql(lhs: *const anyopaque, rhs: *const anyopaque) bool {
        const lhs_int: *const Integer = @ptrCast(@alignCast(lhs));
        const rhs_int: *const Integer = @ptrCast(@alignCast(rhs));
        return lhs_int.value == rhs_int.value;
    }

    pub fn object(self: *const Integer) Object {
        return .{
            .ptr = self,
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
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

    pub fn eql(lhs: *const anyopaque, rhs: *const anyopaque) bool {
        const lhs_bool: *const Boolean = @ptrCast(@alignCast(lhs));
        const rhs_bool: *const Boolean = @ptrCast(@alignCast(rhs));
        return lhs_bool.value == rhs_bool.value;
    }

    pub fn object(self: *const Boolean) Object {
        return .{
            .ptr = self,
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
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

    pub fn eql(_: *const anyopaque, _: *const anyopaque) bool {
        return true;
    }

    pub fn object(self: *const Null) Object {
        return .{
            .ptr = self,
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .object_type = object_type,
            },
        };
    }
};
