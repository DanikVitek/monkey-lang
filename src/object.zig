const std = @import("std");
const Allocator = std.mem.Allocator;

const trait = @import("lib.zig").trait;
const MaybeOwnedSlice = @import("lib.zig").MaybeOwnedSlice;

const String = MaybeOwnedSlice(u8, null);

pub const Object = struct {
    inner: ObjectInner,
    vtable: *const VTable,

    pub const NULL: Object = (Null{}).object();
    pub const FALSE: Object = (Boolean{ .value = false }).object();
    pub const TRUE: Object = (Boolean{ .value = true }).object();

    const VTable = struct {
        inspectFn: *const fn (ctx: ObjectInner, alloc: Allocator) Allocator.Error!String,
        eqlFn: *const fn (lhs: ObjectInner, rhs: ObjectInner) bool,
        object_type: ObjectType,
    };

    pub inline fn inspect(self: Object, alloc: Allocator) Allocator.Error!String {
        return self.vtable.inspectFn(self.inner, alloc);
    }

    pub inline fn objectType(self: Object) ObjectType {
        return self.vtable.object_type;
    }

    pub inline fn eql(self: Object, other: Object) bool {
        return self.objectType() == other.objectType() and self.vtable.eqlFn(self.inner, other.inner);
    }

    pub fn cast(self: Object, comptime T: type) CastType(T) {
        std.debug.assert(self.objectType() == T.object_type);
        return if (T == Integer)
            Integer{ .value = self.inner.asInt() }
        else if (T == Boolean)
            Boolean{ .value = self.inner.asBool() }
        else if (T == Null)
            NULL
        else
            @ptrCast(@alignCast(self.inner.ptr));
    }

    fn CastType(comptime T: type) type {
        return if (T == Integer or T == Boolean or T == Null)
            T
        else
            *const T;
    }
};

pub const ObjectType = enum {
    null,
    integer,
    boolean,
};

const ObjectInner = packed union {
    int: packed struct {
        tag: u1 = 1,
        value: i63,
    },
    bool: packed struct {
        tag: u1 = 1,
        _: u62 = 0,
        value: bool,
    },
    ptr: *const anyopaque,

    fn isPointer(self: ObjectInner) bool {
        return self.int.tag != 1;
    }

    fn asInt(self: ObjectInner) i63 {
        std.debug.assert(!self.isPointer());
        return self.int.value;
    }

    fn asBool(self: ObjectInner) bool {
        std.debug.assert(!self.isPointer());
        return self.bool.value;
    }

    fn asPtr(self: ObjectInner) *const anyopaque {
        std.debug.assert(self.isPointer());
        return self.ptr;
    }
};

pub const Integer = struct {
    value: i63,

    pub const object_type: ObjectType = ObjectType.integer;

    pub fn inspect(ctx: ObjectInner, alloc: Allocator) !String {
        const value: i63 = ctx.asInt();
        return .{ .owned = try std.fmt.allocPrint(alloc, "{d}", .{value}) };
    }

    pub fn eql(lhs: ObjectInner, rhs: ObjectInner) bool {
        const lhs_int: i63 = lhs.asInt();
        const rhs_int: i63 = rhs.asInt();
        return lhs_int == rhs_int;
    }

    pub fn object(self: Integer) Object {
        return .{
            .inner = .{ .int = .{ .value = self.value } },
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

    pub fn inspect(ctx: ObjectInner, _: Allocator) !String {
        const value: bool = ctx.asBool();
        return .{ .borrowed = if (value) "true" else "false" };
    }

    pub fn eql(lhs: ObjectInner, rhs: ObjectInner) bool {
        const lhs_bool: bool = lhs.asBool();
        const rhs_bool: bool = rhs.asBool();
        return lhs_bool == rhs_bool;
    }

    pub fn object(self: Boolean) Object {
        return .{
            .inner = .{ .bool = .{ .value = self.value } },
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

    pub fn inspect(_: ObjectInner, _: Allocator) !String {
        return .{ .borrowed = "null" };
    }

    pub fn eql(_: ObjectInner, _: ObjectInner) bool {
        return true;
    }

    pub fn object(_: Null) Object {
        return .{
            .inner = .{ .ptr = &{} },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .object_type = object_type,
            },
        };
    }
};
