const std = @import("std");
const Allocator = std.mem.Allocator;

const trait = @import("lib.zig").trait;
const String = @import("lib.zig").String;

const BlockExpr = @import("Ast.zig").Expression.BlockExpr;

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

    pub inline fn isError(self: Object) bool {
        return self.objectType() == ObjectType.eval_error;
    }

    pub inline fn cast(self: Object, comptime T: type) CastType(T) {
        std.debug.assert(self.objectType() == T.object_type);
        return if (T == Integer)
            Integer{ .value = self.inner.asInt() }
        else if (T == Boolean)
            Boolean{ .value = self.inner.asBool() }
        else if (T == Null)
            Null{}
        else
            @ptrCast(@alignCast(self.inner.ptr));
    }

    fn CastType(comptime T: type) type {
        return if (T == Integer or T == Boolean or T == Null)
            T
        else
            *const T;
    }

    pub fn deinit(self: Object, alloc: Allocator) void {
        if (self.objectType().isPrimitive()) return;
        switch (self.objectType()) {
            .return_value => {
                const ret = self.cast(ReturnValue);
                defer alloc.destroy(ret);
                ret.value.deinit(alloc);
            },
            .eval_error => {
                const err = self.cast(EvalError);
                defer alloc.destroy(err);
                err.message.deinit(alloc);
            },
            else => unreachable,
        }
    }
};

pub const ObjectType = enum {
    null,
    integer,
    boolean,
    return_value,
    break_value,
    function,
    eval_error,

    pub fn isPrimitive(self: ObjectType) bool {
        return switch (self) {
            .null, .integer, .boolean => true,
            else => false,
        };
    }
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

    pub const object_type: ObjectType = .integer;

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

    pub const object_type: ObjectType = .boolean;

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
    pub const object_type: ObjectType = .null;

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

pub const Function = struct {
    params: []const []const u8,
    body: BlockExpr,
    env: *const Environment,

    pub const object_type: ObjectType = .function;

    pub fn inspect(ctx: ObjectInner, alloc: Allocator) !String {
        const func: *const Function = @ptrCast(@alignCast(ctx.asPtr()));
        return .{ .owned = try std.fmt.allocPrint(
            alloc,
            std.fmt.comptimePrint("<function${{x:0>{d}}}/{{d}}>", .{@sizeOf(@TypeOf(func)) * 2}),
            .{ @intFromPtr(func), func.params.len },
        ) };
    }

    pub fn eql(lhs: ObjectInner, rhs: ObjectInner) bool {
        return lhs.asPtr() == rhs.asPtr();
    }

    pub fn object(self: *const Function) Object {
        return .{
            .inner = .{ .ptr = self },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .object_type = object_type,
            },
        };
    }
};

pub const ReturnValue = struct {
    value: Object,

    pub const object_type: ObjectType = .return_value;

    pub fn inspect(ctx: ObjectInner, alloc: Allocator) !String {
        const ret: *const ReturnValue = @ptrCast(@alignCast(ctx.asPtr()));
        return try ret.value.inspect(alloc);
    }

    pub fn eql(lhs: ObjectInner, rhs: ObjectInner) bool {
        const lhs_ret: *const ReturnValue = @ptrCast(@alignCast(lhs.asPtr()));
        const rhs_ret: *const ReturnValue = @ptrCast(@alignCast(rhs.asPtr()));
        return lhs_ret.value.eql(rhs_ret.value);
    }

    pub fn object(self: *const ReturnValue) Object {
        return .{
            .inner = .{ .ptr = self },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .object_type = object_type,
            },
        };
    }
};

pub const BreakValue = struct {
    value: Object,

    pub const object_type: ObjectType = .break_value;

    pub fn inspect(ctx: ObjectInner, alloc: Allocator) !String {
        const ret: *const BreakValue = @ptrCast(@alignCast(ctx.asPtr()));
        return try ret.value.inspect(alloc);
    }

    pub fn eql(lhs: ObjectInner, rhs: ObjectInner) bool {
        const lhs_ret: *const BreakValue = @ptrCast(@alignCast(lhs.asPtr()));
        const rhs_ret: *const BreakValue = @ptrCast(@alignCast(rhs.asPtr()));
        return lhs_ret.value.eql(rhs_ret.value);
    }

    pub fn object(self: *const BreakValue) Object {
        return .{
            .inner = .{ .ptr = self },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .object_type = object_type,
            },
        };
    }
};

pub const EvalError = struct {
    message: String,

    pub const object_type: ObjectType = .eval_error;

    pub fn inspect(ctx: ObjectInner, alloc: Allocator) !String {
        const err: *const EvalError = @ptrCast(@alignCast(ctx.asPtr()));
        return .{ .owned = try std.fmt.allocPrint(
            alloc,
            "Error: {s}",
            .{err.message.value()},
        ) };
    }

    pub fn eql(lhs: ObjectInner, rhs: ObjectInner) bool {
        const lhs_err: *const EvalError = @ptrCast(@alignCast(lhs.asPtr()));
        const rhs_err: *const EvalError = @ptrCast(@alignCast(rhs.asPtr()));
        return std.mem.eql(u8, lhs_err.message.value(), rhs_err.message.value());
    }

    pub fn object(self: *const EvalError) Object {
        return .{
            .inner = .{ .ptr = self },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .object_type = object_type,
            },
        };
    }
};

pub const Environment = struct {
    store: std.StringHashMapUnmanaged(Object) = .{},

    pub fn get(self: *const Environment, name: []const u8) ?Object {
        return self.store.get(name);
    }

    pub fn set(self: *Environment, alloc: Allocator, name: []const u8, value: Object) Allocator.Error!void {
        try self.store.put(alloc, name, value);
    }

    pub fn clone(self: *const Environment, alloc: Allocator) Allocator.Error!Environment {
        return Environment{ .store = try self.store.clone(alloc) };
    }

    pub fn deinit(self: Environment, alloc: Allocator) void {
        var iter = self.store.valueIterator();
        while (iter.next()) |value| {
            value.deinit(alloc);
        }

        self.store.deinit(alloc);
    }
};
