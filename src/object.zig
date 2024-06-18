const std = @import("std");
const Allocator = std.mem.Allocator;

const trait = @import("lib.zig").trait;
const String = @import("lib.zig").String;

const BlockExpr = @import("Ast.zig").Expression.BlockExpr;

const StringHAMT = @import("im/hamt.zig").StringHashArrayMappedTrie;

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

    pub inline fn isFunction(self: Object) bool {
        return self.objectType() == ObjectType.function;
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
    int: packed struct(usize) {
        tag: u1 = 1,
        value: Int,
    },
    bool: packed struct(usize) {
        tag: u1 = 1,
        _: std.meta.Int(.unsigned, @typeInfo(usize).Int.bits - 2) = 0,
        value: bool,
    },
    ptr: *const anyopaque,

    fn isPointer(self: ObjectInner) bool {
        return self.int.tag != 1;
    }

    fn asInt(self: ObjectInner) Int {
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

/// The unboxed integer type used in the language.
pub const Int: type = std.meta.Int(.signed, @typeInfo(usize).Int.bits - 1);

pub const Integer = struct {
    value: Int,

    pub const object_type: ObjectType = .integer;

    pub fn inspect(ctx: ObjectInner, alloc: Allocator) !String {
        const value: Int = ctx.asInt();
        return .{ .owned = try std.fmt.allocPrint(alloc, "{d}", .{value}) };
    }

    pub fn eql(lhs: ObjectInner, rhs: ObjectInner) bool {
        return lhs.asInt() == rhs.asInt();
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
    parent: ?*const Environment = null,
    store: StringHAMT(Object),

    pub fn init(alloc: Allocator) !Environment {
        return .{
            .store = try StringHAMT(Object).init(alloc),
        };
    }

    pub fn get(
        self: *const Environment,
        name: []const u8,
        // func_idx: ?usize,
    ) ?Object {
        return self.store.getEntry(name) orelse if (self.parent) |parent| parent.get(name) else null;
    }

    pub fn set(self: *const Environment, alloc: Allocator, name: []const u8, value: Object) Allocator.Error!Environment {
        return .{
            .parent = self.parent,
            .store = try self.store.insert(alloc, name, value),
        };
    }

    pub fn inherit(self: *const Environment, alloc: Allocator) !Environment {
        return Environment{ .parent = self, .store = try Environment.init(alloc) };
    }

    pub fn inheritEnsureUnusedCapacity(self: *const Environment, alloc: Allocator, capacity: usize) Allocator.Error!Environment {
        var cloned = self.inherit();
        try cloned.store.ensureTotalCapacity(alloc, capacity);
        return cloned;
    }

    pub fn ensureUnusedCapacity(self: *Environment, alloc: Allocator, capacity: usize) Allocator.Error!void {
        try self.store.ensureTotalCapacity(alloc, capacity);
    }

    pub fn deinit(self: Environment, alloc: Allocator) void {
        var iter = self.store.valueIterator();
        while (iter.next()) |value| {
            value.deinit(alloc);
        }

        self.store.deinit(alloc);
    }

    pub fn iterator(self: *const Environment) Iterator {
        return Iterator{
            .parent_env = self.parent,
            .store_iter = self.store.iterator(),
        };
    }

    pub const Iterator = struct {
        parent_env: ?*const Environment,
        store_iter: std.StringArrayHashMapUnmanaged(Object).Iterator,

        pub fn next(self: *Iterator) ?Object {
            return if (self.store_iter.next()) |entry|
                entry.value_ptr.*
            else if (self.parent_env) |parent_env| b: {
                self.store_iter = parent_env.store.iterator();
                self.parent_env = parent_env.parent;
                break :b self.next();
            } else null;
        }
    };

    test Iterator {
        const testing = std.testing;

        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const alloc = arena.allocator();

        var env = Environment{};
        try env.set(alloc, "a", Object.TRUE);
        try env.set(alloc, "b", Object.FALSE);

        var child_env = try env.inheritEnsureUnusedCapacity(alloc, 1);
        try child_env.set(alloc, "c", Object.NULL);

        var iter = env.iterator();

        try testing.expect(iter.next().?.eql(Object.TRUE));
        try testing.expect(iter.next().?.eql(Object.FALSE));
        try testing.expect(iter.next() == null);

        var child_iter = child_env.iterator();

        try testing.expect(child_iter.next().?.eql(Object.NULL));
        try testing.expect(child_iter.next().?.eql(Object.TRUE));
        try testing.expect(child_iter.next().?.eql(Object.FALSE));
        try testing.expect(child_iter.next() == null);
    }
};
