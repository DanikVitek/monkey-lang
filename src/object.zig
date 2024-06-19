const std = @import("std");
const Allocator = std.mem.Allocator;

const trait = @import("lib.zig").trait;
const String = @import("lib.zig").String;

const BlockExpr = @import("Ast.zig").Expression.BlockExpr;

const StringHAMT = @import("im/hamt.zig").StringHashArrayMappedTrie;

pub const Object = struct {
    inner: ObjectInner,
    vtable: *const VTable,

    pub const VOID: Object = (Void{}).object();
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
        else if (T == Void)
            Void{}
        else if (T == Function)
            @ptrCast(@alignCast(self.inner.ptr))
        else
            @ptrCast(@alignCast(self.inner.ptr_const));
    }

    fn CastType(comptime T: type) type {
        return if (T == Integer or T == Boolean or T == Void)
            T
        else if (T == Function)
            *Function
        else
            *const T;
    }

    pub fn deinit(self: Object, alloc: Allocator) void {
        if (self.objectType().isPrimitive()) return;
        switch (self.objectType()) {
            inline .return_value, .break_value, .function, .eval_error => |tag| {
                const impl = self.cast(tag.comptimeType());
                defer alloc.destroy(impl);
                if (tag == .function)
                    impl.deinit()
                else
                    impl.deinit(alloc);
            },
            else => unreachable,
        }
    }
};

pub const ObjectType = enum {
    void,
    integer,
    boolean,
    return_value,
    break_value,
    function,
    eval_error,

    pub fn isPrimitive(self: ObjectType) bool {
        return switch (self) {
            .void, .integer, .boolean => true,
            else => false,
        };
    }

    pub fn comptimeType(comptime self: ObjectType) type {
        return switch (self) {
            .void => Void,
            .integer => Integer,
            .boolean => Boolean,
            .return_value => ReturnValue,
            .break_value => BreakValue,
            .function => Function,
            .eval_error => EvalError,
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
    void: void,
    ptr: *anyopaque,
    ptr_const: *const anyopaque,

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

    fn asConstPtr(self: ObjectInner) *const anyopaque {
        std.debug.assert(self.isPointer());
        return self.ptr;
    }

    fn asPtr(self: ObjectInner) *anyopaque {
        std.debug.assert(self.isPointer());
        return self.ptr;
    }
};

/// The unboxed integer type used in the language.
pub const Int = std.meta.Int(.signed, @typeInfo(usize).Int.bits - 1);

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

pub const Void = struct {
    pub const object_type: ObjectType = .void;

    pub fn inspect(_: ObjectInner, _: Allocator) !String {
        return .{ .borrowed = "()" };
    }

    pub fn eql(_: ObjectInner, _: ObjectInner) bool {
        return true;
    }

    pub fn object(_: Void) Object {
        return .{
            .inner = .{ .void = {} },
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
    scope: Scope,

    pub const object_type: ObjectType = .function;

    pub fn inspect(ctx: ObjectInner, alloc: Allocator) !String {
        const func: *const Function = @ptrCast(@alignCast(ctx.asConstPtr()));
        return .{ .owned = try std.fmt.allocPrint(
            alloc,
            std.fmt.comptimePrint("<function${{x:0>{d}}}/{{d}}>", .{@sizeOf(@TypeOf(func)) * 2}),
            .{ @intFromPtr(func), func.params.len },
        ) };
    }

    pub fn eql(lhs: ObjectInner, rhs: ObjectInner) bool {
        return lhs.asConstPtr() == rhs.asConstPtr();
    }

    pub fn object(self: *Function) Object {
        return .{
            .inner = .{ .ptr = self },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .object_type = object_type,
            },
        };
    }

    pub fn deinit(self: Function) void {
        self.scope.deinit();
    }
};

pub const ReturnValue = struct {
    value: Object,

    pub const object_type: ObjectType = .return_value;

    pub fn inspect(ctx: ObjectInner, alloc: Allocator) !String {
        const ret: *const ReturnValue = @ptrCast(@alignCast(ctx.asConstPtr()));
        return try ret.value.inspect(alloc);
    }

    pub fn eql(lhs: ObjectInner, rhs: ObjectInner) bool {
        const lhs_ret: *const ReturnValue = @ptrCast(@alignCast(lhs.asConstPtr()));
        const rhs_ret: *const ReturnValue = @ptrCast(@alignCast(rhs.asConstPtr()));
        return lhs_ret.value.eql(rhs_ret.value);
    }

    pub fn object(self: *const ReturnValue) Object {
        return .{
            .inner = .{ .ptr_const = self },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .object_type = object_type,
            },
        };
    }

    pub fn deinit(self: ReturnValue, alloc: Allocator) void {
        self.value.deinit(alloc);
    }
};

pub const BreakValue = struct {
    value: Object,

    pub const object_type: ObjectType = .break_value;

    pub fn inspect(ctx: ObjectInner, alloc: Allocator) !String {
        const ret: *const BreakValue = @ptrCast(@alignCast(ctx.asConstPtr()));
        return try ret.value.inspect(alloc);
    }

    pub fn eql(lhs: ObjectInner, rhs: ObjectInner) bool {
        const lhs_ret: *const BreakValue = @ptrCast(@alignCast(lhs.asConstPtr()));
        const rhs_ret: *const BreakValue = @ptrCast(@alignCast(rhs.asConstPtr()));
        return lhs_ret.value.eql(rhs_ret.value);
    }

    pub fn object(self: *const BreakValue) Object {
        return .{
            .inner = .{ .ptr_const = self },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .object_type = object_type,
            },
        };
    }

    pub fn deinit(self: BreakValue, alloc: Allocator) void {
        self.value.deinit(alloc);
    }
};

pub const EvalError = struct {
    message: String,

    pub const object_type: ObjectType = .eval_error;

    pub fn inspect(ctx: ObjectInner, alloc: Allocator) !String {
        const err: *const EvalError = @ptrCast(@alignCast(ctx.asConstPtr()));
        return .{ .owned = try std.fmt.allocPrint(
            alloc,
            "Error: {s}",
            .{err.message.value()},
        ) };
    }

    pub fn eql(lhs: ObjectInner, rhs: ObjectInner) bool {
        const lhs_err: *const EvalError = @ptrCast(@alignCast(lhs.asConstPtr()));
        const rhs_err: *const EvalError = @ptrCast(@alignCast(rhs.asConstPtr()));
        return std.mem.eql(u8, lhs_err.message.value(), rhs_err.message.value());
    }

    pub fn object(self: *const EvalError) Object {
        return .{
            .inner = .{ .ptr_const = self },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .object_type = object_type,
            },
        };
    }

    pub fn deinit(self: EvalError, alloc: Allocator) void {
        self.message.deinit(alloc);
    }
};

pub const Scope = struct {
    store: StringHAMT(Object),

    pub fn init(alloc: Allocator) !Scope {
        return .{
            .store = try StringHAMT(Object).init(alloc),
        };
    }

    pub fn get(self: *const Scope, name: []const u8) ?Object {
        return self.store.get(name);
    }

    pub fn inserted(self: *const Scope, name: []const u8, value: Object) Allocator.Error!Scope {
        return .{ .store = try self.store.inserted(name, value, struct {
            fn valEql(a: Object, b: Object) bool {
                return a.eql(b);
            }
        }.valEql) };
    }

    pub fn insert(self: *Scope, name: []const u8, value: Object) Allocator.Error!void {
        try self.store.insert(name, value, struct {
            fn valEql(a: Object, b: Object) bool {
                return a.eql(b);
            }
        }.valEql);
    }

    pub fn deinit(
        self: Scope,
        // alloc: Allocator,
    ) void {
        // var iter = self.store.iterator();
        // while (iter.next()) |entry| {
        //     entry.value.deinit(alloc);
        // }

        self.store.deinit();
    }

    pub fn clone(self: *const Scope) !Scope {
        return .{ .store = try self.store.clone() };
    }

    pub fn iterator(self: *const Scope) Iterator {
        return Iterator{ .store_iter = self.store.iterator() };
    }

    pub const Iterator = struct {
        store_iter: StringHAMT(Object).Iterator,

        pub fn next(self: *Iterator) ?Object {
            return if (self.store_iter.next()) |entry|
                entry.value
            else
                null;
        }
    };

    // test Iterator {
    //     const testing = std.testing;

    //     var arena = std.heap.ArenaAllocator.init(testing.allocator);
    //     defer arena.deinit();
    //     const alloc = arena.allocator();

    //     var env = try Environment.init(alloc);
    //     try env.insert("a", Object.TRUE);
    //     try env.insert("b", Object.FALSE);

    //     var child_env = try env.inherit(alloc);
    //     try child_env.insert("c", Object.NULL);

    //     const contains = struct {
    //         fn contains(items: []const Object, obj: Object) bool {
    //             for (items) |item| {
    //                 if (item.eql(obj)) return true;
    //             }
    //             return false;
    //         }
    //     }.contains;

    //     var iter = env.iterator();

    //     var items = std.BoundedArray(Object, 2){};
    //     try items.append(iter.next().?);
    //     try items.append(iter.next().?);
    //     try testing.expect(contains(items.constSlice(), Object.TRUE));
    //     try testing.expect(contains(items.constSlice(), Object.FALSE));
    //     try testing.expect(iter.next() == null);

    //     var child_iter = child_env.iterator();

    //     var child_items = std.BoundedArray(Object, 3){};
    //     try child_items.append(child_iter.next().?);
    //     try child_items.append(child_iter.next().?);
    //     try child_items.append(child_iter.next().?);
    //     try testing.expect(contains(child_items.constSlice(), Object.NULL));
    //     try testing.expect(contains(child_items.constSlice(), Object.TRUE));
    //     try testing.expect(contains(child_items.constSlice(), Object.FALSE));
    //     try testing.expect(child_iter.next() == null);
    //     try testing.expect(child_items.get(0).eql(Object.NULL));
    // }
};
