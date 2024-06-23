const std = @import("std");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;
const AtomicUsize = std.atomic.Value(usize);

const String = @import("lib.zig").String;

const BlockExpr = @import("Ast.zig").Expression.BlockExpr;

const StringHAMT = @import("im/hamt.zig").StringHashArrayMappedTrie;
const Rc = @import("rc.zig").Rc;

pub const Object = struct {
    inner: ObjectInner,
    vtable: *const VTable,

    pub const VOID: Object = (Void{}).object();
    pub const FALSE: Object = (Boolean{ .value = false }).object();
    pub const TRUE: Object = (Boolean{ .value = true }).object();

    const VTable = struct {
        inspectFn: *const fn (ctx: ObjectInner, alloc: Allocator) Allocator.Error!String,
        eqlFn: *const fn (lhs: ObjectInner, rhs: ObjectInner) bool,
        copyFn: *const fn (ctx: ObjectInner) ObjectInner,
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
        return switch (T) {
            Integer => Integer{ .value = self.inner.asInt() },
            Boolean => Boolean{ .value = self.inner.asBool() },
            Void => Void{},
            Function => @ptrCast(@alignCast(self.inner.ptr)),
            else => @ptrCast(@alignCast(self.inner.ptr_const)),
        };
    }

    fn CastType(comptime T: type) type {
        return switch (T) {
            Integer, Boolean, Void => T,
            Function => *Function,
            else => *const T,
        };
    }

    pub fn copy(self: *const Object) Object {
        return .{
            .inner = self.vtable.copyFn(self.inner),
            .vtable = self.vtable,
        };
    }

    pub fn deinit(self: Object, alloc: Allocator) void {
        switch (self.objectType()) {
            inline .function, .eval_error => |tag| {
                const impl = self.cast(tag.Type());
                impl.deinit(alloc);
            },
            .return_value, .break_value => unreachable,
            else => return,
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

    pub fn Type(comptime self: ObjectType) type {
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

    pub fn copy(ctx: ObjectInner) ObjectInner {
        return ctx;
    }

    pub fn object(self: Integer) Object {
        return .{
            .inner = .{ .int = .{ .value = self.value } },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .copyFn = copy,
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

    pub fn copy(ctx: ObjectInner) ObjectInner {
        return ctx;
    }

    pub fn object(self: Boolean) Object {
        return .{
            .inner = .{ .bool = .{ .value = self.value } },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .copyFn = copy,
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

    pub fn copy(ctx: ObjectInner) ObjectInner {
        return ctx;
    }

    pub fn object(_: Void) Object {
        return .{
            .inner = .{ .void = {} },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .copyFn = copy,
                .object_type = object_type,
            },
        };
    }
};

pub const Function = struct {
    params: []const []const u8,
    body: BlockExpr,
    env: Environment,
    ref_count: usize = if (builtin.single_threaded) 1 else AtomicUsize.init(1),

    const RefCount = if (builtin.single_threaded) usize else AtomicUsize;

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

    pub fn copy(ctx: ObjectInner) ObjectInner {
        const func: *Function = @ptrCast(@alignCast(ctx.asPtr()));
        if (builtin.single_threaded)
            func.ref_count += 1
        else
            _ = func.ref_count.fetchAdd(1, .release);
        return ctx;
    }

    pub fn object(self: *Function) Object {
        return .{
            .inner = .{ .ptr = self },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .copyFn = copy,
                .object_type = object_type,
            },
        };
    }

    pub fn deinit(self: *Function, alloc: Allocator) void {
        if (builtin.single_threaded) {
            const prev_ref_count: usize = self.ref_count;
            self.ref_count = prev_ref_count - 1;
            if (prev_ref_count == 1) {
                self.env.deinit();
                alloc.destroy(self);
            }
        } else {
            const ref_count: *AtomicUsize = &self.ref_count;
            if (ref_count.fetchSub(1, .release) == 1) {
                ref_count.fence(.acquire);
                self.env.deinit();
                alloc.destroy(self);
            }
        }
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

    pub fn copy(_: ObjectInner) noreturn {
        unreachable;
    }

    pub fn object(self: *const ReturnValue) Object {
        return .{
            .inner = .{ .ptr_const = self },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .copyFn = copy,
                .object_type = object_type,
            },
        };
    }

    pub fn deinit(self: *const ReturnValue, alloc: Allocator) void {
        self.value.deinit(alloc);
        alloc.destroy(self);
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

    pub fn copy(_: ObjectInner) noreturn {
        unreachable;
    }

    pub fn object(self: *const BreakValue) Object {
        return .{
            .inner = .{ .ptr_const = self },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .copyFn = copy,
                .object_type = object_type,
            },
        };
    }

    pub fn deinit(self: *const BreakValue, alloc: Allocator) void {
        self.value.deinit(alloc);
        alloc.destroy(self);
    }
};

pub const EvalError = struct {
    message: String,

    pub const object_type: ObjectType = .eval_error;

    pub fn inspect(ctx: ObjectInner, alloc: Allocator) !String {
        const err: *const EvalError = @ptrCast(@alignCast(ctx.asConstPtr()));
        return .{ .owned = try std.fmt.allocPrint(alloc, "Error: {s}", .{err.message}) };
    }

    pub fn eql(lhs: ObjectInner, rhs: ObjectInner) bool {
        const lhs_err: *const EvalError = @ptrCast(@alignCast(lhs.asConstPtr()));
        const rhs_err: *const EvalError = @ptrCast(@alignCast(rhs.asConstPtr()));
        return std.mem.eql(u8, lhs_err.message.value(), rhs_err.message.value());
    }

    pub fn copy(_: ObjectInner) noreturn {
        unreachable;
    }

    pub fn object(self: *const EvalError) Object {
        return .{
            .inner = .{ .ptr_const = self },
            .vtable = &.{
                .inspectFn = inspect,
                .eqlFn = eql,
                .copyFn = copy,
                .object_type = object_type,
            },
        };
    }

    pub fn deinit(self: *const EvalError, alloc: Allocator) void {
        self.message.deinit(alloc);
        alloc.destroy(self);
    }
};

pub const Environment = struct {
    store: StringHAMT(Object),

    pub fn init(alloc: Allocator) !Environment {
        return .{
            .store = try StringHAMT(Object).init(alloc),
        };
    }

    pub fn get(self: *const Environment, name: []const u8) ?Object {
        return self.store.get(name);
    }

    pub fn inserted(self: *const Environment, name: []const u8, value: Object) Allocator.Error!Environment {
        return .{ .store = try self.store.inserted(name, value, struct {
            fn valEql(a: Object, b: Object) bool {
                return a.eql(b);
            }
        }.valEql) };
    }

    pub fn insert(self: *Environment, name: []const u8, value: Object) Allocator.Error!void {
        try self.store.insert(name, value, struct {
            fn valEql(a: Object, b: Object) bool {
                return a.eql(b);
            }
        }.valEql);
    }

    pub fn deinit(self: Environment) void {
        const Closure = struct {
            alloc: Allocator,

            fn cleanup(ctx: @This(), name: []const u8, value: Object) void {
                _ = name;
                value.deinit(ctx.alloc);
            }
        };
        self.store.deinitCleanup(
            Closure{ .alloc = self.store.allocator },
            Closure.cleanup,
        );
    }

    pub fn clone(self: *const Environment) Allocator.Error!Environment {
        return .{ .store = try self.store.clone() };
    }
};
