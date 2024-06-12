const std = @import("std");
const Allocator = std.mem.Allocator;

const Ast = @import("Ast.zig");

const Object = @import("object.zig").Object;
const ObjectType = @import("object.zig").ObjectType;
const Integer = @import("object.zig").Integer;
const Boolean = @import("object.zig").Boolean;
const Null = @import("object.zig").Null;
const Function = @import("object.zig").Function;
const ReturnValue = @import("object.zig").ReturnValue;
const BreakValue = @import("object.zig").BreakValue;
const EvalError = @import("object.zig").EvalError;

const Environment = @import("object.zig").Environment;

const String = @import("lib.zig").String;

pub fn execute(alloc: Allocator, ast: Ast, env: *Environment) !Object {
    const program = ast.program.slice();
    var result: Object = Object.NULL;
    for (0..program.len) |i| {
        const stmt = program.get(i);
        result.deinit(alloc);
        result = try executeStatement(alloc, stmt, env, false);
        switch (result.objectType()) {
            .return_value => {
                const ret = result.cast(ReturnValue);
                defer alloc.destroy(ret);
                result = ret.value;
                break;
            },
            .eval_error => break,
            else => {},
        }
    }
    return result;
}

fn executeStatement(alloc: Allocator, stmt: Ast.Statement, env: *Environment, comptime in_block: bool) !Object {
    return switch (stmt) {
        .expr => |expr| try eval(alloc, expr, env),
        .@"return" => |opt_expr| b: {
            const obj = if (opt_expr) |expr| try eval(alloc, expr, env) else Object.NULL;
            if (obj.isError()) break :b obj;
            const ret = try alloc.create(ReturnValue);
            ret.value = obj;
            break :b ret.object();
        },
        .@"break" => |opt_expr| if (in_block) b: {
            const obj = if (opt_expr) |expr| try eval(alloc, expr, env) else Object.NULL;
            if (obj.isError()) break :b obj;
            const brk = try alloc.create(BreakValue);
            brk.value = obj;
            break :b brk.object();
        } else try newError(alloc, "break statement outside of block"),
        .let => |let| b: {
            const val = try eval(alloc, let.value, env);
            if (val.isError()) return val;

            try env.set(alloc, let.name, val);

            break :b Object.NULL;
        },
    };
}

const Error = error{} || Allocator.Error;

fn eval(alloc: Allocator, expr: Ast.Expression, env: *const Environment) Error!Object {
    return switch (expr) {
        .int => |value| (Integer{ .value = value }).object(),
        .bool => |value| nativeBoolToBooleanObject(value),
        .ident => |name| env.get(name) orelse try newErrorFmt(alloc, "identifier not found: {s}", .{name}),
        .unary_op => |operation| b: {
            const operand = try eval(alloc, operation.operand.*, env);
            if (operand.isError()) return operand;
            break :b try evalUnaryOp(alloc, operation.op, operand);
        },
        .binary_op => |operation| b: {
            const lhs = try eval(alloc, operation.left.*, env);
            if (lhs.isError()) return lhs;
            const rhs = try eval(alloc, operation.right.*, env);
            if (rhs.isError()) return rhs;
            break :b try evalBinaryOp(alloc, lhs, operation.op, rhs);
        },
        .@"if" => |conditional| try evalIfExpr(alloc, conditional, env),
        .block => |block| try evalBlockExpr(alloc, block, env),
        .func => |func| {
            const obj = try alloc.create(Function);
            obj.params = func.params.items;
            obj.body = func.body;
            obj.env = env;
            return obj.object();
        },
        inline else => |_, tag| @panic("Unimplemented (" ++ @tagName(tag) ++ ")"),
    };
}

fn evalUnaryOp(alloc: Allocator, operator: Ast.Expression.UnaryOp, operand: Object) !Object {
    return switch (operator) {
        .not => try evalNotOp(alloc, operand),
        .minus => try evalMinusOp(alloc, operand),
    };
}

fn evalNotOp(alloc: Allocator, operand: Object) !Object {
    return switch (operand.objectType()) {
        .boolean => nativeBoolToBooleanObject(!operand.eql(Object.TRUE)),
        else => |object_type| try newErrorFmt(alloc, "type mismatch: !{s}", .{@tagName(object_type)}),
    };
}

fn evalMinusOp(alloc: Allocator, operand: Object) !Object {
    return switch (operand.objectType()) {
        .integer => {
            const int = operand.cast(Integer);
            const obj = try alloc.create(Integer);
            obj.value = -int.value;
            return obj.object();
        },
        else => |object_type| try newErrorFmt(alloc, "type mismatch: -{s}", .{@tagName(object_type)}),
    };
}

fn evalBinaryOp(alloc: Allocator, lhs: Object, operator: Ast.Expression.BinaryOp, rhs: Object) !Object {
    return switch (operator) {
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .lt,
        .gt,
        .leq,
        .geq,
        => try evalIntegerBinaryOp(alloc, lhs, operator, rhs),
        .eq, .neq => try evalEqualityOp(lhs, operator, rhs),
    };
}

fn evalIntegerBinaryOp(alloc: Allocator, lhs: Object, operator: Ast.Expression.BinaryOp, rhs: Object) !Object {
    if (lhs.objectType() != .integer or rhs.objectType() != .integer) {
        return try newErrorFmt(
            alloc,
            "type mismatch: {s} {} {s}",
            .{ @tagName(lhs.objectType()), operator, @tagName(rhs.objectType()) },
        );
    }
    const lhs_int = lhs.cast(Integer);
    const rhs_int = rhs.cast(Integer);

    const value = switch (operator) {
        .add => lhs_int.value +% rhs_int.value,
        .sub => lhs_int.value -% rhs_int.value,
        .mul => lhs_int.value *% rhs_int.value,
        .div => std.math.divTrunc(i63, lhs_int.value, rhs_int.value) catch |err| {
            return try newErrorFmt(alloc, "integer division: {s}", .{@errorName(err)});
        },
        .mod => std.math.mod(i63, lhs_int.value, rhs_int.value) catch |err| {
            return try newErrorFmt(alloc, "integer modulo: {s}", .{@errorName(err)});
        },
        .lt => return nativeBoolToBooleanObject(lhs_int.value < rhs_int.value),
        .gt => return nativeBoolToBooleanObject(lhs_int.value > rhs_int.value),
        .leq => return nativeBoolToBooleanObject(lhs_int.value <= rhs_int.value),
        .geq => return nativeBoolToBooleanObject(lhs_int.value >= rhs_int.value),
        else => unreachable,
    };

    const int = try alloc.create(Integer);
    int.value = value;
    return int.object();
}

fn evalEqualityOp(lhs: Object, operator: Ast.Expression.BinaryOp, rhs: Object) !Object {
    const result: bool = switch (operator) {
        .eq => lhs.eql(rhs),
        .neq => !lhs.eql(rhs),
        else => unreachable,
    };
    return nativeBoolToBooleanObject(result);
}

fn evalIfExpr(alloc: Allocator, conditional: Ast.Expression.IfExpr, env: *const Environment) !Object {
    const cond_obj = try eval(alloc, conditional.cond.*, env);
    if (cond_obj.isError()) return cond_obj;
    if (cond_obj.objectType() != .boolean) {
        return try newErrorFmt(alloc, "type mismatch: {s} in condition", .{@tagName(cond_obj.objectType())});
    }
    const cond_bool = cond_obj.cast(Boolean);
    return if (cond_bool.value)
        try evalBlockExpr(alloc, conditional.conseq, env)
    else if (conditional.alt) |alt|
        try evalBlockExpr(alloc, alt, env)
    else
        Object.NULL;
}

fn evalBlockExpr(alloc: Allocator, block: Ast.Expression.BlockExpr, env: *const Environment) !Object {
    var block_env = try env.clone(alloc); // TODO: replace by persistent map clone
    errdefer { // TODO: implement proper garbage collection. If the block_env is passed into a function, created in this block, and then deleted -- use after free may occur at the place of the function's call. So `errdefer` for now
        var block_iter = block_env.store.iterator();
        while (block_iter.next()) |entry| {
            if (env.store.contains(entry.key_ptr.*)) continue;
            entry.value_ptr.deinit(alloc);
        }
        block_env.store.deinit(alloc);
    }

    const program = block.program.slice();
    var result: Object = Object.NULL;
    for (0..program.len) |i| {
        const stmt = program.get(i);
        result = try executeStatement(alloc, stmt, &block_env, true);
        if (result.objectType() == .return_value or result.objectType() == .eval_error) break;
        switch (result.objectType()) {
            .break_value => {
                const ret = result.cast(BreakValue);
                defer alloc.destroy(ret);
                result = ret.value;
                break;
            },
            .return_value, .eval_error => break,
            else => {},
        }
    }
    return result;
}

inline fn nativeBoolToBooleanObject(value: bool) Object {
    return if (value) Object.TRUE else Object.FALSE;
}

fn newErrorFmt(alloc: Allocator, comptime fmt: []const u8, args: anytype) !Object {
    const message = try std.fmt.allocPrint(alloc, fmt, args);
    const err = try alloc.create(EvalError);
    err.message = .{ .owned = message };
    return err.object();
}

fn newError(alloc: Allocator, message: []const u8) !Object {
    const err = try alloc.create(EvalError);
    err.message = .{ .borrowed = message };
    return err.object();
}

const testing = std.testing;

test "eval integer expression" {
    const cases = [_]struct {
        input: []const u8,
        expected: i63,
    }{
        .{ .input = "5;", .expected = 5 },
        .{ .input = "10;", .expected = 10 },
        .{ .input = "-5;", .expected = -5 },
        .{ .input = "-10;", .expected = -10 },
        .{ .input = "--5;", .expected = 5 },
        .{ .input = "--10;", .expected = 10 },
        .{ .input = "5 + 5 + 5 + 5 - 10;", .expected = 10 },
        .{ .input = "2 * 2 * 2 * 2 * 2;", .expected = 32 },
        .{ .input = "-50 + 100 + -50;", .expected = 0 },
        .{ .input = "5 * 2 + 10;", .expected = 20 },
        .{ .input = "5 + 2 * 10;", .expected = 25 },
        .{ .input = "20 + 2 * -10;", .expected = 0 },
        .{ .input = "50 / 2 * 2 + 10;", .expected = 60 },
        .{ .input = "2 * (5 + 10);", .expected = 30 },
        .{ .input = "3 * 3 * 3 + 10;", .expected = 37 },
        .{ .input = "3 * (3 * 3) + 10;", .expected = 37 },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10;", .expected = 50 },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    inline for (cases, 0..) |case, i| {
        const evaluated = try testEval(alloc, case.input);
        testIntegerObject(alloc, evaluated, case.expected) catch |err| {
            std.debug.print("[case {d}] \"{s}\":\n", .{ i, case.input });
            return err;
        };
    }
}

test "eval bool expression" {
    const cases = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "false;", .expected = false },
        .{ .input = "true;", .expected = true },
        .{ .input = "!false;", .expected = true },
        .{ .input = "!true;", .expected = false },
        .{ .input = "!!false;", .expected = false },
        .{ .input = "!!true;", .expected = true },
        .{ .input = "1 < 2;", .expected = true },
        .{ .input = "1 > 2;", .expected = false },
        .{ .input = "1 < 1;", .expected = false },
        .{ .input = "1 > 1;", .expected = false },
        .{ .input = "1 <= 1;", .expected = true },
        .{ .input = "1 >= 1;", .expected = true },
        .{ .input = "1 <= 2;", .expected = true },
        .{ .input = "1 >= 2;", .expected = false },
        .{ .input = "1 == 1;", .expected = true },
        .{ .input = "1 != 1;", .expected = false },
        .{ .input = "1 == 2;", .expected = false },
        .{ .input = "1 != 2;", .expected = true },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    inline for (cases, 0..) |case, i| {
        const evaluated = try testEval(alloc, case.input);
        testBooleanObject(alloc, evaluated, case.expected) catch |err| {
            std.debug.print("[case {d}] \"{s}\":\n", .{ i, case.input });
            return err;
        };
    }
}

test "eval if/else expression" {
    const cases = comptime [_]struct {
        input: []const u8,
        expected: Object,
    }{
        .{ .input = "if (true) { 10 }", .expected = (Integer{ .value = 10 }).object() },
        .{ .input = "if (false) { 10 }", .expected = Object.NULL },
        .{
            .input = "if (1) { 10 }",
            .expected = (&EvalError{ .message = .{ .borrowed = "type mismatch: integer in condition" } }).object(),
        },
        .{ .input = "if (1 < 2) { 10 }", .expected = (Integer{ .value = 10 }).object() },
        .{ .input = "if (1 > 2) { 10 }", .expected = Object.NULL },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = (Integer{ .value = 20 }).object() },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = (Integer{ .value = 10 }).object() },
        .{ .input = "if (1 < 2) { return; 10 } else { 20 }; 5;", .expected = Object.NULL },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    inline for (cases, 0..) |case, i| {
        const evaluated = try testEval(alloc, case.input);
        switch (comptime case.expected.objectType()) {
            .integer => testIntegerObject(alloc, evaluated, case.expected.cast(Integer).value) catch |err| {
                std.debug.print("[case {d}] \"{s}\":\n", .{ i, case.input });
                return err;
            },
            .eval_error => testErrorObject(alloc, evaluated, case.expected.cast(EvalError).message.value()) catch |err| {
                std.debug.print("[case {d}] \"{s}\":\n", .{ i, case.input });
                return err;
            },
            .null => testNullObject(alloc, evaluated) catch |err| {
                std.debug.print("[case {d}] \"{s}\":\n", .{ i, case.input });
                return err;
            },
            else => unreachable,
        }
    }
}

test "eval return statement" {
    const cases = comptime [_]struct {
        input: []const u8,
        expected: i63,
    }{
        .{ .input = "return 10;", .expected = 10 },
        .{ .input = "return 10; 9;", .expected = 10 },
        .{ .input = "return 2 * 5; 9;", .expected = 10 },
        .{ .input = "8; return 2 * 5; 9;", .expected = 10 },
        .{
            .input =
            \\if (10 > 1) {
            \\    if (10 > 1) {
            \\        return 10;
            \\    }
            \\    return 1;
            \\}
            ,
            .expected = 10,
        },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    inline for (cases, 0..) |case, i| {
        const evaluated = try testEval(alloc, case.input);
        testIntegerObject(alloc, evaluated, case.expected) catch |err| {
            std.debug.print("[case {d}] {s}:\n", .{ i, case.input });
            return err;
        };
    }

    const bare_return = "9; return; 8;";
    const evaluated = try testEval(alloc, bare_return);
    testNullObject(alloc, evaluated) catch |err| {
        std.debug.print("[case {d}] \"{s}\":\n", .{ cases.len, bare_return });
        return err;
    };
}

test "error handling" {
    const cases = comptime [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{
            .input = "5 + true;",
            .expected = "type mismatch: integer + boolean",
        },
        .{
            .input = "5 + true; 5;",
            .expected = "type mismatch: integer + boolean",
        },
        .{
            .input = "-true;",
            .expected = "type mismatch: -boolean",
        },
        .{
            .input = "!0;",
            .expected = "type mismatch: !integer",
        },
        .{
            .input = "true + false;",
            .expected = "type mismatch: boolean + boolean",
        },
        .{
            .input = "5; true + false; 5;",
            .expected = "type mismatch: boolean + boolean",
        },
        .{
            .input = "if (10 > 1) { true + false; }",
            .expected = "type mismatch: boolean + boolean",
        },
        .{
            .input =
            \\if (10 > 1) {
            \\    if (10 > 1) {
            \\        return true + false;
            \\    }
            \\    return 1;
            \\}
            ,
            .expected = "type mismatch: boolean + boolean",
        },
        .{
            .input = "foobar;",
            .expected = "identifier not found: foobar",
        },
        .{
            .input = "{ let foobar = 10; } foobar;",
            .expected = "identifier not found: foobar",
        },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    inline for (cases, 0..) |case, i| {
        const evaluated = try testEval(alloc, case.input);
        testErrorObject(alloc, evaluated, case.expected) catch |err| {
            std.debug.print("[case {d}] \"{s}\":\n", .{ i, case.input });
            return err;
        };
    }
}

test "eval let statements and bindings" {
    const cases = comptime [_]struct {
        input: []const u8,
        expected: i63,
    }{
        .{ .input = "let a = 5; a;", .expected = 5 },
        .{ .input = "let a = 5 * 5; a;", .expected = 25 },
        .{ .input = "let a = 5; let b = a; b;", .expected = 5 },
        .{ .input = "let a = 5; let b = a; let c = a + b + 5; c;", .expected = 15 },
        .{ .input = "let a = 5; let c = {let b = a; a + b + 5}; c;", .expected = 15 },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    inline for (cases, 0..) |case, i| {
        const evaluated = try testEval(alloc, case.input);
        testIntegerObject(alloc, evaluated, case.expected) catch |err| {
            std.debug.print("[case {d}] \"{s}\":\n", .{ i, case.input });
            return err;
        };
    }
}

test "eval function object" {
    const input = "fn(x) { x + 2 };";

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const evaluated = try testEval(alloc, input);

    const obj_type = evaluated.objectType();
    testing.expectEqual(.function, obj_type) catch |err| {
        std.debug.print("Object is not function. Got .{s} ({s})\n", .{
            @tagName(obj_type),
            (try evaluated.inspect(alloc)).value(),
        });
        return err;
    };

    const function: *const Function = evaluated.cast(Function);

    try testing.expectEqual(@as(usize, 1), function.params.len);
    try testing.expectEqualStrings("x", function.params[0]);

    try testing.expectEqualStrings("{ (x + 2) }", try std.fmt.allocPrint(alloc, "{}", .{function.body}));
}

test "eval function application" {
    const cases = comptime [_]struct {
        input: []const u8,
        expected: i63,
    }{
        .{ .input = "let identity = fn(x) { x }; identity(5);", .expected = 5 },
        .{ .input = "let identity = fn(x) { return x; }; identity(5);", .expected = 5 },
        .{ .input = "let double = fn(x) { x * 2 }; double(5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y }; add(5, 5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y }; add(5 + 5, add(5, 5));", .expected = 20 },
        .{ .input = "fn(x) { x }(5);", .expected = 5 },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    inline for (cases, 0..) |case, i| {
        const evaluated = try testEval(alloc, case.input);
        testIntegerObject(alloc, evaluated, case.expected) catch |err| {
            std.debug.print("[case {d}] \"{s}\":\n", .{ i, case.input });
            return err;
        };
    }
}

fn testIntegerObject(alloc: Allocator, obj: Object, expected: i63) !void {
    const object_type = obj.objectType();
    testing.expectEqual(.integer, object_type) catch |err| {
        std.debug.print("Object is not integer. Got .{s} ({s})\n", .{
            @tagName(object_type),
            (try obj.inspect(alloc)).value(),
        });
        return err;
    };
    const int = obj.cast(Integer);
    try testing.expectEqual(expected, int.value);
}

fn testBooleanObject(alloc: Allocator, obj: Object, expected: bool) !void {
    const object_type = obj.objectType();
    testing.expectEqual(.boolean, object_type) catch |err| {
        std.debug.print("Object is not boolean. Got .{s} ({s})\n", .{
            @tagName(object_type),
            (try obj.inspect(alloc)).value(),
        });
        return err;
    };
    const int = obj.cast(Boolean);
    try testing.expectEqual(expected, int.value);
}

fn testNullObject(alloc: Allocator, obj: Object) !void {
    const object_type = obj.objectType();
    testing.expectEqual(.null, object_type) catch |err| {
        std.debug.print("Object is not null. Got .{s} ({s})\n", .{
            @tagName(object_type),
            (try obj.inspect(alloc)).value(),
        });
        return err;
    };
}

fn testErrorObject(alloc: Allocator, obj: Object, expected: []const u8) !void {
    const object_type = obj.objectType();
    testing.expectEqual(.eval_error, object_type) catch |err| {
        std.debug.print("Object is not eval_error. Got .{s} ({s})\n", .{
            @tagName(object_type),
            (try obj.inspect(alloc)).value(),
        });
        return err;
    };
    const eval_err = obj.cast(EvalError);
    try testing.expectEqualStrings(expected, eval_err.message.value());
}

const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

fn testEval(alloc: Allocator, input: []const u8) !Object {
    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);
    const ast = try parser.parseProgram(alloc);

    var env = Environment{};

    return try execute(alloc, ast, &env);
}
