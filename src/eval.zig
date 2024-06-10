const std = @import("std");
const Allocator = std.mem.Allocator;

const Ast = @import("Ast.zig");

const Object = @import("object.zig").Object;
const ObjectType = @import("object.zig").ObjectType;
const Integer = @import("object.zig").Integer;
const Boolean = @import("object.zig").Boolean;
const Null = @import("object.zig").Null;
const ReturnValue = @import("object.zig").ReturnValue;

pub fn execute(ast: Ast, alloc: Allocator) !Object {
    const program = ast.program.slice();
    var result: Object = Object.NULL;
    for (0..program.len) |i| {
        const stmt = program.get(i);
        result.deinit(alloc);
        result = try executeStatement(stmt, alloc);
        if (result.objectType() == .return_value) {
            const ret = result.cast(ReturnValue);
            defer alloc.destroy(ret);
            result = ret.value;
            break;
        }
    }
    return result;
}

fn executeStatement(stmt: Ast.Statement, alloc: Allocator) !Object {
    return switch (stmt) {
        .expr => |expr| try eval(expr, alloc),
        .@"return" => |opt_expr| b: {
            const ret = try alloc.create(ReturnValue);
            errdefer alloc.destroy(ret);
            ret.value = if (opt_expr) |expr| try eval(expr, alloc) else Object.NULL;
            break :b ret.object();
        },
        inline else => |_, tag| @panic("Unimplemented (" ++ @tagName(tag) ++ ")"),
    };
}

const EvalError = error{} || Allocator.Error;

fn eval(expr: Ast.Expression, alloc: Allocator) EvalError!Object {
    return switch (expr) {
        .int => |value| b: {
            const obj = try alloc.create(Integer);
            comptime std.debug.assert(std.math.maxInt(u64) <= std.math.maxInt(i65));
            comptime std.debug.assert(std.math.maxInt(u64) <= -@as(comptime_int, std.math.minInt(i65)));
            obj.value = value;
            break :b obj.object();
        },
        .bool => |value| return nativeBoolToBooleanObject(value),
        .unary_op => |operation| b: {
            const operand = try eval(operation.operand.*, alloc);
            break :b try evalUnaryOp(operation.op, operand, alloc);
        },
        .binary_op => |operation| b: {
            const lhs = try eval(operation.left.*, alloc);
            const rhs = try eval(operation.right.*, alloc);
            break :b try evalBinaryOp(lhs, operation.op, rhs, alloc);
        },
        .@"if" => |conditional| try evalIfExpr(conditional, alloc),
        .block => |block| try evalBlockExpr(block, alloc),
        inline else => |_, tag| @panic("Unimplemented (" ++ @tagName(tag) ++ ")"),
    };
}

fn evalUnaryOp(operator: Ast.Expression.UnaryOp, operand: Object, alloc: Allocator) !Object {
    return switch (operator) {
        .not => evalNotOp(operand),
        .minus => try evalMinusOp(operand, alloc),
    };
}

fn evalNotOp(operand: Object) Object {
    return switch (operand.objectType()) {
        .boolean => nativeBoolToBooleanObject(!operand.eql(Object.TRUE)),
        else => Object.NULL, // TODO: handle properly
    };
}

fn evalMinusOp(operand: Object, alloc: Allocator) !Object {
    return switch (operand.objectType()) {
        .integer => {
            const int = operand.cast(Integer);
            const obj = try alloc.create(Integer);
            obj.value = -int.value;
            return obj.object();
        },
        else => Object.NULL, // TODO: handle properly
    };
}

fn evalBinaryOp(lhs: Object, operator: Ast.Expression.BinaryOp, rhs: Object, alloc: Allocator) !Object {
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
        => try evalIntegerBinaryOp(lhs, operator, rhs, alloc),
        .eq, .neq => try evalEqualityOp(lhs, operator, rhs),
    };
}

fn evalIntegerBinaryOp(lhs: Object, operator: Ast.Expression.BinaryOp, rhs: Object, alloc: Allocator) !Object {
    if (lhs.objectType() != .integer or rhs.objectType() != .integer) {
        return Object.NULL; // TODO: handle properly
    }
    const lhs_int = lhs.cast(Integer);
    const rhs_int = rhs.cast(Integer);

    const value = switch (operator) {
        .add => lhs_int.value +% rhs_int.value,
        .sub => lhs_int.value -% rhs_int.value,
        .mul => lhs_int.value *% rhs_int.value,
        .div => std.math.divTrunc(i63, lhs_int.value, rhs_int.value) catch {
            return Object.NULL; // TODO: handle properly
        },
        .mod => std.math.mod(i63, lhs_int.value, rhs_int.value) catch {
            return Object.NULL; // TODO: handle properly
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

fn evalIfExpr(conditional: Ast.Expression.IfExpr, alloc: Allocator) !Object {
    const cond_obj = try eval(conditional.cond.*, alloc);
    if (cond_obj.objectType() != .boolean) {
        return Object.NULL; // TODO: handle properly
    }
    const cond_bool = cond_obj.cast(Boolean);
    return if (cond_bool.value)
        try evalBlockExpr(conditional.conseq, alloc)
    else if (conditional.alt) |alt|
        try evalBlockExpr(alt, alloc)
    else
        Object.NULL;
}

fn evalBlockExpr(block: Ast.Expression.BlockExpr, alloc: Allocator) !Object {
    const program = block.program.slice();
    var result: Object = Object.NULL;
    for (0..program.len) |i| {
        const stmt = program.get(i);
        result = try executeStatement(stmt, alloc);
        if (result.objectType() == .return_value) break;
    }
    return result;
}

inline fn nativeBoolToBooleanObject(value: bool) Object {
    return if (value) Object.TRUE else Object.FALSE;
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
        const evaluated = try testEval(case.input, alloc);
        testIntegerObject(evaluated, case.expected, alloc) catch |err| {
            std.debug.print("[case {d}] {s}:\n", .{ i, case.input });
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
        const evaluated = try testEval(case.input, alloc);
        testBooleanObject(evaluated, case.expected, alloc) catch |err| {
            std.debug.print("[case {d}] {s}:\n", .{ i, case.input });
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
        .{ .input = "if (1) { 10 }", .expected = Object.NULL },
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
        const evaluated = try testEval(case.input, alloc);
        if (comptime (case.expected.objectType() == .integer)) {
            testIntegerObject(evaluated, case.expected.cast(Integer).value, alloc) catch |err| {
                std.debug.print("[case {d}] {s}:\n", .{ i, case.input });
                return err;
            };
        } else {
            testNullObject(evaluated, alloc) catch |err| {
                std.debug.print("[case {d}] {s}:\n", .{ i, case.input });
                return err;
            };
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
        const evaluated = try testEval(case.input, alloc);
        testIntegerObject(evaluated, case.expected, alloc) catch |err| {
            std.debug.print("[case {d}] {s}:\n", .{ i, case.input });
            return err;
        };
    }

    const bare_return = "9; return; 8;";
    const evaluated = try testEval(bare_return, alloc);
    testNullObject(evaluated, alloc) catch |err| {
        std.debug.print("[case {d}] {s}:\n", .{ cases.len, bare_return });
        return err;
    };
}

fn testIntegerObject(obj: Object, expected: i63, alloc: Allocator) !void {
    const object_type = obj.objectType();
    testing.expectEqual(.integer, object_type) catch |err| {
        std.debug.print("Object is not integer. Got .{s} ({s})", .{
            @tagName(object_type),
            (try obj.inspect(alloc)).value(),
        });
        return err;
    };
    const int = obj.cast(Integer);
    try testing.expectEqual(expected, int.value);
}

fn testBooleanObject(obj: Object, expected: bool, alloc: Allocator) !void {
    const object_type = obj.objectType();
    testing.expectEqual(.boolean, object_type) catch |err| {
        std.debug.print("Object is not boolean. Got .{s} ({s})", .{
            @tagName(object_type),
            (try obj.inspect(alloc)).value(),
        });
        return err;
    };
    const int = obj.cast(Boolean);
    try testing.expectEqual(expected, int.value);
}

fn testNullObject(obj: Object, alloc: Allocator) !void {
    const object_type = obj.objectType();
    testing.expectEqual(.null, object_type) catch |err| {
        std.debug.print("Object is not null. Got .{s} ({s})", .{
            @tagName(object_type),
            (try obj.inspect(alloc)).value(),
        });
        return err;
    };
}

const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

fn testEval(input: []const u8, alloc: Allocator) !Object {
    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);
    const ast = try parser.parseProgram(alloc);

    return try execute(ast, alloc);
}
