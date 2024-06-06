const std = @import("std");
const Allocator = std.mem.Allocator;

const Ast = @import("Ast.zig");

const Object = @import("object.zig").Object;
const ObjectType = @import("object.zig").ObjectType;
const Integer = @import("object.zig").Integer;
const Boolean = @import("object.zig").Boolean;
const Null = @import("object.zig").Null;

pub fn execute(ast: Ast, alloc: Allocator) !Object {
    const program = ast.program.slice();
    var result: Object = undefined;
    for (0..program.len) |i| {
        result = try executeStatement(program.get(i), alloc);
    }
    return result;
}

fn executeStatement(stmt: Ast.Statement, alloc: Allocator) !Object {
    switch (stmt) {
        .expr => |expr| return try eval(expr, alloc),
        else => @panic("Unimplemented"),
    }
}

fn eval(expr: Ast.Expression, alloc: Allocator) !Object {
    switch (expr) {
        .int => |value| {
            const obj = try alloc.create(Integer);
            comptime std.debug.assert(std.math.maxInt(u64) <= std.math.maxInt(i65));
            obj.* = .{ .value = value };
            return obj.object();
        },
        .bool => |value| return if (value) Object.TRUE else Object.FALSE,
        .unary_op => |operation| {
            const operand = try eval(operation.operand.*, alloc);
            return try evalUnaryOp(operation.op, operand, alloc);
        },
        .binary_op => |operation| {
            const lhs = try eval(operation.left.*, alloc);
            const rhs = try eval(operation.right.*, alloc);
            return try evalBinaryOp(lhs, operation.op, rhs, alloc);
        },
        else => @panic("Unimplemented"),
    }
}

fn evalUnaryOp(operator: Ast.Expression.UnaryOp, operand: Object, alloc: Allocator) !Object {
    return switch (operator) {
        .not => evalNotOp(operand),
        .minus => try evalMinusOp(operand, alloc),
    };
}

fn evalNotOp(operand: Object) Object {
    return switch (operand.objectType()) {
        .boolean => if (std.meta.eql(operand, Object.TRUE)) Object.FALSE else Object.TRUE,
        else => Object.NULL, // TODO: handle properly
    };
}

fn evalMinusOp(operand: Object, alloc: Allocator) !Object {
    return switch (operand.objectType()) {
        .integer => {
            const int = operand.cast(Integer);
            const obj = try alloc.create(Integer);
            obj.* = .{ .value = -int.value };
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

    return switch (operator) {
        .add => b: {
            const int = try alloc.create(Integer);
            int.* = .{ .value = lhs_int.value +% rhs_int.value };
            break :b int.object();
        },
        .sub => b: {
            const int = try alloc.create(Integer);
            int.* = .{ .value = lhs_int.value -% rhs_int.value };
            break :b int.object();
        },
        .mul => b: {
            const int = try alloc.create(Integer);
            int.* = .{ .value = lhs_int.value *% rhs_int.value };
            break :b int.object();
        },
        .div => b: {
            const value = std.math.divTrunc(i65, lhs_int.value, rhs_int.value) catch {
                return Object.NULL; // TODO: handle properly
            };
            const int = try alloc.create(Integer);
            int.* = .{ .value = value };
            break :b int.object();
        },
        .mod => b: {
            const value = std.math.mod(i65, lhs_int.value, rhs_int.value) catch {
                return Object.NULL; // TODO: handle properly
            };
            const int = try alloc.create(Integer);
            int.* = .{ .value = value };
            break :b int.object();
        },
        .lt => if (lhs_int.value < rhs_int.value) Object.TRUE else Object.FALSE,
        .gt => if (lhs_int.value > rhs_int.value) Object.TRUE else Object.FALSE,
        .leq => if (lhs_int.value <= rhs_int.value) Object.TRUE else Object.FALSE,
        .geq => if (lhs_int.value >= rhs_int.value) Object.TRUE else Object.FALSE,
        else => unreachable,
    };
}

fn evalEqualityOp(lhs: Object, operator: Ast.Expression.BinaryOp, rhs: Object) !Object {
    return switch (operator) {
        .eq => if (lhs.eql(rhs)) Object.TRUE else Object.FALSE,
        .neq => if (!lhs.eql(rhs)) Object.TRUE else Object.FALSE,
        else => unreachable,
    };
}

const testing = std.testing;

test "eval integer expression" {
    const cases = [_]struct {
        input: []const u8,
        expected: i65,
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

fn testIntegerObject(obj: Object, expected: i65, alloc: Allocator) !void {
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

const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

fn testEval(input: []const u8, alloc: Allocator) !Object {
    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);
    const ast = try parser.parseProgram(alloc);

    return try execute(ast, alloc);
}
