const std = @import("std");
const Allocator = std.mem.Allocator;

const Ast = @import("Ast.zig");

const object = @import("object.zig");
const Object = object.Object;
const ObjectType = object.ObjectType;
const Integer = object.Integer;
const Boolean = object.Boolean;
const Null = object.Null;

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
            obj.* = .{ .value = value };
            return obj.object();
        },
        .bool => |value| return if (value) Object.TRUE else Object.FALSE,
        .unary_op => |operation| {
            const operand = try eval(operation.operand.*, alloc);
            return try evalUnaryOp(operation.op, operand, alloc);
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
            obj.* = .{ .sign = int.sign.opposite(), .value = int.value };
            return obj.object();
        },
        else => Object.NULL, // TODO: handle properly
    };
}

const testing = std.testing;

test "eval integer expression" {
    const cases = [_]struct {
        input: []const u8,
        expected: u64,
    }{
        .{ .input = "5;", .expected = 5 },
        .{ .input = "10;", .expected = 10 },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    inline for (cases, 0..) |case, i| {
        const evaluated = try testEval(case.input, alloc);
        testIntegerObject(evaluated, .{ .plus, case.expected }, alloc) catch |err| {
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

test "eval `!` operator" {
    const cases = [_]struct {
        input: []const u8,
        expected: bool,
    }{
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

test "eval `-` operator" {
    const cases = [_]struct {
        input: []const u8,
        expected: struct { Integer.Sign, u64 },
    }{
        .{ .input = "-5;", .expected = .{ .minus, 5 } },
        .{ .input = "-10;", .expected = .{ .minus, 10 } },
        .{ .input = "--5;", .expected = .{ .plus, 5 } },
        .{ .input = "--10;", .expected = .{ .plus, 10 } },
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

fn testIntegerObject(obj: Object, expected: struct { Integer.Sign, u64 }, alloc: Allocator) !void {
    const object_type = obj.objectType();
    testing.expectEqual(.integer, object_type) catch |err| {
        std.debug.print("Object is not integer. Got .{s} ({s})", .{
            @tagName(object_type),
            (try obj.inspect(alloc)).value(),
        });
        return err;
    };
    const int = obj.cast(Integer);
    try testing.expectEqual(expected[0], int.sign);
    try testing.expectEqual(expected[1], int.value);
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
