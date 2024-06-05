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

pub fn executeStatement(stmt: Ast.Statement, alloc: Allocator) !Object {
    switch (stmt) {
        .expr => |expr| return try eval(expr, alloc),
        else => @panic("Unimplemented"),
    }
}

pub fn eval(expr: Ast.Expression, alloc: Allocator) !Object {
    switch (expr) {
        .int => |value| {
            const obj = try alloc.create(Integer);
            obj.* = .{ .value = value };
            return obj.object();
        },
        .bool => |value| return if (value) Boolean.TRUE else Boolean.FALSE,
        else => @panic("Unimplemented"),
    }
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
        testIntegerObject(evaluated, case.expected, alloc) catch |err| {
            std.debug.print("[case {d}] {s}:\n", .{ i, case.input });
            return err;
        };
    }
}

fn testIntegerObject(obj: Object, expected: u64, alloc: Allocator) !void {
    const object_type = obj.objectType();
    testing.expectEqual(.integer, object_type) catch |err| {
        std.debug.print("Object is not integer. Got .{s} ({s})", .{
            @tagName(object_type),
            (try obj.inspect(alloc)).value(),
        });
        return err;
    };
    const int: *const Integer = @ptrCast(@alignCast(obj.ptr));
    try testing.expectEqual(expected, int.value);
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

fn testBooleanObject(obj: Object, expected: bool, alloc: Allocator) !void {
    const object_type = obj.objectType();
    testing.expectEqual(.boolean, object_type) catch |err| {
        std.debug.print("Object is not boolean. Got .{s} ({s})", .{
            @tagName(object_type),
            (try obj.inspect(alloc)).value(),
        });
        return err;
    };
    const int: *const Boolean = @ptrCast(@alignCast(obj.ptr));
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
