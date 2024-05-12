const std = @import("std");
const MultiArrayList = std.MultiArrayList;
const Allocator = std.mem.Allocator;

const Token = @import("token.zig").Token;

program: MultiArrayList(Statement),

const Ast = @This();

pub const Statement = union(enum) {
    let: LetStmt,
    @"return": Expression,
    expr: Expression,

    pub const LetStmt = struct {
        name: []const u8,
        value: Expression,
    };

    pub fn deinit(self: Statement, alloc: Allocator) void {
        switch (self) {
            .let => |stmt| stmt.value.deinit(alloc),
            inline else => |stmt| stmt.deinit(alloc),
        }
    }

    pub fn format(value: Statement, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        return switch (value) {
            .let => |stmt| std.fmt.format(writer, "let {s} = {};", .{ stmt.name, stmt.value }),
            .@"return" => |expr| std.fmt.format(writer, "return {};", .{expr}),
            .expr => |expr| std.fmt.format(writer, "{};", .{expr}),
        };
    }
};

pub const Expression = union(enum) {
    ident: []const u8,
    int: u64,
    unary_op: UnaryOpExpr,
    bin_op: BinOpExpr,
    @"if": IfExpr,

    pub const UnaryOpExpr = struct {
        op: UnaryOp,
        operand: *const Expression,
    };

    pub const BinOpExpr = struct {
        left: *const Expression,
        op: BinOp,
        right: *const Expression,
    };

    pub const IfExpr = struct {
        cond: *const Expression,
        true_case: *const Expression,
        false_case: *const Expression,
    };

    pub const UnaryOp = enum {
        not,
        minus,

        pub fn fromToken(tok: Token) ?UnaryOp {
            return switch (tok) {
                .bang => .not,
                .minus => .minus,
                else => null,
            };
        }

        fn toToken(op: UnaryOp) Token {
            return switch (op) {
                .not => .bang,
                .minus => .minus,
            };
        }

        pub fn format(value: UnaryOp, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            return std.fmt.format(writer, "{s}", .{value.toToken()});
        }
    };

    pub const BinOp = enum {
        add,
        sub,
        mul,
        div,
        lt,
        gt,
        eq,
        neq,
        leq,
        geq,

        pub fn fromToken(tok: Token) ?BinOp {
            return switch (tok) {
                .plus => .add,
                .minus => .sub,
                .star => .mul,
                .slash => .div,
                .lt => .lt,
                .gt => .gt,
                .leq => .leq,
                .geq => .geq,
                .eq => .eq,
                .neq => .neq,
                else => null,
            };
        }

        fn toToken(op: BinOp) Token {
            return switch (op) {
                .add => .plus,
                .sub => .minus,
                .mul => .star,
                .div => .slash,
                .lt => .lt,
                .gt => .gt,
                .leq => .leq,
                .geq => .geq,
                .eq => .eq,
                .neq => .neq,
            };
        }

        pub fn format(value: BinOp, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            return std.fmt.format(writer, "{s}", .{value.toToken()});
        }
    };

    pub fn deinit(self: Expression, alloc: Allocator) void {
        switch (self) {
            .bin_op => |expr| {
                expr.right.deinit(alloc);
                alloc.destroy(expr.right);
                expr.left.deinit(alloc);
                alloc.destroy(expr.left);
            },
            .unary_op => |expr| {
                expr.operand.deinit(alloc);
                alloc.destroy(expr.operand);
            },
            .@"if" => |expr| {
                expr.false_case.deinit(alloc);
                alloc.destroy(expr.false_case);
                expr.true_case.deinit(alloc);
                alloc.destroy(expr.true_case);
                expr.cond.deinit(alloc);
                alloc.destroy(expr.cond);
            },
            else => {},
        }
    }

    pub fn format(value: Expression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        return switch (value) {
            .ident => |lit| std.fmt.format(writer, "{s}", .{lit}),
            .int => |lit| std.fmt.format(writer, "{d}", .{lit}),
            .unary_op => |expr| std.fmt.format(writer, "({}{})", .{ expr.op, expr.operand }),
            .bin_op => |expr| std.fmt.format(writer, "({} {} {})", .{ expr.left, expr.op, expr.right }),
            .@"if" => |expr| std.fmt.format(writer, "if ({}) {{ {} }} else {{ {} }}", .{ expr.cond, expr.true_case, expr.false_case }),
        };
    }
};

pub fn deinit(self: Ast, alloc: Allocator) void {
    var program = self.program;
    const slice = program.slice();
    for (0..slice.len) |i| {
        const stmt = slice.get(i);
        stmt.deinit(alloc);
    }
    program.deinit(alloc);
}

pub fn format(value: Ast, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = fmt;
    _ = options;
    const stmts = value.program.slice();
    if (stmts.len > 0) {
        const stmt = stmts.get(0);
        try std.fmt.format(writer, "{}", .{stmt});
    }
    for (1..stmts.len) |i| {
        const stmt = stmts.get(i);
        try std.fmt.format(writer, "\n{}", .{stmt});
    }
}
