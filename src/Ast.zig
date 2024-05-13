const std = @import("std");
const MultiArrayList = std.MultiArrayList;
const Allocator = std.mem.Allocator;

const Token = @import("token.zig").Token;

program: MultiArrayList(Statement),

const Ast = @This();

// pub fn Spanned(comptime T: type) type {
//     return struct {
//         node: T,
//         span: Span,
//     };
// }

// pub const Span = struct {
//     start: Pos,
//     end: Pos,
// };

// pub const Pos = struct {
//     line: u32 = 1,
//     col: u32 = 1,
//     offset: usize = 0,
// };

pub const Statement = union(enum) {
    let: LetStmt,
    @"return": Expression,
    expr: Expression,

    pub const LetStmt = struct {
        name: []const u8,
        value: Expression,

        pub inline fn deinit(self: LetStmt, alloc: Allocator) void {
            self.value.deinit(alloc);
        }
    };

    pub fn deinit(self: Statement, alloc: Allocator) void {
        switch (self) {
            inline else => |stmt| stmt.deinit(alloc),
        }
    }

    pub fn format(value: Statement, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        return switch (value) {
            .let => |stmt| std.fmt.format(writer, "let {s} = {};", .{ stmt.name, stmt.value }),
            .@"return" => |expr| std.fmt.format(writer, "return {};", .{expr}),
            .expr => |expr| {
                try std.fmt.format(writer, "{}", .{expr});
                if (std.meta.activeTag(expr) != .block) {
                    try writer.writeByte(';');
                }
            },
        };
    }
};

pub const Expression = union(enum) {
    unit: void,
    ident: []const u8,
    int: u64,
    bool: bool,
    unary_op: UnaryOpExpr,
    bin_op: BinOpExpr,
    @"if": IfExpr,
    block: BlockExpr,

    statement: *const Statement,

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
        conseq: BlockExpr,
        alt: ?BlockExpr = null,

        pub fn format(expr: IfExpr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;

            try std.fmt.format(writer, "if ({}) {{ {} }}", .{ expr.cond, expr.conseq });
            if (expr.alt) |alt| {
                try std.fmt.format(writer, " else {{ {} }}", .{alt});
            }
        }
    };

    pub const BlockExpr = struct {
        body: MultiArrayList(Statement) = .{},
        @"return": ?*const Expression = null,

        pub fn deinit(self: BlockExpr, alloc: Allocator) void {
            var program = self.body;
            const stmts = program.slice();
            for (0..stmts.len) |i| {
                const stmt = stmts.get(i);
                stmt.deinit(alloc);
            }
            program.deinit(alloc);
            if (self.@"return") |expr| {
                expr.deinit(alloc);
                alloc.destroy(expr);
            }
        }

        pub fn format(self: BlockExpr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;

            const stmts = self.body.slice();
            if (stmts.len > 0) {
                try std.fmt.format(writer, "{}", .{stmts.get(0)});
                for (1..stmts.len) |i| {
                    const stmt = stmts.get(i);
                    try std.fmt.format(writer, " {}", .{stmt});
                }
            }

            if (self.@"return") |expr| {
                if (stmts.len > 0) {
                    try writer.writeByte(' ');
                }
                try std.fmt.format(writer, "{}", .{expr});
            }
        }
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
        mod,
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
                .percent => .mod,
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
                .mod => .percent,
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
                expr.left.deinit(alloc);
                alloc.destroy(expr.left);

                expr.right.deinit(alloc);
                alloc.destroy(expr.right);
            },
            .unary_op => |expr| {
                expr.operand.deinit(alloc);
                alloc.destroy(expr.operand);
            },
            .@"if" => |expr| {
                expr.cond.deinit(alloc);
                alloc.destroy(expr.cond);

                expr.conseq.deinit(alloc);

                if (expr.alt) |alt| {
                    alt.deinit(alloc);
                }
            },
            .block => |expr| expr.deinit(alloc),
            .statement => |stmt| stmt.deinit(alloc),
            else => {},
        }
    }

    pub fn format(value: Expression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        return switch (value) {
            .unit => writer.writeAll("()"),
            .ident => |name| std.fmt.format(writer, "{s}", .{name}),
            .int => |val| std.fmt.format(writer, "{d}", .{val}),
            .bool => |val| std.fmt.format(writer, "{}", .{val}),
            .unary_op => |expr| std.fmt.format(writer, "({}{})", .{ expr.op, expr.operand }),
            .bin_op => |expr| std.fmt.format(writer, "({} {} {})", .{ expr.left, expr.op, expr.right }),
            .@"if" => |expr| expr.format("", .{}, writer),
            .block => |block| std.fmt.format(writer, "{{ {} }}", .{block}),
            .statement => |stmt| stmt.format("", .{}, writer),
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
        try std.fmt.format(writer, "\n{}", .{stmts.get(i)});
    }
}
