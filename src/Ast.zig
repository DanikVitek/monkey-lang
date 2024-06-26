const std = @import("std");
const ArrayList = std.ArrayListUnmanaged;
const MultiArrayList = std.MultiArrayList;
const Allocator = std.mem.Allocator;

const Token = @import("token.zig").Token;

const Int = @import("object.zig").Int;

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
    expr: Expression,
    @"return": ?Expression,
    @"break": ?Expression,

    pub const LetStmt = struct {
        name: []const u8,
        value: Expression,

        pub inline fn deinit(self: LetStmt, alloc: Allocator) void {
            alloc.free(self.name);
            self.value.deinit(alloc);
        }
    };

    pub fn deinit(self: Statement, alloc: Allocator) void {
        switch (self) {
            inline .@"return", .@"break" => |opt_expr| if (opt_expr) |expr| expr.deinit(alloc),
            inline else => |stmt| stmt.deinit(alloc),
        }
    }

    pub fn format(self: Statement, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        return switch (self) {
            .let => |stmt| writer.print("let {s} = {};", .{ stmt.name, stmt.value }),
            inline .@"return", .@"break" => |opt_expr, tag| if (opt_expr) |expr|
                writer.print("{s} {};", .{ @tagName(tag), expr })
            else
                writer.print("{s};", .{@tagName(tag)}),
            .expr => |expr| {
                try writer.print("{}", .{expr});
                if (self.endsWithSemicolon()) {
                    try writer.writeByte(';');
                }
            },
        };
    }

    pub fn endsWithSemicolon(self: *const Statement) bool {
        return switch (self.*) {
            .expr => |expr| switch (std.meta.activeTag(expr)) {
                .block, .@"if", .func => false,
                else => true,
            },
            else => true,
        };
    }
};

pub const Expression = union(enum) {
    unit: void,
    ident: []const u8,
    int: Int,
    bool: bool,
    unary_op: UnaryOpExpr,
    binary_op: BinaryOpExpr,
    @"if": IfExpr,
    block: BlockExpr,
    func: FnExpr,
    call: CallExpr,

    pub const UnaryOpExpr = struct {
        op: UnaryOp,
        operand: *const Expression,
    };

    pub const BinaryOpExpr = struct {
        left: *const Expression,
        op: BinaryOp,
        right: *const Expression,
    };

    pub const IfExpr = struct {
        cond: *const Expression,
        conseq: BlockExpr,
        alt: ?BlockExpr = null,

        pub fn format(expr: IfExpr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;

            try std.fmt.format(writer, "if ({}) {}", .{ expr.cond, expr.conseq });
            if (expr.alt) |alt| {
                try std.fmt.format(writer, " else {}", .{alt});
            }
        }
    };

    pub const BlockExpr = struct {
        program: MultiArrayList(Statement) = .{},

        pub fn deinit(self: BlockExpr, alloc: Allocator) void {
            var program = self.program;
            const stmts = program.slice();
            for (0..stmts.len) |i| {
                const stmt = stmts.get(i);
                stmt.deinit(alloc);
            }
            program.deinit(alloc);
        }

        pub fn format(self: BlockExpr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;

            const stmts = self.program.slice();
            try writer.writeByte('{');

            if (stmts.len > 0) {
                for (0..stmts.len - 1) |i| {
                    const stmt = stmts.get(i);
                    try writer.print(" {}", .{stmt});
                }
                const last_stmt = stmts.get(stmts.len - 1);
                if (std.meta.activeTag(last_stmt) == .expr) {
                    try writer.print(" {} ", .{last_stmt.expr});
                } else {
                    try writer.print(" {} ", .{last_stmt});
                }
            }
            try writer.writeByte('}');
        }
    };

    pub const FnExpr = struct {
        params: ArrayList([]const u8),
        body: BlockExpr,

        pub fn deinit(self: FnExpr, alloc: Allocator) void {
            var params = self.params;
            for (0..params.items.len) |i| {
                alloc.free(params.items[i]);
            }
            params.deinit(alloc);
            self.body.deinit(alloc);
        }

        pub fn format(self: FnExpr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            try writer.writeAll("fn(");
            if (self.params.items.len > 0) {
                try std.fmt.format(writer, "{s}", .{self.params.items[0]});
                for (self.params.items[1..]) |param| {
                    try std.fmt.format(writer, ", {s}", .{param});
                }
            }
            try std.fmt.format(writer, ") {}", .{self.body});
        }
    };

    pub const CallExpr = struct {
        callee: *const Expression,
        args: MultiArrayList(Expression),

        pub fn format(self: CallExpr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            try std.fmt.format(writer, "{}(", .{self.callee});
            if (self.args.len > 0) {
                const args = self.args.slice();
                try args.get(0).format("", .{}, writer);
                for (1..args.len) |i| {
                    try std.fmt.format(writer, ", {}", .{args.get(i)});
                }
            }
            try writer.writeByte(')');
        }

        pub fn deinit(self: CallExpr, alloc: Allocator) void {
            self.callee.deinit(alloc);
            alloc.destroy(self.callee);
            var args = self.args;
            const slice = args.slice();
            for (0..slice.len) |i| {
                slice.get(i).deinit(alloc);
            }
            args.deinit(alloc);
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

    pub const BinaryOp = enum {
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

        pub fn fromToken(tok: Token) ?BinaryOp {
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

        fn toToken(op: BinaryOp) Token {
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

        pub fn format(value: BinaryOp, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            return std.fmt.format(writer, "{s}", .{value.toToken()});
        }
    };

    pub fn deinit(self: Expression, alloc: Allocator) void {
        switch (self) {
            .ident => |name| alloc.free(name),
            .binary_op => |expr| {
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
            inline .block, .func, .call => |expr| expr.deinit(alloc),
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
            .binary_op => |expr| std.fmt.format(writer, "({} {} {})", .{ expr.left, expr.op, expr.right }),
            inline else => |expr| std.fmt.format(writer, "{}", .{expr}),
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
