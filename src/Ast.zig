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
};

pub const Expression = union(enum) {
    ident: []const u8,
    int: u64,
    bin_op: BinOpExpr,
    unary_op: UnaryOpExpr,
    @"if": IfExpr,

    pub const BinOpExpr = struct {
        left: *const Expression,
        op: BinOp,
        right: *const Expression,
    };

    pub const UnaryOpExpr = struct {
        op: UnaryOp,
        operand: *const Expression,
    };

    pub const IfExpr = struct {
        cond: *const Expression,
        true_case: *const Expression,
        false_case: *const Expression,
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
    };

    pub const UnaryOp = enum {
        not,
        minus,

        pub fn fromToken(tok: Token) ?UnaryOp {
            return switch (tok) {
                .minus => .minus,
                .bang => .not,
                else => null,
            };
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
