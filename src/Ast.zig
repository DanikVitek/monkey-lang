const std = @import("std");
const MultiArrayList = std.MultiArrayList;

const Token = @import("token.zig").Token;

program: MultiArrayList(Statement),

const Ast = @This();

pub const Statement = union(enum) {
    let: struct {
        name: []const u8,
        value: Expression,
    },
    @"return": Expression,
};

pub const Expression = union(enum) {
    ident: []const u8,
    int: u64,
    bin_op: struct {
        left: *const Expression,
        op: BinOp,
        right: *const Expression,
    },
    unary_op: struct {
        op: UnaryOp,
        operand: *const Expression,
    },

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
};
