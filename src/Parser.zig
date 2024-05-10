const std = @import("std");
const Allocator = std.mem.Allocator;
const MultiArrayList = std.MultiArrayList;

const Lexer = @import("Lexter.zig");
const Token = @import("token.zig").Token;
const Ast = @import("Ast.zig");
const Statement = Ast.Statement;
const Expression = Ast.Expression;
const BinOp = Expression.BinOp;
const UnaryOp = Expression.UnaryOp;

lexer: *Lexer,
curr_token: ?Token,
peek_token: ?Token,

const Parser = @This();

pub fn init(lexer: *Lexer) Parser {
    var p = Parser{
        .lexer = lexer,
        .curr_token = null,
        .peek_token = null,
    };

    p.advanceTokens();
    p.advanceTokens();

    return p;
}

fn advanceTokens(self: *Parser) void {
    self.curr_token = self.peek_token;
    self.peek_token = self.lexer.nextToken();
}

pub fn parseProgram(self: *Parser, allocator: Allocator) !Ast {
    var program = MultiArrayList(Ast.Statement){};

    while (try self.parseStmt(allocator)) |statement| : (self.advanceTokens()) {
        try program.append(allocator, statement);
    }

    return Ast{ .program = program };
}

fn parseStmt(self: *Parser, allocator: Allocator) !?Statement {
    return switch (self.curr_token orelse return null) {
        .let => try self.parseLetStmt(allocator),
        .@"return" => try self.parseReturnStmt(allocator),
        else => |tok| b: {
            std.log.err("Unimplemented for token {}\n", .{tok});
            break :b error.Unimplemented;
        },
    };
}

fn parseLetStmt(self: *Parser, allocator: Allocator) !Statement {
    std.debug.assert(self.curr_token != null and self.curr_token.? == .let);

    try self.expectPeek(.ident, error.ExpectedIdent);
    const name = self.curr_token.?.ident;

    try self.expectPeek(.assign, error.ExpectedAssign);

    const value = try self.parseExpr(allocator);

    try self.expectPeek(.semicolon, error.ExpectedSemicolon);

    return Statement{ .let = .{ .name = name, .value = value } };
}

fn parseReturnStmt(self: *Parser, allocator: Allocator) !Statement {
    std.debug.assert(self.curr_token != null and self.curr_token.? == .@"return");

    const value = try self.parseExpr(allocator);

    try self.expectPeek(.semicolon, error.ExpectedSemicolon);

    return Statement{ .@"return" = value };
}

fn parseExpr(self: *Parser, allocator: Allocator) anyerror!Expression {
    self.advanceTokens();
    return switch (self.curr_token orelse return error.UnexpectedEof) {
        .int => |lit| switch (self.peek_token orelse return error.UnexpectedEof) {
            .plus, .minus, .star, .slash, .lt, .gt, .leq, .geq, .eq, .neq => self.parseBinOpExpr(allocator),
            .semicolon => Expression{ .int = try std.fmt.parseInt(u64, lit, 10) },
            else => error.UnexpectedToken,
        },
        .minus, .bang => Expression{ .unary_op = .{
            .op = UnaryOp.fromToken(self.curr_token.?).?,
            .operand = blk: {
                const operand = try allocator.create(Expression);
                operand.* = try self.parseExpr(allocator);
                break :blk operand;
            },
        } },
        // .lparen => self.parseGroupedExpr(),
        else => error.Unimplemented,
    };
}

fn parseBinOpExpr(self: *Parser, allocator: Allocator) !Expression {
    const left = try allocator.create(Expression);
    left.* = switch (self.curr_token orelse unreachable) {
        .int => |lit| Expression{ .int = try std.fmt.parseInt(u64, lit, 10) },
        else => return error.Unimplemented,
    };

    self.advanceTokens();
    const op = BinOp.fromToken(self.curr_token orelse return error.ExpectedBinOp) orelse return error.ExpectedBinOp;

    self.advanceTokens();
    const right = try allocator.create(Expression);
    right.* = try self.parseExpr(allocator);

    return Expression{ .bin_op = .{
        .left = left,
        .op = op,
        .right = right,
    } };
}

fn expectPeek(self: *Parser, expected: std.meta.Tag(Token), err: anytype) @TypeOf(err)!void {
    if (self.peek_token == null or self.peek_token.? != expected) return err;
    self.advanceTokens();
}

const testing = std.testing;

test "let statements" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 696969;
    ;
    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);

    var program = (try parser.parseProgram(testing.allocator)).program;
    defer program.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 3), program.len);

    const cases = [_]struct { []const u8, Expression }{
        .{ "x", .{ .int = 5 } },
        .{ "y", .{ .int = 10 } },
        .{ "foobar", .{ .int = 696969 } },
    };

    const statements = program.slice();
    for (statements.items(.tags), statements.items(.data), cases) |tag, data, case| {
        try testing.expectEqual(.let, tag);
        try testing.expectEqualStrings(case[0], data.let.name);
        try testing.expectEqual(case[1], data.let.value);
    }
}

test "return statement" {
    const input =
        \\return 5;
        \\return 10;
        \\return 696969;
    ;
    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);

    var program = (try parser.parseProgram(testing.allocator)).program;
    defer program.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 3), program.len);

    const cases = [_]Expression{
        .{ .int = 5 },
        .{ .int = 10 },
        .{ .int = 696969 },
    };

    const statements = program.slice();
    for (statements.items(.tags), statements.items(.data), cases) |tag, data, case| {
        try testing.expectEqual(.@"return", tag);
        try testing.expectEqual(case, data.@"return");
    }
}
