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

pub fn parseProgram(self: *Parser, alloc: Allocator) !Ast {
    var program = MultiArrayList(Ast.Statement){};

    while (try self.parseStmt(alloc)) |statement| : (self.advanceTokens()) {
        try program.append(alloc, statement);
    }

    return Ast{ .program = program };
}

fn parseStmt(self: *Parser, alloc: Allocator) !?Statement {
    while (self.currTokenIs(.semicolon)) {
        self.advanceTokens();
    }
    return switch (self.curr_token orelse return null) {
        .let => try self.parseLetStmt(alloc),
        .@"return" => try self.parseReturnStmt(alloc),
        else => try self.parseExprStmt(alloc),
    };
}

fn parseLetStmt(self: *Parser, alloc: Allocator) !Statement {
    std.debug.assert(self.curr_token != null and self.curr_token.? == .let);

    try self.expectPeek(.ident, error.ExpectedIdent);
    const name = self.curr_token.?.ident;

    try self.expectPeek(.assign, error.ExpectedAssign);

    self.advanceTokens();
    const value = try PrattParser.parseExpr(self, .lowest, alloc);

    try self.expectPeek(.semicolon, error.ExpectedSemicolon);

    return Statement{ .let = .{ .name = name, .value = value } };
}

fn parseReturnStmt(self: *Parser, alloc: Allocator) !Statement {
    std.debug.assert(self.curr_token != null and self.curr_token.? == .@"return");

    self.advanceTokens();
    const value = try PrattParser.parseExpr(self, .lowest, alloc);

    try self.expectPeek(.semicolon, error.ExpectedSemicolon);

    return Statement{ .@"return" = value };
}

fn parseExprStmt(self: *Parser, alloc: Allocator) !Statement {
    const expr = try PrattParser.parseExpr(self, .lowest, alloc);

    try self.expectPeek(.semicolon, error.ExpectedSemicolon);

    return Statement{ .expr = expr };
}

fn expectPeek(self: *Parser, expected: std.meta.Tag(Token), err: anytype) @TypeOf(err)!void {
    if (!self.peekTokenIs(expected)) return err;
    self.advanceTokens();
}

fn currTokenIs(self: *const Parser, tok: std.meta.Tag(Token)) bool {
    return self.curr_token != null and self.curr_token.? == tok;
}

fn peekTokenIs(self: *const Parser, tok: std.meta.Tag(Token)) bool {
    return self.peek_token != null and self.peek_token.? == tok;
}

const PrattParser = struct {
    pub const Precedence = enum(u3) {
        lowest = 1,
        equals,
        @"less/greater",
        sum,
        product,
        prefix,
        call,

        fn fromInfixToken(tok: Token) ?Precedence {
            return switch (tok) {
                .eq, .neq => .equals,
                .lt, .gt, .leq, .geq => .@"less/greater",
                .plus, .minus => .sum,
                .star, .slash => .product,
                else => null,
            };
        }
    };

    pub const ParseExprError = error{
        UnexpectedEof,
        UnexpectedToken,
        Unimplemented,
    } || std.fmt.ParseIntError || Allocator.Error;

    pub fn parseExpr(p: *Parser, precedence: Precedence, alloc: Allocator) ParseExprError!Expression {
        var left_expr = try prefix(p, alloc);
        errdefer left_expr.deinit(alloc);

        while (!p.peekTokenIs(.semicolon) and @intFromEnum(precedence) < @intFromEnum(peekPrecedence(p))) {
            if (!isInfix(p.peek_token.?)) return left_expr;

            p.advanceTokens();

            left_expr = try parseInfixExpr(p, left_expr, alloc);
        }

        return left_expr;
    }

    fn prefix(p: *Parser, alloc: Allocator) !Expression {
        const curr_tok = p.curr_token orelse return error.UnexpectedEof;
        return switch (curr_tok) {
            .ident => parseIdent(p),
            .int => parseInt(p),
            .bang, .minus => parsePrefixExpr(p, alloc),
            else => b: {
                std.log.err("No prefix parse function for '{s}'", .{curr_tok});
                break :b error.UnexpectedToken;
            },
        };
    }

    inline fn parseIdent(p: *Parser) Expression {
        std.debug.assert(p.curr_token != null and p.curr_token.? == .ident);
        return Expression{ .ident = p.curr_token.?.ident };
    }

    inline fn parseInt(p: *Parser) !Expression {
        std.debug.assert(p.curr_token != null and p.curr_token.? == .int);
        return Expression{ .int = try std.fmt.parseInt(u64, p.curr_token.?.int, 10) };
    }

    fn parsePrefixExpr(p: *Parser, alloc: Allocator) !Expression {
        std.debug.assert(p.curr_token != null and (p.curr_token.? == .minus or p.curr_token.? == .bang));

        const op = UnaryOp.fromToken(p.curr_token.?).?;
        p.advanceTokens();

        const right = try alloc.create(Expression);
        errdefer alloc.destroy(right);
        right.* = try parseExpr(p, .prefix, alloc);

        return Expression{ .unary_op = .{ .op = op, .operand = right } };
    }

    fn isInfix(tok: Token) bool {
        return switch (tok) {
            .plus, .minus, .star, .slash, .eq, .neq, .lt, .gt, .leq, .geq => true,
            else => false,
        };
    }

    fn parseInfixExpr(p: *Parser, left: Expression, alloc: Allocator) !Expression {
        const op = BinOp.fromToken(p.curr_token.?).?;

        const precedence = currPrecedence(p);
        p.advanceTokens();

        const right = try alloc.create(Expression);
        errdefer alloc.destroy(right);
        right.* = try parseExpr(p, precedence, alloc);

        const boxed_left = try alloc.create(Expression);
        boxed_left.* = left;

        return Expression{ .bin_op = .{
            .left = boxed_left,
            .op = op,
            .right = right,
        } };
    }

    fn currPrecedence(p: *const Parser) Precedence {
        return Precedence.fromInfixToken(p.curr_token orelse return .lowest) orelse .lowest;
    }

    fn peekPrecedence(p: *const Parser) Precedence {
        return Precedence.fromInfixToken(p.peek_token orelse return .lowest) orelse .lowest;
    }
};

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

test "identifier expression" {
    const input = "foobar;";

    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);

    var program = (try parser.parseProgram(testing.allocator)).program;
    defer program.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 1), program.len);

    const cases = [_]Statement{
        .{ .expr = .{ .ident = "foobar" } },
    };

    const statements = program.slice();
    for (0..statements.len, cases) |i, case| {
        const stmt = statements.get(i);
        try testing.expectEqualDeep(case, stmt);
    }
}

test "integer expression" {
    const input = "5;";

    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);

    var program = (try parser.parseProgram(testing.allocator)).program;
    defer program.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 1), program.len);

    const cases = [_]Statement{
        .{ .expr = .{ .int = 5 } },
    };

    const statements = program.slice();
    for (0..statements.len, cases) |i, case| {
        const stmt = statements.get(i);
        try testing.expectEqualDeep(case, stmt);
    }
}

test "prefix expression" {
    const cases = [_]struct {
        input: []const u8,
        op: UnaryOp,
        operand: Expression,
    }{ .{
        .input = "!5;",
        .op = .not,
        .operand = Expression{ .int = 5 },
    }, .{
        .input = "-15;",
        .op = .minus,
        .operand = Expression{ .int = 15 },
    } };

    for (cases) |case| {
        var lexer = try Lexer.init(case.input);
        var parser = Parser.init(&lexer);

        const ast = try parser.parseProgram(testing.allocator);
        defer ast.deinit(testing.allocator);
        const program = ast.program;

        try testing.expectEqual(@as(usize, 1), program.len);

        const stmt = program.get(0);
        try testing.expectEqualDeep(
            Statement{ .expr = .{ .unary_op = .{
                .op = case.op,
                .operand = &case.operand,
            } } },
            stmt,
        );
    }
}

// const pretty = @import("pretty");

test "infix expression" {
    const cases = comptime [_]struct {
        input: []const u8,
        left: Expression,
        op: BinOp,
        right: Expression,
    }{ .{
        .input = "5 + 6;",
        .left = .{ .int = 5 },
        .op = .add,
        .right = .{ .int = 6 },
    }, .{
        .input = "5 - 6;",
        .left = .{ .int = 5 },
        .op = .sub,
        .right = .{ .int = 6 },
    }, .{
        .input = "5 * 6;",
        .left = .{ .int = 5 },
        .op = .mul,
        .right = .{ .int = 6 },
    }, .{
        .input = "5 / 6;",
        .left = .{ .int = 5 },
        .op = .div,
        .right = .{ .int = 6 },
    }, .{
        .input = "5 > 6;",
        .left = .{ .int = 5 },
        .op = .gt,
        .right = .{ .int = 6 },
    }, .{
        .input = "5 < 6;",
        .left = .{ .int = 5 },
        .op = .lt,
        .right = .{ .int = 6 },
    }, .{
        .input = "5 == 6;",
        .left = .{ .int = 5 },
        .op = .eq,
        .right = .{ .int = 6 },
    }, .{
        .input = "5 != 6;",
        .left = .{ .int = 5 },
        .op = .neq,
        .right = .{ .int = 6 },
    }, .{
        .input = "5 >= 6;",
        .left = .{ .int = 5 },
        .op = .geq,
        .right = .{ .int = 6 },
    }, .{
        .input = "5 <= 6;",
        .left = .{ .int = 5 },
        .op = .leq,
        .right = .{ .int = 6 },
    } };

    inline for (cases) |case| {
        var lexer = try Lexer.init(case.input);
        var parser = Parser.init(&lexer);

        const ast = try parser.parseProgram(testing.allocator);
        defer ast.deinit(testing.allocator);
        const program = ast.program;

        try testing.expectEqual(@as(usize, 1), program.len);

        const stmt = program.get(0);
        try testing.expectEqualDeep(
            Statement{ .expr = .{ .bin_op = .{
                .left = &case.left,
                .op = case.op,
                .right = &case.right,
            } } },
            stmt,
        );
    }
}

test "operator precedence parsing" {
    const cases = comptime [_]struct { []const u8, []const u8 }{ .{
        "-a * b",
        "((-a) * b)",
    }, .{
        "!-a",
        "(!(-a))",
    }, .{
        "a + b + c",
        "((a + b) + c)",
    }, .{
        "a + b - c",
        "((a + b) - c)",
    }, .{
        "a * b * c",
        "((a * b) * c)",
    }, .{
        "a * b / c",
        "((a * b) / c)",
    }, .{
        "a + b / c",
        "(a + (b / c))",
    }, .{
        "a + b * c + d / e - f",
        "(((a + (b * c)) + (d / e)) - f)",
    }, .{
        "3 + 4; -5 * 5",
        "(3 + 4)((-5) * 5)",
    }, .{
        "5 > 4 == 3 < 4",
        "((5 > 4) == (3 < 4))",
    }, .{
        "5 < 4 != 3 > 4",
        "((5 < 4) != (3 > 4))",
    }, .{
        "3 + 4 * 5 == 3 * 1 + 4 * 5",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
    } };

    inline for (cases) |case| {
        var lexer = try Lexer.init(case[0] ++ ";");
        var parser = Parser.init(&lexer);

        const ast = try parser.parseProgram(testing.allocator);
        defer ast.deinit(testing.allocator);
        const program = ast.program;

        try testing.expectEqual(std.mem.count(u8, case[0], ";") + 1, program.len);
        // catch |err| {
        //     std.log.err("case: \"{s}\"", .{case[0]});
        //     const slice = program.slice();
        //     for (0..slice.len) |i| {
        //         std.log.err("{}\n", .{slice.get(i)});
        //     }
        //     return err;
        // };

        var concatenated = try std.ArrayList(u8).initCapacity(testing.allocator, case[1].len);
        defer concatenated.deinit();

        const slice = program.slice();
        for (0..slice.len) |i| {
            try std.fmt.format(concatenated.writer(), "{}", .{program.get(i).expr});
        }

        try testing.expectEqualStrings(case[1], concatenated.items);
    }
}
