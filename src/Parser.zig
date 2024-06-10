const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayListUnmanaged;
const MultiArrayList = std.MultiArrayList;

const Lexer = @import("Lexer.zig");
const Token = @import("token.zig").Token;
const Ast = @import("Ast.zig");
const Statement = Ast.Statement;
const Expression = Ast.Expression;
const BinaryOp = Expression.BinaryOp;
const UnaryOp = Expression.UnaryOp;
const IfExpr = Expression.IfExpr;
const BlockExpr = Expression.BlockExpr;

lexer: *Lexer,
curr_token: ?Token = null,
peek_token: ?Token = null,

const Parser = @This();

pub fn init(lexer: *Lexer) Parser {
    var p = Parser{ .lexer = lexer };

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
    errdefer (Ast{ .program = program }).deinit(alloc);

    while (try self.parseStmt(alloc)) |statement| : (self.advanceTokens()) {
        errdefer statement.deinit(alloc);
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
    std.debug.assert(self.curr_token.? == .let);

    try self.expectPeekAdvance(.ident, error.ExpectedIdent);
    const name = self.curr_token.?.ident;

    try self.expectPeekAdvance(.assign, error.ExpectedAssign);

    self.advanceTokens();
    const value = try PrattParser.parseExpr(alloc, self, .lowest);
    errdefer value.deinit(alloc);

    try self.expectPeekAdvance(.semicolon, error.ExpectedSemicolon);

    return Statement{ .let = .{ .name = name, .value = value } };
}

fn parseReturnStmt(self: *Parser, alloc: Allocator) !Statement {
    std.debug.assert(self.curr_token.? == .@"return");

    const expr: ?Expression = if (!self.peekTokenIs(.semicolon)) b: {
        self.advanceTokens();
        break :b try PrattParser.parseExpr(alloc, self, .lowest);
    } else null;
    errdefer if (expr) |e| e.deinit(alloc);

    try self.expectPeekAdvance(.semicolon, error.ExpectedSemicolon);

    return Statement{ .@"return" = expr };
}

fn parseExprStmt(self: *Parser, alloc: Allocator) !Statement {
    const expr = try PrattParser.parseExpr(alloc, self, .lowest);
    errdefer expr.deinit(alloc);

    if (!self.currTokenIs(.rbrace)) {
        try self.expectPeekAdvance(.semicolon, error.ExpectedSemicolon);
    } else if (self.peekTokenIs(.semicolon)) {
        self.advanceTokens();
    }

    return Statement{ .expr = expr };
}

fn expectPeekAdvance(self: *Parser, expected: std.meta.Tag(Token), err: anytype) @TypeOf(err)!void {
    if (!self.peekTokenIs(expected)) {
        // std.log.err("Expected '.{s}', but got '{?}'", .{ @tagName(expected), self.peek_token });
        return err;
    }
    self.advanceTokens();
}

inline fn currTokenIs(self: *const Parser, tok: std.meta.Tag(Token)) bool {
    return self.curr_token != null and self.curr_token.? == tok;
}

inline fn peekTokenIs(self: *const Parser, tok: std.meta.Tag(Token)) bool {
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
                .star, .slash, .percent => .product,
                .lparen => .call,
                else => null,
            };
        }
    };

    pub const ParseExprError = error{
        UnexpectedEof,
        UnexpectedToken,
        Unimplemented,
        ExpectedLParen,
        ExpectedRParen,
        ExpectedLBrace,
        ExpectedRBrace,
        ExpectedIdent,
        ExpectedAssign,
        ExpectedSemicolon,
    } || std.fmt.ParseIntError || Allocator.Error;

    pub fn parseExpr(alloc: Allocator, p: *Parser, precedence: Precedence) ParseExprError!Expression {
        var left_expr = try prefix(alloc, p);
        errdefer left_expr.deinit(alloc);

        while (!p.peekTokenIs(.semicolon) and @intFromEnum(precedence) < @intFromEnum(peekPrecedence(p))) {
            if (!isInfix(p.peek_token.?)) return left_expr;

            p.advanceTokens();

            left_expr = switch (p.curr_token.?) {
                .lparen => try parseCallExpr(alloc, p, left_expr),
                else => try parseInfixExpr(alloc, p, left_expr),
            };
        }

        return left_expr;
    }

    fn prefix(alloc: Allocator, p: *Parser) !Expression {
        const curr_tok = p.curr_token orelse return error.UnexpectedEof;
        return switch (curr_tok) {
            .false, .true => parseBool(p),
            .ident => parseIdent(p),
            .int => parseInt(p),
            .bang, .minus => parsePrefixExpr(alloc, p),
            .lparen => parseGroupExpr(alloc, p),
            .lbrace => b: {
                if (p.peekTokenIs(.rbrace)) {
                    p.advanceTokens();
                    break :b Expression.unit;
                }
                break :b .{ .block = try parseBlockExpr(alloc, p) };
            },
            .@"if" => parseIfExpr(alloc, p),
            .func => parseFuncExpr(alloc, p),
            else => b: {
                std.log.err("No prefix parse function for '{s}'", .{curr_tok});
                break :b error.UnexpectedToken;
            },
        };
    }

    inline fn parseBool(p: *const Parser) Expression {
        return Expression{ .bool = switch (p.curr_token.?) {
            .false => false,
            .true => true,
            else => unreachable,
        } };
    }

    inline fn parseIdent(p: *const Parser) Expression {
        return Expression{ .ident = p.curr_token.?.ident };
    }

    inline fn parseInt(p: *const Parser) !Expression {
        return Expression{ .int = try std.fmt.parseInt(i63, p.curr_token.?.int, 10) };
    }

    fn parsePrefixExpr(alloc: Allocator, p: *Parser) !Expression {
        const op = UnaryOp.fromToken(p.curr_token.?).?;
        p.advanceTokens();

        const right = try alloc.create(Expression);
        errdefer alloc.destroy(right);
        right.* = try parseExpr(alloc, p, .prefix);

        return Expression{ .unary_op = .{ .op = op, .operand = right } };
    }

    fn isInfix(tok: Token) bool {
        return tok == .lparen or BinaryOp.fromToken(tok) != null;
    }

    fn parseInfixExpr(alloc: Allocator, p: *Parser, left: Expression) !Expression {
        const op = BinaryOp.fromToken(p.curr_token.?).?;

        const precedence = currPrecedence(p);
        p.advanceTokens();

        const right = try alloc.create(Expression);
        errdefer alloc.destroy(right);
        right.* = try parseExpr(alloc, p, precedence);

        const boxed_left = try alloc.create(Expression);
        boxed_left.* = left;

        return Expression{ .binary_op = .{
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

    fn parseGroupExpr(alloc: Allocator, p: *Parser) !Expression {
        std.debug.assert(p.curr_token.? == .lparen);
        p.advanceTokens();

        if (p.currTokenIs(.rparen)) {
            return Expression.unit;
        }

        const expr = try parseExpr(alloc, p, .lowest);
        errdefer expr.deinit(alloc);

        try p.expectPeekAdvance(.rparen, error.ExpectedRParen);

        return expr;
    }

    fn parseIfExpr(alloc: Allocator, p: *Parser) !Expression {
        std.debug.assert(p.curr_token.? == .@"if");

        try p.expectPeekAdvance(.lparen, error.ExpectedLParen);

        p.advanceTokens();
        const cond = try parseExpr(alloc, p, .lowest);
        errdefer cond.deinit(alloc);

        try p.expectPeekAdvance(.rparen, error.ExpectedRParen);
        try p.expectPeekAdvance(.lbrace, error.ExpectedLBrace);

        const conseq = try parseBlockExpr(alloc, p);
        errdefer conseq.deinit(alloc);

        const alt: ?BlockExpr = if (p.peekTokenIs(.@"else")) b: {
            p.advanceTokens();

            try p.expectPeekAdvance(.lbrace, error.ExpectedLBrace);

            break :b try parseBlockExpr(alloc, p);
        } else null;

        const boxed_cond = try alloc.create(Expression);
        boxed_cond.* = cond;

        return Expression{ .@"if" = IfExpr{
            .cond = boxed_cond,
            .conseq = conseq,
            .alt = alt,
        } };
    }

    fn parseBlockExpr(alloc: Allocator, p: *Parser) !BlockExpr {
        std.debug.assert(p.curr_token.? == .lbrace);

        var program: MultiArrayList(Statement) = .{};
        errdefer (BlockExpr{ .program = program }).deinit(alloc);

        p.advanceTokens();

        var prev_parser = p.*;
        var prev_lexer = prev_parser.lexer.*;

        var not_stmt = false;
        while (p.curr_token != null and !p.currTokenIs(.rbrace)) {
            if (p.parseStmt(alloc) catch |err| switch (err) {
                error.ExpectedSemicolon => {
                    not_stmt = true;
                    break;
                },
                else => return err,
            }) |stmt| {
                errdefer stmt.deinit(alloc);
                try program.append(alloc, stmt);
            }
            p.advanceTokens();
            prev_parser = p.*;
            prev_lexer = prev_parser.lexer.*;
        }

        if (not_stmt) {
            prev_parser.lexer.* = prev_lexer;
            p.* = prev_parser;

            const expr = try parseExpr(alloc, p, .lowest);
            try program.append(alloc, .{ .expr = expr });

            try p.expectPeekAdvance(.rbrace, error.ExpectedRBrace);
        }

        if (!p.currTokenIs(.rbrace)) return error.ExpectedRBrace;

        return BlockExpr{ .program = program };
    }

    fn parseFuncExpr(alloc: Allocator, p: *Parser) !Expression {
        std.debug.assert(p.currTokenIs(.func));

        try p.expectPeekAdvance(.lparen, error.ExpectedLParen);

        var params = try parseFuncParams(alloc, p);
        errdefer params.deinit(alloc);

        try p.expectPeekAdvance(.lbrace, error.ExpectedLBrace);

        const body = try parseBlockExpr(alloc, p);

        return Expression{ .func = .{ .params = params, .body = body } };
    }

    fn parseFuncParams(alloc: Allocator, p: *Parser) !ArrayList([]const u8) {
        var params = ArrayList([]const u8){};
        errdefer params.deinit(alloc);

        if (p.peekTokenIs(.rparen)) {
            p.advanceTokens();
            return params;
        }

        p.advanceTokens();

        if (p.currTokenIs(.ident)) {
            try params.append(alloc, p.curr_token.?.ident);
        } else return error.ExpectedIdent;

        while (p.peekTokenIs(.comma)) {
            p.advanceTokens();
            p.advanceTokens();
            switch (p.curr_token orelse return error.UnexpectedEof) {
                .ident => |name| try params.append(alloc, name),
                .rparen => return params,
                else => return error.ExpectedIdent,
            }
        }

        try p.expectPeekAdvance(.rparen, error.ExpectedRParen);

        return params;
    }

    fn parseCallExpr(alloc: Allocator, p: *Parser, callee: Expression) !Expression {
        var args = try parseCallArgs(alloc, p);
        errdefer {
            const slice = args.slice();
            for (0..slice.len) |i| {
                slice.get(i).deinit(alloc);
            }
            args.deinit(alloc);
        }
        const boxed_callee = try alloc.create(Expression);
        boxed_callee.* = callee;

        return Expression{ .call = .{ .callee = boxed_callee, .args = args } };
    }

    fn parseCallArgs(alloc: Allocator, p: *Parser) !MultiArrayList(Expression) {
        var args: MultiArrayList(Expression) = .{};
        errdefer {
            const slice = args.slice();
            for (0..slice.len) |i| {
                slice.get(i).deinit(alloc);
            }
            args.deinit(alloc);
        }

        if (p.peekTokenIs(.rparen)) {
            p.advanceTokens();
            return args;
        }

        p.advanceTokens();
        try args.append(alloc, try parseExpr(alloc, p, .lowest));

        while (p.peekTokenIs(.comma)) {
            p.advanceTokens();
            p.advanceTokens();
            if (p.currTokenIs(.rparen)) {
                return args;
            }
            try args.append(alloc, try parseExpr(alloc, p, .lowest));
        }

        try p.expectPeekAdvance(.rparen, error.ExpectedRParen);

        return args;
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
    for (0..statements.len, cases) |i, case| {
        const stmt = statements.get(i);
        try testing.expectEqualStrings(case[0], stmt.let.name);
        try testing.expectEqual(case[1], stmt.let.value);
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
    for (0..statements.len, cases) |i, case| {
        const stmt = statements.get(i);
        try testing.expectEqual(case, stmt.@"return");
    }
}

test "unit expression" {
    const input = "();{};";

    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);

    var program = (try parser.parseProgram(testing.allocator)).program;
    defer program.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 2), program.len);

    const cases = [_]Statement{
        .{ .expr = .unit },
        .{ .expr = .unit },
    };

    const statements = program.slice();
    for (0..statements.len, cases) |i, case| {
        const stmt = statements.get(i);
        try testing.expectEqualDeep(case, stmt);
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

test "bool expression" {
    const input = "true;false;";

    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);

    var program = (try parser.parseProgram(testing.allocator)).program;
    defer program.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 2), program.len);

    const cases = [_]Statement{
        .{ .expr = .{ .bool = true } },
        .{ .expr = .{ .bool = false } },
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
        .operand = .{ .int = 5 },
    }, .{
        .input = "-(15);",
        .op = .minus,
        .operand = .{ .int = 15 },
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
        op: BinaryOp,
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
            Statement{ .expr = .{ .binary_op = .{
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
        "-a * b;",
        "((-a) * b);",
    }, .{
        "!-a;",
        "(!(-a));",
    }, .{
        "a + b + c;",
        "((a + b) + c);",
    }, .{
        "a + b - c;",
        "((a + b) - c);",
    }, .{
        "a * b * c;",
        "((a * b) * c);",
    }, .{
        "a * b / c;",
        "((a * b) / c);",
    }, .{
        "a + b / c;",
        "(a + (b / c));",
    }, .{
        "a + b * c + d / e - f;",
        "(((a + (b * c)) + (d / e)) - f);",
    }, .{
        "3 + 4; -5 * 5;",
        "(3 + 4);\n(-5 * 5);",
    }, .{
        "5 > 4 == 3 < 4;",
        "((5 > 4) == (3 < 4));",
    }, .{
        "5 < 4 != 3 > 4;",
        "((5 < 4) != (3 > 4));",
    }, .{
        "3 + 4 * 5 == 3 * 1 + 4 * 5;",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
    }, .{
        "1 + (2 + 3) + 4;",
        "((1 + (2 + 3)) + 4);",
    }, .{
        "(5 + 5) * 2;",
        "((5 + 5) * 2);",
    }, .{
        "2 / (5 + 5);",
        "(2 / (5 + 5));",
    }, .{
        "-(5 + 5);",
        "(-(5 + 5));",
    }, .{
        "!(true == true);",
        "(!(true == true));",
    }, .{
        "a + add(b * c) + d;",
        "((a + add((b * c))) + d);",
    }, .{
        "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));",
        "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
    }, .{
        "add(a + b + c * d / f + g);",
        "add((((a + b) + ((c * d) / f)) + g));",
    } };

    inline for (cases) |case| {
        var lexer = try Lexer.init(case[0]);
        var parser = Parser.init(&lexer);

        const ast = try parser.parseProgram(testing.allocator);
        defer ast.deinit(testing.allocator);
        const program = ast.program;

        try testing.expectEqual(std.mem.count(u8, case[0], ";"), program.len);
        // catch |err| {
        //     std.log.err("case: \"{s}\"", .{case[0]});
        //     const slice = program.slice();
        //     for (0..slice.len) |i| {
        //         std.log.err("{}\n", .{slice.get(i)});
        //     }
        //     return err;
        // };

        try testing.expectFmt(case[1], "{}", .{ast});
    }
}

test "if expression" {
    const input = "if (x < y) { x };";

    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);

    const ast = try parser.parseProgram(testing.allocator);
    defer ast.deinit(testing.allocator);
    const program = ast.program;

    try testing.expectEqual(@as(usize, 1), program.len);

    const expr = program.get(0).expr.@"if";

    try testing.expectEqualDeep(
        Expression{ .binary_op = .{
            .left = &Expression{ .ident = "x" },
            .op = BinaryOp.lt,
            .right = &Expression{ .ident = "y" },
        } },
        expr.cond.*,
    );

    try testing.expectEqual(@as(usize, 1), expr.conseq.program.len);
    try testing.expectEqualDeep(
        Expression{ .ident = "x" },
        expr.conseq.program.get(0).expr,
    );

    try testing.expectEqual(
        @as(?Ast.Expression.BlockExpr, null),
        expr.alt,
    );
}

test "if/else expression" {
    const input = "if (x < y) { x } else { y };";

    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);

    const ast = try parser.parseProgram(testing.allocator);
    defer ast.deinit(testing.allocator);
    const program = ast.program;

    try testing.expectEqual(@as(usize, 1), program.len);

    const expr = program.get(0).expr.@"if";

    try testing.expectEqualDeep(
        Expression{ .binary_op = .{
            .left = &Expression{ .ident = "x" },
            .op = BinaryOp.lt,
            .right = &Expression{ .ident = "y" },
        } },
        expr.cond.*,
    );

    try testing.expectEqual(@as(usize, 1), expr.conseq.program.len);
    try testing.expectEqualDeep(
        Expression{ .ident = "x" },
        expr.conseq.program.get(0).expr,
    );

    try testing.expect(expr.alt != null);
    try testing.expectEqual(@as(usize, 1), expr.alt.?.program.len);
    try testing.expectEqualDeep(
        Expression{ .ident = "y" },
        expr.alt.?.program.get(0).expr,
    );
}

test "fn parsing" {
    const input = "fn(x, y) { (); x + y }";

    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);

    const ast = try parser.parseProgram(testing.allocator);
    defer ast.deinit(testing.allocator);
    const program = ast.program;

    try testing.expectEqual(@as(usize, 1), program.len);

    const expr = program.get(0).expr.func;

    try testing.expectEqual(@as(usize, 2), expr.params.items.len);
    try testing.expectEqualStrings("x", expr.params.items[0]);
    try testing.expectEqualStrings("y", expr.params.items[1]);

    try testing.expectEqual(@as(usize, 2), expr.body.program.len);
    try testing.expectEqualDeep(Statement{ .expr = .unit }, expr.body.program.get(0));
    try testing.expectEqualDeep(
        Expression{ .binary_op = .{
            .left = &Expression{ .ident = "x" },
            .op = BinaryOp.add,
            .right = &Expression{ .ident = "y" },
        } },
        expr.body.program.get(1).expr,
    );
}

test "fn parameters parsing" {
    const cases = comptime [_]struct { []const u8, []const []const u8 }{
        .{ "fn() {}", &.{} },
        .{ "fn(x) {}", &.{"x"} },
        .{ "fn(x,) {}", &.{"x"} },
        .{ "fn(x, y, z) {}", &.{ "x", "y", "z" } },
        .{ "fn(x, y, z,) {}", &.{ "x", "y", "z" } },
    };

    inline for (cases) |case| {
        var lexer = try Lexer.init(case[0]);
        var parser = Parser.init(&lexer);

        const ast = try parser.parseProgram(testing.allocator);
        defer ast.deinit(testing.allocator);
        const program = ast.program;

        try testing.expectEqual(@as(usize, 1), program.len);

        const expr = program.get(0).expr.func;

        try testing.expectEqual(case[1].len, expr.params.items.len);
        inline for (0.., case[1]) |i, expected_param| {
            try testing.expectEqualStrings(expected_param, expr.params.items[i]);
        }
    }
}

test "call expression parsing" {
    const input = "add(1, 2 * 3, 4 + 5);";

    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);

    const ast = try parser.parseProgram(testing.allocator);
    defer ast.deinit(testing.allocator);
    const program = ast.program;

    try testing.expectEqual(@as(usize, 1), program.len);

    const expr = program.get(0).expr.call;

    try testing.expectEqualStrings("add", expr.callee.ident);

    try testing.expectEqual(@as(usize, 3), expr.args.len);

    try testing.expectEqualDeep(Expression{ .int = 1 }, expr.args.get(0));
    try testing.expectEqualDeep(Expression{ .binary_op = .{
        .left = &Expression{ .int = 2 },
        .op = BinaryOp.mul,
        .right = &Expression{ .int = 3 },
    } }, expr.args.get(1));
    try testing.expectEqualDeep(Expression{ .binary_op = .{
        .left = &Expression{ .int = 4 },
        .op = BinaryOp.add,
        .right = &Expression{ .int = 5 },
    } }, expr.args.get(2));
}

test "call expression args parsing" {
    const cases = comptime [_]struct { []const u8, []const []const u8 }{
        .{ "add();", &.{} },
        .{ "add(1);", &.{"1"} },
        .{ "add(1,);", &.{"1"} },
        .{ "add(1, 2 * 3, 4 + 5);", &.{ "1", "(2 * 3)", "(4 + 5)" } },
        .{ "add(1, 2 * 3, 4 + 5,);", &.{ "1", "(2 * 3)", "(4 + 5)" } },
    };

    inline for (cases) |case| {
        var lexer = try Lexer.init(case[0]);
        var parser = Parser.init(&lexer);

        const ast = try parser.parseProgram(testing.allocator);
        defer ast.deinit(testing.allocator);
        const program = ast.program;

        try testing.expectEqual(@as(usize, 1), program.len);

        const expr = program.get(0).expr.call;

        try testing.expectEqual(case[1].len, expr.args.len);
        const args = expr.args.slice();
        inline for (0.., case[1]) |i, expected_param| {
            try testing.expectFmt(expected_param, "{}", .{args.get(i)});
        }
    }
}
