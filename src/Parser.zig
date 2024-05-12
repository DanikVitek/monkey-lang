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
    if (self.peek_token == null or self.peek_token.? != expected) return err;
    self.advanceTokens();
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
    };

    const EnumMap = std.EnumMap;
    const TokenTag = std.meta.Tag(Token);

    const PrefixParseFn = *const fn (*Parser, Allocator) ParseExprError!Expression;
    const InfixParseFn = *const fn (*Parser, Expression, Allocator) ParseExprError!Expression;

    const prefix_parse_fns = b: {
        var map = EnumMap(TokenTag, PrefixParseFn){};
        map.put(.ident, &parseIdent);
        map.put(.int, &parseInt);
        map.put(.bang, &parsePrefixExpr);
        map.put(.minus, &parsePrefixExpr);
        break :b map;
    };

    const infix_parse_fns = b: {
        const map = EnumMap(TokenTag, InfixParseFn){};
        break :b map;
    };

    pub const ParseExprError = error{
        UnexpectedEof,
        UnexpectedToken,
        Unimplemented,
        // ExpectedBinOp,
    } || std.fmt.ParseIntError || Allocator.Error;

    pub fn parseExpr(p: *Parser, precedence: Precedence, alloc: Allocator) ParseExprError!Expression {
        _ = precedence;

        return prefix(p, alloc); // left expr
    }

    fn prefix(p: *Parser, alloc: Allocator) !Expression {
        const curr_token = p.curr_token orelse return error.UnexpectedEof;
        return switch (curr_token) {
            .ident => parseIdent(p),
            .int => parseInt(p),
            .bang, .minus => parsePrefixExpr(p, alloc),
            else => b: {
                std.log.err("No prefix parse function found for '{s}'", .{curr_token});
                break :b error.UnexpectedToken;
            },
        };
    }

    fn parseIdent(p: *Parser) Expression {
        std.debug.assert(p.curr_token != null and p.curr_token.? == .ident);
        return Expression{ .ident = p.curr_token.?.ident };
    }

    fn parseInt(p: *Parser) !Expression {
        std.debug.assert(p.curr_token != null and p.curr_token.? == .int);
        return Expression{ .int = try std.fmt.parseInt(u64, p.curr_token.?.int, 10) };
    }

    fn parsePrefixExpr(p: *Parser, alloc: Allocator) !Expression {
        std.debug.assert(p.curr_token != null and (p.curr_token.? == .minus or p.curr_token.? == .bang));

        const op = UnaryOp.fromToken(p.curr_token.?).?;
        p.advanceTokens();

        const operand = try alloc.create(Expression);
        operand.* = try parseExpr(p, .prefix, alloc);

        return Expression{ .unary_op = .{ .op = op, .operand = operand } };
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
    const input =
        \\!5;
        \\-15;
    ;

    var lexer = try Lexer.init(input);
    var parser = Parser.init(&lexer);

    const ast = try parser.parseProgram(testing.allocator);
    defer ast.deinit(testing.allocator);
    const program = ast.program;

    try testing.expectEqual(@as(usize, 2), program.len);

    const cases = [_]Statement{
        .{ .expr = Expression{ .unary_op = .{
            .op = UnaryOp.not,
            .operand = &Expression{ .int = 5 },
        } } },
        .{ .expr = Expression{ .unary_op = .{
            .op = UnaryOp.minus,
            .operand = &Expression{ .int = 15 },
        } } },
    };

    const statements = program.slice();
    for (0..statements.len, cases) |i, case| {
        const stmt = statements.get(i);
        try testing.expectEqualDeep(case, stmt);
    }
}
