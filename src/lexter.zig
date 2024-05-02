const std = @import("std");

const token = @import("token.zig");
const Token = token.Token;

const testing = std.testing;

const Lexer = struct {
    /// current position in input (points to current char)
    position: usize,
    /// iterator over the utf8 codepoints
    utf8: std.unicode.Utf8Iterator,

    pub fn init(input: []const u8) !Lexer {
        return .{
            .position = 0,
            .utf8 = (try std.unicode.Utf8View.init(input)).iterator(),
        };
    }

    pub fn nextToken(self: *Lexer) Token {
        self.skipWhitespace();

        return switch (self.readChar() orelse return .eof) {
            '=' => .assign,
            '+' => .plus,
            ',' => .comma,
            ';' => .semicolon,
            '(' => .lparen,
            ')' => .rparen,
            '{' => .lbrace,
            '}' => .rbrace,
            else => |ch| if (isLetter(ch)) blk: {
                const start = self.position;
                const end = self.endOfIdentifier();
                const literal = self.utf8.bytes[start..end];
                break :blk fromIdentifier(literal);
            } else if (isDigit(ch)) blk: {
                const start = self.position;
                const end = self.endOfInt();
                const literal = self.utf8.bytes[start..end];
                break :blk .{ .int = literal };
            } else blk: {
                const len = std.unicode.utf8CodepointSequenceLength(ch) catch unreachable;
                break :blk .{ .illegal = self.utf8.bytes[self.position .. self.position + len] };
            },
        };
    }

    fn readChar(self: *Lexer) ?u21 {
        self.position = self.utf8.i;
        return self.utf8.nextCodepoint();
    }

    fn peekChar(self: *Lexer) ?u21 {
        const slice = self.utf8.peek(1);
        if (slice.len == 0) return null;
        return std.unicode.utf8Decode(slice) catch unreachable;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (isWhitespace(self.peekChar())) {
            _ = self.readChar();
        }
    }

    fn endOfIdentifier(self: *Lexer) usize {
        while (isLetter(self.peekChar())) {
            _ = self.readChar();
        }
        return self.utf8.i;
    }

    fn endOfInt(self: *Lexer) usize {
        while (isDigit(self.peekChar())) {
            _ = self.readChar();
        }
        return self.utf8.i;
    }
};

fn isWhitespace(char: ?u21) bool {
    return switch (char orelse return false) {
        ' ' => true,
        '\t' => true,
        '\r' => true,
        '\n' => true,
        else => false,
    };
}

fn isLetter(char: ?u21) bool {
    return switch (char orelse return false) {
        'a'...'z' => true,
        'A'...'Z' => true,
        '_' => true,
        else => false,
    };
}

fn isDigit(char: ?u21) bool {
    return switch (char orelse return false) {
        '0'...'9' => true,
        else => false,
    };
}

fn fromIdentifier(literal: []const u8) Token {
    if (std.mem.eql(u8, "fn", literal)) return .function;
    if (std.mem.eql(u8, "let", literal)) return .let;
    return .{ .ident = literal };
}

test "next token" {
    const input = "=+(){},;";

    const tests = [_]Token{
        .assign,
        .plus,
        .lparen,
        .rparen,
        .lbrace,
        .rbrace,
        .comma,
        .semicolon,
        .eof,
    };

    var lexer = Lexer.init(input) catch unreachable;

    inline for (tests) |expected| {
        const tok = lexer.nextToken();
        try testing.expectEqual(expected, tok);
    }
}

test "next token from source" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\  x + y
        \\};
        \\
        \\let result = add(five, ten);
    ;

    const tests = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .semicolon,
        .let,
        .{ .ident = "ten" },
        .assign,
        .{ .int = "10" },
        .semicolon,
        .let,
        .{ .ident = "add" },
        .assign,
        .function,
        .lparen,
        .{ .ident = "x" },
        .comma,
        .{ .ident = "y" },
        .rparen,
        .lbrace,
        .{ .ident = "x" },
        .plus,
        .{ .ident = "y" },
        .rbrace,
        .semicolon,
        .let,
        .{ .ident = "result" },
        .assign,
        .{ .ident = "add" },
        .lparen,
        .{ .ident = "five" },
        .comma,
        .{ .ident = "ten" },
        .rparen,
        .semicolon,
        .eof,
    };

    var lexer = Lexer.init(input) catch unreachable;

    inline for (0.., tests) |i, expected| {
        const tok = lexer.nextToken();
        testing.expectEqualDeep(expected, tok) catch |err| switch (err) {
            error.TestExpectedEqual => std.debug.panic("[test #{d}] actual: {}", .{ i, tok }),
        };
    }
}
