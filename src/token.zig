const std = @import("std");

pub const Token = union(enum) {
    /// illegal token/character
    illegal: []const u8,

    // identifiers + literals
    /// names for variables, functions, etc.
    ident: []const u8,
    /// integer literals
    int: []const u8,

    // operators
    /// `=`
    assign,
    /// `+`
    plus,
    /// `-`
    minus,
    /// `*`
    star,
    /// `/`
    slash,
    /// `%`
    percent,
    /// `<`
    lt,
    /// `>`
    gt,
    /// `!`
    bang,
    /// `<=`
    leq,
    /// `>=`
    geq,
    /// `==`
    eq,
    /// `!=`
    neq,

    // delimiters
    /// `,`
    comma,
    /// `;`
    semicolon,

    /// `(`
    lparen,
    /// `)`
    rparen,
    /// `{`
    lbrace,
    /// `}`
    rbrace,

    // keywords
    /// `fn`
    function,
    /// `let`
    let,
    /// `true`
    true,
    /// `false`
    false,
    /// `if`
    @"if",
    /// `else`
    @"else",
    /// `return`
    @"return",

    pub fn format(value: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        return if (std.mem.eql(u8, "s", fmt)) switch (value) {
            .illegal => |lit| std.fmt.format(writer, ".{{ .illegal = {s} }}", .{lit}),
            .ident => |lit| std.fmt.format(writer, "{s}", .{lit}),
            .int => |lit| std.fmt.format(writer, "{s}", .{lit}),
            .assign => writer.writeByte('='),
            .plus => writer.writeByte('+'),
            .minus => writer.writeByte('-'),
            .star => writer.writeByte('*'),
            .slash => writer.writeByte('/'),
            .percent => writer.writeByte('%'),
            .lt => writer.writeByte('<'),
            .gt => writer.writeByte('>'),
            .bang => writer.writeByte('!'),
            .semicolon => writer.writeByte(';'),
            .comma => writer.writeByte(','),
            .lparen => writer.writeByte('('),
            .rparen => writer.writeByte(')'),
            .lbrace => writer.writeByte('{'),
            .rbrace => writer.writeByte('}'),
            .leq => writer.writeAll("<="),
            .geq => writer.writeAll(">="),
            .eq => writer.writeAll("=="),
            .neq => writer.writeAll("!="),
            .function => writer.writeAll("fn"),
            .let => writer.writeAll("let"),
            .true => writer.writeAll("true"),
            .false => writer.writeAll("false"),
            .@"if" => writer.writeAll("if"),
            .@"else" => writer.writeAll("else"),
            .@"return" => writer.writeAll("return"),
        } else switch (value) {
            .illegal, .ident, .int => |lit| std.fmt.format(
                writer,
                ".{{ .{s} = {s} }}",
                .{ @tagName(value), lit },
            ),
            else => std.fmt.format(writer, ".{s}", .{@tagName(value)}),
        };
    }
};
