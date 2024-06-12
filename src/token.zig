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
    func,
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
    /// `break`
    @"break",

    pub fn format(value: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        return if (std.mem.eql(u8, "s", fmt)) switch (value) {
            .illegal => |lit| std.fmt.format(writer, ".{{ .illegal = {s} }}", .{lit}),
            inline .ident, .int => |lit| std.fmt.format(writer, "{s}", .{lit}),
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
            .func => writer.writeAll("fn"),
            inline .let, .true, .false, .@"if", .@"else", .@"return", .@"break" => |_, tag| writer.writeAll(@tagName(tag)),
        } else switch (value) {
            inline .illegal, .ident, .int => |lit, tag| std.fmt.format(
                writer,
                ".{{ ." ++ @tagName(tag) ++ " = {s} }}",
                .{lit},
            ),
            inline .@"else", .@"return", .@"break" => |_, tag| writer.writeAll(".@\"" ++ @tagName(tag) ++ "\""),
            inline else => |_, tag| writer.writeAll("." ++ @tagName(tag)),
        };
    }
};
