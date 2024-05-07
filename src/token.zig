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
        _ = fmt;
        _ = options;
        switch (value) {
            .illegal, .ident, .int => |lit| try std.fmt.format(
                writer,
                ".{{ .{s} = {s} }}",
                .{ @tagName(value), lit },
            ),
            else => try std.fmt.format(writer, ".{s}", .{@tagName(value)}),
        }
    }
};
