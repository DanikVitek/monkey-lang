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
        _ = options;
        return if (std.mem.eql(u8, "s", fmt)) switch (value) {
            .illegal => |lit| std.fmt.format(writer, ".{{ .illegal = {s} }}", .{lit}),
            .ident => |lit| std.fmt.format(writer, "{s}", .{lit}),
            .int => |lit| std.fmt.format(writer, "{s}", .{lit}),
            .assign => std.fmt.format(writer, "=", .{}),
            .plus => std.fmt.format(writer, "+", .{}),
            .minus => std.fmt.format(writer, "-", .{}),
            .star => std.fmt.format(writer, "*", .{}),
            .slash => std.fmt.format(writer, "/", .{}),
            .lt => std.fmt.format(writer, "<", .{}),
            .gt => std.fmt.format(writer, ">", .{}),
            .bang => std.fmt.format(writer, "!", .{}),
            .leq => std.fmt.format(writer, "<=", .{}),
            .geq => std.fmt.format(writer, ">=", .{}),
            .eq => std.fmt.format(writer, "==", .{}),
            .neq => std.fmt.format(writer, "!=", .{}),
            .comma => std.fmt.format(writer, ",", .{}),
            .semicolon => std.fmt.format(writer, ";", .{}),
            .lparen => std.fmt.format(writer, "(", .{}),
            .rparen => std.fmt.format(writer, ")", .{}),
            .lbrace => std.fmt.format(writer, "{{", .{}),
            .rbrace => std.fmt.format(writer, "}}", .{}),
            .function => std.fmt.format(writer, "fn", .{}),
            .let => std.fmt.format(writer, "let", .{}),
            .true => std.fmt.format(writer, "true", .{}),
            .false => std.fmt.format(writer, "false", .{}),
            .@"if" => std.fmt.format(writer, "if", .{}),
            .@"else" => std.fmt.format(writer, "else", .{}),
            .@"return" => std.fmt.format(writer, "return", .{}),
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
