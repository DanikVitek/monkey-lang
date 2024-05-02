pub const Token = union(enum) {
    /// illegal token/character
    illegal: []const u8,
    /// end of file
    eof,

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
};
