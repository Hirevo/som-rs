/// Represents a token from the lexer.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// A tilde, the bitwise 'not' operator (`~`).
    Not,
    /// A ampersand, the binary 'and' operator (`&`).
    And,
    /// A vertical bar, the binary 'or' operator (`|`).
    Or,
    /// A star, the multiplication operator (`*`).
    Star,
    /// A forward slash, the division operator (`/`).
    Div,
    /// A backward slash (`\\`).
    Mod,
    /// A plus, the addition operator ('+').
    Plus,
    /// A minus sign, the substraction operator (`-`)
    Minus,
    /// An equal sign, the equality operator (`=`).
    Equal,
    /// A greater-than sign, the greater-than operator (`>`).
    More,
    /// A lesser-than sign, the lesser-than operator (`>`).
    Less,
    /// A comma (`,`).
    Comma,
    /// An at-sign (`@`).
    At,
    /// A percentage sign, the modulo operator (`%`).
    Per,
    /// An opening square-bracket (`[`).
    NewBlock,
    /// A closing square-bracket (`]`).
    EndBlock,
    /// A colon (`:`).
    Colon,
    /// A period, the statement terminator (`.`).
    Period,
    /// A caret, the return operator (`^`).
    Exit,
    /// The assignment operator (`:=`).
    Assign,
    /// An opening parenthesis (`(`).
    NewTerm,
    /// A closing parenthesis (`)`).
    EndTerm,
    /// A pound sign immediately followed by opening parenthesis (`#(`).
    NewArray,
    /// A pound sign (`#`).
    Pound,
    /// The primitive keyword (`primitive`).
    Primitive,
    /// The separator sequence (`-------`).
    Separator,
    /// An integer literal (`10`).
    LitInteger(i32),
    /// A big integer literal (`1542252643255252434`).
    LitBigInteger(String),
    /// A floating-point literal (`10.6`).
    LitDouble(f64),
    /// A string literal (`'hello, world'`).
    LitString(String),
    /// A symbol literal (`#foo`).
    LitSymbol(String),
    /// An identifier (`foo`).
    Identifier(String),
    /// A keyword (`fromString:`).
    Keyword(String),
    /// A sequence of operators (eg: `>>>`).
    OperatorSequence(String),
    /// A comment (`"what a beautiful and majestic piece of code"`).
    Comment(String),
    /// Some whitespace (` `).
    Whitespace,
}
