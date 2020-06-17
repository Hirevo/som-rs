use som_core::span::Span;

/// Represents the kind of a token from the lexer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
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
    LitInteger,
    /// A floating-point literal (`10.6`).
    LitDouble,
    /// A string literal (`'hello, world'`).
    LitString(Span),
    /// A symbol literal (`#foo`).
    LitSymbol(Span),
    /// An identifier (`foo`).
    Identifier,
    /// A keyword (`fromString:`).
    Keyword,
    /// A sequence of operators (eg: `>>>`).
    OperatorSequence,
    /// A comment (`"what a beautiful and majestic piece of code"`).
    Comment(Span),
    /// Some whitespace (` `).
    Whitespace,
}

/// Represents a token from the lexer.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    /// The span of the token.
    span: Span,
    /// The kind of the token.
    kind: TokenKind,
}

impl Token {
    /// Construct a token given its span and its kind.
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self { span, kind }
    }

    /// Get the kind of the token.
    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    /// Get the span of the token.
    pub fn span(&self) -> Span {
        self.span
    }
}
