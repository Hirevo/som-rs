use crate::token::Token;

/// The lexer for the Simple Object Machine.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lexer {
    pub(crate) chars: Vec<char>,
    pub(crate) skip_comments: bool,
    pub(crate) skip_whitespace: bool,
    pub(crate) skip_separator: bool,
}

impl Lexer {
    const SEPARATOR: &'static str = "----";
    const PRIMITIVE: &'static str = "primitive";

    /// Construct a new lexer.
    pub fn new<T: AsRef<str>>(input: T) -> Lexer {
        Lexer {
            chars: input.as_ref().chars().rev().collect(),
            skip_comments: false,
            skip_whitespace: false,
            skip_separator: false,
        }
    }

    /// Configure the lexer on whether to skip whitespace or not.
    pub fn skip_whitespace(mut self, value: bool) -> Lexer {
        self.skip_whitespace = value;
        self
    }

    /// Configure the lexer on whether to skip comments or not.
    pub fn skip_comments(mut self, value: bool) -> Lexer {
        self.skip_comments = value;
        self
    }

    /// Consume the lexer and return the left-over text.
    pub fn text(self) -> String {
        self.chars.into_iter().rev().collect()
    }

    fn lex_string(&mut self) -> Option<String> {
        let mut output = String::new();
        self.chars.pop()?;
        loop {
            let ch = self.chars.pop()?;
            match ch {
                '\'' => break Some(output),
                '\\' => {
                    let ch = self.chars.pop()?;
                    match ch {
                        't' => output.push('\t'),
                        'b' => output.push('\x08'),
                        'n' => output.push('\n'),
                        'r' => output.push('\r'),
                        'f' => output.push('\x12'),
                        '\'' => output.push('\''),
                        '\\' => output.push('\\'),
                        '0' => output.push('\0'),
                        _ => {}
                    }
                }
                ch => output.push(ch),
            }
        }
    }

    fn lex_comment(&mut self) -> Option<Token> {
        let mut output = String::new();
        self.chars.pop()?;
        loop {
            let ch = self.chars.pop()?;
            if ch == '"' {
                break if self.skip_comments {
                    self.next()
                } else {
                    Some(Token::Comment(output))
                };
            } else {
                output.push(ch);
            }
        }
    }

    fn lex_operator(&mut self) -> Option<Token> {
        let iter = self.chars.iter().rev().copied();
        let length = iter.take_while(|ch| Lexer::is_operator(*ch)).count();
        match length {
            0 => None,
            1 => {
                let ch = self.chars.pop()?;
                match ch {
                    '~' => Some(Token::Not),
                    '&' => Some(Token::And),
                    '|' => Some(Token::Or),
                    '*' => Some(Token::Star),
                    '/' => Some(Token::Div),
                    '\\' => Some(Token::Mod),
                    '+' => Some(Token::Plus),
                    '=' => Some(Token::Equal),
                    '>' => Some(Token::More),
                    '<' => Some(Token::Less),
                    ',' => Some(Token::Comma),
                    '@' => Some(Token::At),
                    '%' => Some(Token::Per),
                    '-' => Some(Token::Minus),
                    _ => None,
                }
            }
            length => {
                let mut operator = String::with_capacity(length);
                for _ in 0..length {
                    operator.push(self.chars.pop()?);
                }
                Some(Token::OperatorSequence(operator))
            }
        }
    }

    fn is_operator(ch: char) -> bool {
        matches!(
            ch,
            '~' | '&' | '|' | '*' | '/' | '\\' | '+' | '=' | '>' | '<' | ',' | '@' | '%' | '-'
        )
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut iter = self.chars.iter().rev().copied().peekable();
        let peeked = iter.peek().copied()?;
        match peeked {
            _ if peeked.is_whitespace() => {
                let count = iter.take_while(|c| c.is_whitespace()).count();
                for _ in 0..count {
                    self.chars.pop()?;
                }
                if self.skip_whitespace {
                    self.next()
                } else {
                    Some(Token::Whitespace)
                }
            }
            '\'' => self.lex_string().map(Token::LitString),
            '"' => self.lex_comment(),
            '[' => {
                self.chars.pop()?;
                Some(Token::NewBlock)
            }
            ']' => {
                self.chars.pop()?;
                Some(Token::EndBlock)
            }
            '(' => {
                self.chars.pop()?;
                Some(Token::NewTerm)
            }
            ')' => {
                self.chars.pop()?;
                Some(Token::EndTerm)
            }
            '#' => {
                iter.next()?;
                match iter.peek().copied() {
                    Some('\'') => {
                        self.chars.pop()?;
                        let symbol = self.lex_string()?;
                        Some(Token::LitSymbol(symbol))
                    }
                    Some('(') => {
                        self.chars.pop()?;
                        self.chars.pop()?;
                        Some(Token::NewArray)
                    }
                    Some(ch) if ch.is_alphabetic() => {
                        let len = iter
                            .take_while(|ch| ch.is_alphabetic() || matches!(*ch, ':' | '_'))
                            .count();
                        let mut symbol = String::with_capacity(len);
                        self.chars.pop()?;
                        for _ in 0..len {
                            symbol.push(self.chars.pop()?);
                        }
                        Some(Token::LitSymbol(symbol))
                    }
                    Some(ch) if Lexer::is_operator(ch) => {
                        let len = iter.take_while(|ch| Lexer::is_operator(*ch)).count();
                        let mut symbol = String::with_capacity(len);
                        self.chars.pop()?;
                        for _ in 0..len {
                            symbol.push(self.chars.pop()?);
                        }
                        Some(Token::LitSymbol(symbol))
                    }
                    _ => None,
                }
            }
            '^' => {
                self.chars.pop()?;
                Some(Token::Exit)
            }
            '.' => {
                self.chars.pop()?;
                Some(Token::Period)
            }
            '-' => {
                let sep_len = iter.take_while(|ch| *ch == '-').count();
                if sep_len >= Lexer::SEPARATOR.len() {
                    for _ in 0..sep_len {
                        self.chars.pop()?;
                    }
                    if self.skip_separator {
                        self.next()
                    } else {
                        Some(Token::Separator)
                    }
                } else {
                    self.lex_operator()
                }
            }
            ':' => {
                iter.next()?;
                if let Some('=') = iter.peek().copied() {
                    self.chars.pop()?;
                    self.chars.pop()?;
                    Some(Token::Assign)
                } else {
                    self.chars.pop()?;
                    Some(Token::Colon)
                }
            }
            _ if Lexer::is_operator(peeked) => self.lex_operator(),
            _ => {
                let primitive_len = Lexer::PRIMITIVE.chars().count();
                if iter.take(primitive_len).eq(Lexer::PRIMITIVE.chars()) {
                    for _ in 0..primitive_len {
                        self.chars.pop()?;
                    }
                    Some(Token::Primitive)
                } else if peeked.is_alphabetic() {
                    let mut ident: String = self
                        .chars
                        .iter()
                        .rev()
                        .copied()
                        .take_while(|c| c.is_alphanumeric() || *c == '_')
                        .collect();
                    let ident_len = ident.chars().count();
                    for _ in 0..ident_len {
                        self.chars.pop()?;
                    }
                    if let Some(':') = self.chars.last().copied() {
                        self.chars.pop()?;
                        ident.push(':');
                        Some(Token::Keyword(ident))
                    } else {
                        Some(Token::Identifier(ident))
                    }
                } else if peeked.is_digit(10) {
                    let iter = self.chars.iter().rev().copied();
                    let int_part_len = iter.clone().take_while(|c| c.is_digit(10)).count();
                    let mut dec_iter = iter.clone().skip(int_part_len).peekable();
                    match (dec_iter.next(), dec_iter.peek()) {
                        (Some('.'), Some(ch)) if ch.is_digit(10) => {
                            let dec_part_len =
                                dec_iter.clone().take_while(|c| c.is_digit(10)).count();
                            let total_len = int_part_len + dec_part_len + 1;
                            let repr: String = iter.take(total_len).collect();
                            let number: f64 = repr.parse().ok()?;
                            for _ in 0..total_len {
                                self.chars.pop()?;
                            }
                            Some(Token::LitDouble(number))
                        }
                        _ => {
                            let repr: String = iter.take(int_part_len).collect();
                            for _ in 0..int_part_len {
                                self.chars.pop()?;
                            }
                            if let Ok(number) = repr.parse::<i64>() {
                                Some(Token::LitInteger(number))
                            } else {
                                Some(Token::LitBigInteger(repr))
                            }
                        }
                    }
                } else {
                    None
                }
            }
        }
    }
}
