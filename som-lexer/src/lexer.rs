use crate::symbol::Symbol;

/// The lexer for the Simple Object Machine.
pub struct Lexer {
    pub(crate) chars: Vec<char>,
    pub(crate) skip_comments: bool,
    pub(crate) skip_whitespace: bool,
    pub(crate) skip_separator: bool,
}

impl Lexer {
    const SEPARATOR: &'static str = "----";
    const PRIMITIVE: &'static str = "primitive";

    pub fn new<T: AsRef<str>>(input: T) -> Lexer {
        Lexer {
            chars: input.as_ref().chars().rev().collect(),
            skip_comments: false,
            skip_whitespace: false,
            skip_separator: false,
        }
    }

    pub fn skip_whitespace(mut self, value: bool) -> Lexer {
        self.skip_whitespace = value;
        self
    }

    pub fn skip_comments(mut self, value: bool) -> Lexer {
        self.skip_comments = value;
        self
    }

    pub fn skip_separator(mut self, value: bool) -> Lexer {
        self.skip_separator = value;
        self
    }

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
                        _ => {}
                    }
                }
                ch => output.push(ch),
            }
        }
    }

    fn lex_comment(&mut self) -> Option<Symbol> {
        let mut output = String::new();
        self.chars.pop()?;
        loop {
            let ch = self.chars.pop()?;
            if ch == '"' {
                break if self.skip_comments {
                    self.next()
                } else {
                    Some(Symbol::Comment(output))
                };
            } else {
                output.push(ch);
            }
        }
    }

    fn lex_operator(&mut self) -> Option<Symbol> {
        let ch = self.chars.pop()?;
        match ch {
            '~' => Some(Symbol::Not),
            '&' => Some(Symbol::And),
            '|' => Some(Symbol::Or),
            '*' => Some(Symbol::Star),
            '/' => Some(Symbol::Div),
            '\\' => Some(Symbol::Mod),
            '+' => Some(Symbol::Plus),
            '=' => Some(Symbol::Equal),
            '>' => Some(Symbol::More),
            '<' => Some(Symbol::Less),
            ',' => Some(Symbol::Comma),
            '@' => Some(Symbol::At),
            '%' => Some(Symbol::Per),
            '-' => Some(Symbol::Minus),
            _ => None,
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
    type Item = Symbol;

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
                    Some(Symbol::Whitespace)
                }
            }
            '\'' => self.lex_string().map(Symbol::LitString),
            '"' => self.lex_comment(),
            '[' => {
                self.chars.pop()?;
                Some(Symbol::NewBlock)
            }
            ']' => {
                self.chars.pop()?;
                Some(Symbol::EndBlock)
            }
            '(' => {
                self.chars.pop()?;
                Some(Symbol::NewTerm)
            }
            ')' => {
                self.chars.pop()?;
                Some(Symbol::EndTerm)
            }
            '#' => {
                iter.next()?;
                match iter.peek().copied() {
                    Some('\'') => {
                        self.chars.pop()?;
                        let symbol = self.lex_string()?;
                        Some(Symbol::LitSymbol(symbol))
                    }
                    Some('(') => {
                        self.chars.pop()?;
                        self.chars.pop()?;
                        Some(Symbol::NewArray)
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
                        Some(Symbol::LitSymbol(symbol))
                    }
                    Some(ch) if Lexer::is_operator(ch) => {
                        let len = iter.take_while(|ch| Lexer::is_operator(*ch)).count();
                        let mut symbol = String::with_capacity(len);
                        self.chars.pop()?;
                        for _ in 0..len {
                            symbol.push(self.chars.pop()?);
                        }
                        Some(Symbol::LitSymbol(symbol))
                    }
                    _ => None,
                }
            }
            '^' => {
                self.chars.pop()?;
                Some(Symbol::Exit)
            }
            '.' => {
                self.chars.pop()?;
                Some(Symbol::Period)
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
                        Some(Symbol::Separator)
                    }
                } else {
                    self.chars.pop()?;
                    Some(Symbol::Minus)
                }
            }
            ':' => {
                iter.next()?;
                if let Some('=') = iter.peek().copied() {
                    self.chars.pop()?;
                    self.chars.pop()?;
                    Some(Symbol::Assign)
                } else {
                    self.chars.pop()?;
                    Some(Symbol::Colon)
                }
            }
            _ if Lexer::is_operator(peeked) => self.lex_operator(),
            _ => {
                let primitive_len = Lexer::PRIMITIVE.chars().count();
                if iter.take(primitive_len).eq(Lexer::PRIMITIVE.chars()) {
                    for _ in 0..primitive_len {
                        self.chars.pop()?;
                    }
                    Some(Symbol::Primitive)
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
                        Some(Symbol::Keyword(ident))
                    } else {
                        Some(Symbol::Identifier(ident))
                    }
                } else if peeked.is_digit(10) {
                    let iter = self.chars.iter().rev().copied();
                    let int_part_len = iter.clone().take_while(|c| c.is_digit(10)).count();
                    let mut dec_iter = iter.clone().skip(int_part_len).peekable();
                    if let Some('.') = dec_iter.peek().copied() {
                        dec_iter.next()?;
                        match dec_iter.peek() {
                            Some(v) if v.is_digit(10) => {
                                let dec_part_len =
                                    dec_iter.clone().take_while(|c| c.is_digit(10)).count();
                                let total_len = int_part_len + dec_part_len + 1;
                                let repr: String = iter.take(total_len).collect();
                                let number: f64 = repr.parse().ok()?;
                                for _ in 0..total_len {
                                    self.chars.pop()?;
                                }
                                Some(Symbol::LitDouble(number))
                            }
                            _ => {
                                let repr: String = iter.take(int_part_len).collect();
                                let number: i64 = repr.parse().ok()?;
                                for _ in 0..int_part_len {
                                    self.chars.pop()?;
                                }
                                Some(Symbol::LitInteger(number))
                            }
                        }
                    } else {
                        let repr: String = iter.take(int_part_len).collect();
                        let number: i64 = repr.parse().ok()?;
                        for _ in 0..int_part_len {
                            self.chars.pop()?;
                        }
                        Some(Symbol::LitInteger(number))
                    }
                } else {
                    None
                }
            }
        }
    }
}
