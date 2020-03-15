use crate::symbol::Symbol;

/// The lexer for the Simple Object Machine.
pub struct Lexer {
    pub(crate) chars: Vec<char>,
    pub(crate) skip_comments: bool,
    pub(crate) skip_whitespace: bool,
}

impl Lexer {
    const SEPARATOR: &'static str = "----";
    const PRIMITIVE: &'static str = "primitive";

    pub fn new(input: &str) -> Lexer {
        Lexer {
            chars: input.chars().rev().collect(),
            skip_comments: false,
            skip_whitespace: false,
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

    pub fn text(self) -> String {
        self.chars.into_iter().rev().collect()
    }

    fn lex_string(&mut self) -> Option<Symbol> {
        let mut output = String::new();
        self.chars.pop()?;
        loop {
            let ch = self.chars.pop()?;
            match ch {
                '\'' => break Some(Symbol::Str(output)),
                '\\' => {
                    let ch = self.chars.pop()?;
                    let ch = match ch {
                        't' => '\t',
                        'b' => '\x08',
                        'n' => '\n',
                        'r' => '\r',
                        'f' => '\x12',
                        '\'' => '\'',
                        '\\' => '\\',
                        _ => return None,
                    };
                    output.push(ch);
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
            match ch {
                '"' => break Some(Symbol::Comment(output)),
                '\\' => {
                    let ch = self.chars.pop()?;
                    let ch = match ch {
                        't' => '\t',
                        'b' => '\x08',
                        'n' => '\n',
                        'r' => '\r',
                        'f' => '\x12',
                        '"' => '"',
                        '\\' => '\\',
                        _ => return None,
                    };
                    output.push(ch);
                }
                ch => output.push(ch),
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
            '\'' => self.lex_string(),
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
                self.chars.pop()?;
                Some(Symbol::Pound)
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
                let sep_len = Lexer::SEPARATOR.chars().count();
                if iter.take(sep_len).eq(Lexer::SEPARATOR.chars()) {
                    for _ in 0..sep_len {
                        self.chars.pop()?;
                    }
                    Some(Symbol::Separator)
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
                    let ident: String = self
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
                    Some(Symbol::Identifier(ident))
                } else if peeked.is_digit(10) {
                    let iter = self.chars.iter().rev().copied();
                    let int_part_len = iter.clone().take_while(|c| c.is_digit(10)).count();
                    let mut dec_iter = iter.clone().skip(int_part_len).peekable();
                    if let Some('.') = dec_iter.peek().copied() {
                        dec_iter.next()?;
                        let dec_part_len = dec_iter.clone().take_while(|c| c.is_digit(10)).count();
                        let total_len = int_part_len + dec_part_len + 1;
                        let repr: String = iter.take(total_len).collect();
                        let number: f64 = repr.parse().ok()?;
                        for _ in 0..total_len {
                            self.chars.pop()?;
                        }
                        Some(Symbol::Double(number))
                    } else {
                        let repr: String = iter.take(int_part_len).collect();
                        let number: i64 = repr.parse().ok()?;
                        for _ in 0..int_part_len {
                            self.chars.pop()?;
                        }
                        Some(Symbol::Integer(number))
                    }
                } else {
                    None
                }
            }
        }
    }
}
