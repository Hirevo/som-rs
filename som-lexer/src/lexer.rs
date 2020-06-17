use std::iter::Peekable;
use std::str::Chars;

use som_core::span::Span;

use crate::token::{Token, TokenKind};

macro_rules! count_while {
    ($iter:expr, $func:expr) => {{
        let mut count = 0;
        loop {
            match $iter.peek().copied() {
                Some(ch) if { $func }(ch) => {
                    $iter.next()?;
                    count += 1;
                }
                _ => break,
            }
        }
        count
    }};
}

/// The lexer for the Simple Object Machine.
#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    pub(crate) current: usize,
    pub(crate) iter: Peekable<Chars<'a>>,
    pub(crate) skip_comments: bool,
    pub(crate) skip_whitespace: bool,
    pub(crate) skip_separator: bool,
}

impl<'a> Lexer<'a> {
    const SEPARATOR: &'static str = "----";
    const PRIMITIVE: &'static str = "primitive";

    /// Construct a new lexer.
    pub fn new(input: &'a str) -> Self {
        Self {
            current: 0,
            iter: input.chars().peekable(),
            skip_comments: false,
            skip_whitespace: false,
            skip_separator: false,
        }
    }

    /// Configure the lexer on whether to skip whitespace or not.
    pub fn skip_whitespace(mut self, value: bool) -> Self {
        self.skip_whitespace = value;
        self
    }

    /// Configure the lexer on whether to skip comments or not.
    pub fn skip_comments(mut self, value: bool) -> Self {
        self.skip_comments = value;
        self
    }

    /// Consume the lexer and return the left-over text.
    pub fn text(self) -> String {
        self.iter.rev().collect()
    }

    fn advance(&mut self, length: usize) -> Span {
        let end = self.current + length;
        let span = Span {
            from: self.current,
            to: end,
        };
        self.current = end;
        span
    }

    fn lex_string(&mut self) -> Option<(Span, Span)> {
        let mut length = 0;
        self.iter.next()?;
        let start = self.advance(1);
        loop {
            let ch = self.iter.next()?;
            match ch {
                '\'' => {
                    let inner_span = self.advance(length);
                    let end = self.advance(1);
                    break Some((Span::between(start, end), inner_span));
                }
                '\\' => {
                    self.iter.next()?;
                    length += 2;
                }
                _ => {
                    length += ch.len_utf8();
                }
            }
        }
    }

    fn lex_comment(&mut self) -> Option<Token> {
        let mut length = 0;
        self.iter.next()?;
        let start = self.advance(1);
        loop {
            let ch = self.iter.next()?;
            match ch {
                '"' => {
                    let inner_span = self.advance(length);
                    let end = self.advance(1);
                    break if self.skip_comments {
                        self.next()
                    } else {
                        Some(Token::new(
                            Span::between(start, end),
                            TokenKind::Comment(inner_span),
                        ))
                    };
                }
                _ => {
                    length += ch.len_utf8();
                }
            }
        }
    }

    fn lex_operator(&mut self) -> Option<Token> {
        let head = self.iter.next()?;
        let length = count_while!(self.iter, Self::is_operator);
        match length {
            0 => {
                let span = self.advance(1);
                match head {
                    '~' => Some(Token::new(span, TokenKind::Not)),
                    '&' => Some(Token::new(span, TokenKind::And)),
                    '|' => Some(Token::new(span, TokenKind::Or)),
                    '*' => Some(Token::new(span, TokenKind::Star)),
                    '/' => Some(Token::new(span, TokenKind::Div)),
                    '\\' => Some(Token::new(span, TokenKind::Mod)),
                    '+' => Some(Token::new(span, TokenKind::Plus)),
                    '=' => Some(Token::new(span, TokenKind::Equal)),
                    '>' => Some(Token::new(span, TokenKind::More)),
                    '<' => Some(Token::new(span, TokenKind::Less)),
                    ',' => Some(Token::new(span, TokenKind::Comma)),
                    '@' => Some(Token::new(span, TokenKind::At)),
                    '%' => Some(Token::new(span, TokenKind::Per)),
                    '-' => Some(Token::new(span, TokenKind::Minus)),
                    _ => None,
                }
            }
            length => {
                let span = self.advance(length + 1);
                Some(Token::new(span, TokenKind::OperatorSequence))
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

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // let mut iter = self.chars.iter().rev().copied().peekable();
        let peeked = self.iter.peek().copied()?;
        match peeked {
            _ if peeked.is_whitespace() => {
                let length = count_while!(self.iter, char::is_whitespace);
                let span = self.advance(length);
                if self.skip_whitespace {
                    self.next()
                } else {
                    Some(Token::new(span, TokenKind::Whitespace))
                }
            }
            '\'' => self
                .lex_string()
                .map(|(outer, inner)| Token::new(outer, TokenKind::LitString(inner))),
            '"' => self.lex_comment(),
            '[' => {
                self.iter.next()?;
                let span = self.advance(1);
                Some(Token::new(span, TokenKind::NewBlock))
            }
            ']' => {
                self.iter.next()?;
                let span = self.advance(1);
                Some(Token::new(span, TokenKind::EndBlock))
            }
            '(' => {
                self.iter.next()?;
                let span = self.advance(1);
                Some(Token::new(span, TokenKind::NewTerm))
            }
            ')' => {
                self.iter.next()?;
                let span = self.advance(1);
                Some(Token::new(span, TokenKind::EndTerm))
            }
            '#' => {
                self.iter.next()?;
                let start = self.advance(1);
                match self.iter.peek().copied() {
                    Some('\'') => {
                        let (outer, inner) = self.lex_string()?;
                        Some(Token::new(
                            Span::between(start, outer),
                            TokenKind::LitSymbol(inner),
                        ))
                    }
                    Some('(') => {
                        self.iter.next()?;
                        let end = self.advance(1);
                        Some(Token::new(Span::between(start, end), TokenKind::NewArray))
                    }
                    Some(ch) if ch.is_alphabetic() => {
                        let length = count_while!(self.iter, |ch: char| ch.is_alphabetic()
                            || matches!(ch, ':' | '_'));
                        let end = self.advance(length);
                        Some(Token::new(
                            Span::between(start, end),
                            TokenKind::LitSymbol(end),
                        ))
                    }
                    Some(ch) if Self::is_operator(ch) => {
                        let length = count_while!(self.iter, Self::is_operator);
                        let end = self.advance(length);
                        Some(Token::new(
                            Span::between(start, end),
                            TokenKind::LitSymbol(end),
                        ))
                    }
                    _ => None,
                }
            }
            '^' => {
                self.iter.next()?;
                let span = self.advance(1);
                Some(Token::new(span, TokenKind::Exit))
            }
            '.' => {
                self.iter.next()?;
                let span = self.advance(1);
                Some(Token::new(span, TokenKind::Period))
            }
            '-' => {
                let length = self.iter.clone().take_while(|ch| *ch == '-').count();
                if length >= Self::SEPARATOR.len() {
                    for _ in 0..length {
                        self.iter.next()?;
                    }
                    let span = self.advance(length);
                    if self.skip_separator {
                        self.next()
                    } else {
                        Some(Token::new(span, TokenKind::Separator))
                    }
                } else {
                    self.lex_operator()
                }
            }
            ':' => {
                self.iter.next()?;
                if let Some('=') = self.iter.peek().copied() {
                    self.iter.next()?;
                    let span = self.advance(2);
                    Some(Token::new(span, TokenKind::Assign))
                } else {
                    let span = self.advance(1);
                    Some(Token::new(span, TokenKind::Colon))
                }
            }
            _ if Self::is_operator(peeked) => self.lex_operator(),
            _ => {
                let length = Self::PRIMITIVE.chars().count();
                if self.iter.clone().take(length).eq(Self::PRIMITIVE.chars()) {
                    for _ in 0..length {
                        self.iter.next()?;
                    }
                    let span = self.advance(length);
                    Some(Token::new(span, TokenKind::Primitive))
                } else if peeked.is_alphabetic() {
                    let length =
                        count_while!(self.iter, |ch: char| ch.is_alphanumeric() || ch == '_');
                    let span = self.advance(length);
                    if let Some(':') = self.iter.peek().copied() {
                        self.iter.next()?;
                        let additional = self.advance(1);
                        Some(Token::new(
                            Span::between(span, additional),
                            TokenKind::Keyword,
                        ))
                    } else {
                        Some(Token::new(span, TokenKind::Identifier))
                    }
                } else if peeked.is_digit(10) {
                    let int_part_length = count_while!(self.iter, |ch: char| ch.is_digit(10));
                    let mut iter = self.iter.clone();
                    match (iter.next(), iter.peek()) {
                        (Some('.'), Some(ch)) if ch.is_digit(10) => {
                            self.iter.next()?;
                            let dec_part_length =
                                count_while!(self.iter, |ch: char| ch.is_digit(10));
                            let total_length = int_part_length + dec_part_length + 1;
                            let span = self.advance(total_length);
                            Some(Token::new(span, TokenKind::LitDouble))
                        }
                        _ => {
                            let span = self.advance(int_part_length);
                            Some(Token::new(span, TokenKind::LitInteger))
                        }
                    }
                } else {
                    None
                }
            }
        }
    }
}
