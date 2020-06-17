use som_core::span::Span;
use som_lexer::{Lexer, TokenKind};

#[test]
fn empty_class_test() {
    const CODE: &str = "Foo = ()";

    let mut lexer = Lexer::new(CODE).map(|token| (token.span().to_str(CODE), token.kind()));

    assert_eq!(lexer.next(), Some(("Foo", TokenKind::Identifier)));
    assert_eq!(lexer.next(), Some((" ", TokenKind::Whitespace)));
    assert_eq!(lexer.next(), Some(("=", TokenKind::Equal)));
    assert_eq!(lexer.next(), Some((" ", TokenKind::Whitespace)));
    assert_eq!(lexer.next(), Some(("(", TokenKind::NewTerm)));
    assert_eq!(lexer.next(), Some((")", TokenKind::EndTerm)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn symbol_literal_test() {
    const CODE: &str = "#key:word:";

    let mut lexer = Lexer::new(CODE).map(|token| (token.span().to_str(CODE), token.kind()));

    assert_eq!(
        lexer.next(),
        Some(("#key:word:", TokenKind::LitSymbol(Span::new(1, 10))))
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn assignment_test() {
    const CODE: &str = "var := 3.14.";

    let mut lexer = Lexer::new(CODE).map(|token| (token.span().to_str(CODE), token.kind()));

    assert_eq!(lexer.next(), Some(("var", TokenKind::Identifier)));
    assert_eq!(lexer.next(), Some((" ", TokenKind::Whitespace)));
    assert_eq!(lexer.next(), Some((":=", TokenKind::Assign)));
    assert_eq!(lexer.next(), Some((" ", TokenKind::Whitespace)));
    assert_eq!(lexer.next(), Some(("3.14", TokenKind::LitDouble)));
    assert_eq!(lexer.next(), Some((".", TokenKind::Period)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn string_literal_test() {
    const CODE: &str = "'some string with new\nline'";

    let mut lexer = Lexer::new(CODE).map(|token| (token.span().to_str(CODE), token.kind()));

    assert_eq!(
        lexer.next(),
        Some((CODE, TokenKind::LitString(Span::new(1, 26))))
    );
    assert_eq!(lexer.next(), None);
}
