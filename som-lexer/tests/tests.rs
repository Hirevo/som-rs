use som_lexer::{Lexer, Token};

#[test]
fn empty_class_test() {
    let mut lexer = Lexer::new("Foo = ()");

    assert_eq!(lexer.next(), Some(Token::Identifier(String::from("Foo"))));
    assert_eq!(lexer.next(), Some(Token::Whitespace));
    assert_eq!(lexer.next(), Some(Token::Equal));
    assert_eq!(lexer.next(), Some(Token::Whitespace));
    assert_eq!(lexer.next(), Some(Token::NewTerm));
    assert_eq!(lexer.next(), Some(Token::EndTerm));
    assert_eq!(lexer.next(), None);
}

#[test]
fn symbol_literal_test() {
    let mut lexer = Lexer::new("#key:word:");

    assert_eq!(
        lexer.next(),
        Some(Token::LitSymbol(String::from("key:word:")))
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn assignment_test() {
    let mut lexer = Lexer::new("var := 3.14.");

    assert_eq!(lexer.next(), Some(Token::Identifier(String::from("var"))));
    assert_eq!(lexer.next(), Some(Token::Whitespace));
    assert_eq!(lexer.next(), Some(Token::Assign));
    assert_eq!(lexer.next(), Some(Token::Whitespace));
    assert_eq!(lexer.next(), Some(Token::LitDouble(3.14)));
    assert_eq!(lexer.next(), Some(Token::Period));
    assert_eq!(lexer.next(), None);
}

#[test]
fn string_literal_test() {
    let mut lexer = Lexer::new("'some string with new\nline'");

    assert_eq!(
        lexer.next(),
        Some(Token::LitString(String::from("some string with new\nline")))
    );
    assert_eq!(lexer.next(), None);
}
