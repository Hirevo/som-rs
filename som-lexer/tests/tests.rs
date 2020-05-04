use som_lexer::{Lexer, Symbol};

#[test]
fn empty_class_test() {
    let mut lexer = Lexer::new("Foo = ()");

    assert_eq!(lexer.next(), Some(Symbol::Identifier(String::from("Foo"))));
    assert_eq!(lexer.next(), Some(Symbol::Whitespace));
    assert_eq!(lexer.next(), Some(Symbol::Equal));
    assert_eq!(lexer.next(), Some(Symbol::Whitespace));
    assert_eq!(lexer.next(), Some(Symbol::NewTerm));
    assert_eq!(lexer.next(), Some(Symbol::EndTerm));
    assert_eq!(lexer.next(), None);
}

#[test]
fn symbol_literal_test() {
    let mut lexer = Lexer::new("#key:word:");

    assert_eq!(
        lexer.next(),
        Some(Symbol::LitSymbol(String::from("key:word:")))
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn assignment_test() {
    let mut lexer = Lexer::new("var := 3.14.");

    assert_eq!(lexer.next(), Some(Symbol::Identifier(String::from("var"))));
    assert_eq!(lexer.next(), Some(Symbol::Whitespace));
    assert_eq!(lexer.next(), Some(Symbol::Assign));
    assert_eq!(lexer.next(), Some(Symbol::Whitespace));
    assert_eq!(lexer.next(), Some(Symbol::LitDouble(3.14)));
    assert_eq!(lexer.next(), Some(Symbol::Period));
    assert_eq!(lexer.next(), None);
}

#[test]
fn string_literal_test() {
    let mut lexer = Lexer::new("'some string with new\nline'");

    assert_eq!(
        lexer.next(),
        Some(Symbol::LitString(String::from(
            "some string with new\nline"
        )))
    );
    assert_eq!(lexer.next(), None);
}
