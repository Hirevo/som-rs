//!
//! A small set of tests for the SOM lexer to demonstrate it's basic functionality.
//!

use som_lexer::{Lexer, Symbol};

#[test]
fn empty_class() {
    let mut lexer = Lexer::new("Foo = ()")
        .skip_whitespace(true)
        .skip_comments(true);

    assert_eq!(lexer.next(), Some(Symbol::Identifier(String::from("Foo"))));
    assert_eq!(lexer.next(), Some(Symbol::Equal));
    assert_eq!(lexer.next(), Some(Symbol::NewTerm));
    assert_eq!(lexer.next(), Some(Symbol::EndTerm));
    assert_eq!(lexer.next(), None);
}

#[test]
fn keyword_symbol() {
    let mut lexer = Lexer::new("#key:word:")
        .skip_whitespace(true)
        .skip_comments(true);

    assert_eq!(lexer.next(), Some(Symbol::Pound));

    //? ORIGINAL SOURCE (unclear if we keep this):
    //?   assertEquals(Symbol.KeywordSequence, l.getSym());
    //?   assertEquals("key:word:", l.getText());
    //?
    //? Could maybe look like:
    //?   assert_eq!(lexer.next(), Some(Symbol::Keyword(String::from("key:word:"))));
    assert_eq!(lexer.next(), Some(Symbol::Identifier(String::from("key"))));
    assert_eq!(lexer.next(), Some(Symbol::Colon));
    assert_eq!(lexer.next(), Some(Symbol::Identifier(String::from("word"))));
    assert_eq!(lexer.next(), Some(Symbol::Colon));

    assert_eq!(lexer.next(), None);
}

#[test]
fn assign_double() {
    let mut lexer = Lexer::new("var := 3.14.")
        .skip_whitespace(true)
        .skip_comments(true);

    assert_eq!(lexer.next(), Some(Symbol::Identifier(String::from("var"))));
    assert_eq!(lexer.next(), Some(Symbol::Assign));
    assert_eq!(lexer.next(), Some(Symbol::Double(3.14)));
    assert_eq!(lexer.next(), Some(Symbol::Period));
    assert_eq!(lexer.next(), None);
}

#[test]
fn string() {
    let mut lexer = Lexer::new("'some string with new\nline'")
        .skip_whitespace(true)
        .skip_comments(true);

    assert_eq!(
        lexer.next(),
        Some(Symbol::Str(String::from("some string with new\nline")))
    );
    assert_eq!(lexer.next(), None);
}
