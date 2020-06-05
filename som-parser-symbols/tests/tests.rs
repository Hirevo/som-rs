use som_core::ast::*;
use som_lexer::{Lexer, Token};
use som_parser_symbols::combinators::*;
use som_parser_symbols::lang::*;
use som_parser_symbols::Parser;

#[test]
fn literal_tests() {
    let tokens: Vec<Token> = Lexer::new("1.2 5 #foo 'test'")
        .skip_whitespace(true)
        .collect();

    let parser = many(literal());
    let result = parser.parse(tokens.as_slice());

    assert!(result.is_some(), "input did not parse successfully");
    let (literals, rest) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    let mut iter = literals.into_iter();
    assert_eq!(iter.next(), Some(Literal::Double(1.2)));
    assert_eq!(iter.next(), Some(Literal::Integer(5)));
    assert_eq!(iter.next(), Some(Literal::Symbol(String::from("foo"))));
    assert_eq!(iter.next(), Some(Literal::String(String::from("test"))));
    assert_eq!(iter.next(), None);
}

#[test]
fn expression_test_1() {
    let tokens: Vec<Token> = Lexer::new("3 + counter get")
        .skip_whitespace(true)
        .collect();

    let parser = expression();
    let result = parser.parse(tokens.as_slice());

    assert!(result.is_some(), "input did not parse successfully");
    let (expression, rest) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    assert_eq!(
        expression,
        Expression::BinaryOp(BinaryOp {
            op: String::from("+"),
            lhs: Box::new(Expression::Literal(Literal::Integer(3))),
            rhs: Box::new(Expression::Message(Message {
                receiver: Box::new(Expression::Reference(String::from("counter"))),
                signature: String::from("get"),
                values: vec![],
            }))
        })
    );
}

#[test]
fn block_test() {
    let tokens: Vec<Token> =
        Lexer::new("[ :test | |local| local := 'this is correct'. local println. ]")
            .skip_whitespace(true)
            .collect();

    let parser = block();
    let result = parser.parse(tokens.as_slice());

    assert!(result.is_some(), "input did not parse successfully");
    let (block, rest) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    assert_eq!(
        block,
        Expression::Block(Block {
            parameters: vec![String::from("test")],
            locals: vec![String::from("local")],
            body: Body {
                exprs: vec![
                    Expression::Assignment(
                        String::from("local"),
                        Box::new(Expression::Literal(Literal::String(String::from(
                            "this is correct"
                        ))))
                    ),
                    Expression::Message(Message {
                        receiver: Box::new(Expression::Reference(String::from("local"))),
                        signature: String::from("println"),
                        values: vec![],
                    })
                ],
                full_stopped: true,
            }
        }),
    );
}

#[test]
fn expression_test_2() {
    let tokens: Vec<Token> = Lexer::new(
        "( 3 == 3 ) ifTrue: [ 'this is correct' println. ] ifFalse: [ 'oh no' println ]",
    )
    .skip_whitespace(true)
    .collect();

    let parser = expression();
    let result = parser.parse(tokens.as_slice());

    assert!(result.is_some(), "input did not parse successfully");
    let (expression, rest) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    assert_eq!(
        expression,
        Expression::Message(Message {
            receiver: Box::new(Expression::Term(Term {
                body: Body {
                    exprs: vec![Expression::BinaryOp(BinaryOp {
                        op: String::from("=="),
                        lhs: Box::new(Expression::Literal(Literal::Integer(3))),
                        rhs: Box::new(Expression::Literal(Literal::Integer(3))),
                    })],
                    full_stopped: false,
                }
            })),
            signature: String::from("ifTrue:ifFalse:"),
            values: vec![
                Expression::Block(Block {
                    parameters: vec![],
                    locals: vec![],
                    body: Body {
                        exprs: vec![Expression::Message(Message {
                            receiver: Box::new(Expression::Literal(Literal::String(String::from(
                                "this is correct"
                            )))),
                            signature: String::from("println"),
                            values: vec![],
                        })],
                        full_stopped: true,
                    }
                }),
                Expression::Block(Block {
                    parameters: vec![],
                    locals: vec![],
                    body: Body {
                        exprs: vec![Expression::Message(Message {
                            receiver: Box::new(Expression::Literal(Literal::String(String::from(
                                "oh no"
                            )))),
                            signature: String::from("println"),
                            values: vec![],
                        })],
                        full_stopped: false,
                    }
                }),
            ],
        }),
    );
}

#[test]
fn primary_test() {
    let tokens: Vec<Token> = Lexer::new("[ self fib: (n - 1) + (self fib: (n - 2)) ]")
        .skip_whitespace(true)
        .collect();
    let parser = primary();
    let result = parser.parse(tokens.as_slice());

    assert!(result.is_some(), "input did not parse successfully");
    let (primary, rest) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    assert_eq!(
        primary,
        Expression::Block(Block {
            parameters: vec![],
            locals: vec![],
            body: Body {
                exprs: vec![Expression::Message(Message {
                    receiver: Box::new(Expression::Reference(String::from("self"))),
                    signature: String::from("fib:"),
                    values: vec![Expression::BinaryOp(BinaryOp {
                        op: String::from("+"),
                        lhs: Box::new(Expression::Term(Term {
                            body: Body {
                                exprs: vec![Expression::BinaryOp(BinaryOp {
                                    op: String::from("-"),
                                    lhs: Box::new(Expression::Reference(String::from("n"))),
                                    rhs: Box::new(Expression::Literal(Literal::Integer(1))),
                                })],
                                full_stopped: false,
                            }
                        })),
                        rhs: Box::new(Expression::Term(Term {
                            body: Body {
                                exprs: vec![Expression::Message(Message {
                                    receiver: Box::new(Expression::Reference(String::from("self"))),
                                    signature: String::from("fib:"),
                                    values: vec![Expression::Term(Term {
                                        body: Body {
                                            exprs: vec![Expression::BinaryOp(BinaryOp {
                                                op: String::from("-"),
                                                lhs: Box::new(Expression::Reference(String::from(
                                                    "n"
                                                ))),
                                                rhs: Box::new(Expression::Literal(
                                                    Literal::Integer(2)
                                                )),
                                            })],
                                            full_stopped: false,
                                        }
                                    })],
                                })],
                                full_stopped: false,
                            }
                        }))
                    })],
                })],
                full_stopped: false,
            }
        }),
    );
}
