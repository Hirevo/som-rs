use som_core::ast::*;
use som_core::span::Span;
use som_lexer::{Lexer, Token};
use som_parser_symbols::combinators::*;
use som_parser_symbols::lang::*;
use som_parser_symbols::Parser;

#[test]
fn literal_tests() {
    const CODE: &str = "1.2 5 #foo 'test'";

    let tokens: Vec<Token> = Lexer::new(CODE).skip_whitespace(true).collect();

    let parser = many(literal());
    let result = parser.parse(tokens.as_slice());

    assert!(result.is_some(), "input did not parse successfully");
    let (literals, rest) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    let mut iter = literals.into_iter();
    assert_eq!(
        iter.next(),
        Some(Expression {
            span: Span::new(0, 3),
            kind: ExpressionKind::Literal(Literal::Double),
        })
    );
    assert_eq!(
        iter.next(),
        Some(Expression {
            span: Span::new(4, 5),
            kind: ExpressionKind::Literal(Literal::Integer),
        })
    );
    assert_eq!(
        iter.next(),
        Some(Expression {
            span: Span::new(6, 10),
            kind: ExpressionKind::Literal(Literal::Symbol(Span::new(7, 10))),
        })
    );
    assert_eq!(
        iter.next(),
        Some(Expression {
            span: Span::new(11, 17),
            kind: ExpressionKind::Literal(Literal::String(Span::new(12, 16))),
        })
    );
    assert_eq!(iter.next(), None);
}

#[test]
fn expression_test_1() {
    const CODE: &str = "3 + counter get";

    let tokens: Vec<Token> = Lexer::new(CODE).skip_whitespace(true).collect();

    let parser = expression(CODE);
    let result = parser.parse(tokens.as_slice());

    assert!(result.is_some(), "input did not parse successfully");
    let (expression, rest) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    assert_eq!(
        expression,
        Expression {
            span: Span::new(0, 15),
            kind: ExpressionKind::Message(Message {
                signature: String::from("+"),
                receiver: Box::new(Expression {
                    span: Span::new(0, 1),
                    kind: ExpressionKind::Literal(Literal::Integer)
                }),
                kind: MessageKind::Binary {
                    rhs: Box::new(Expression {
                        span: Span::new(4, 15),
                        kind: ExpressionKind::Message(Message {
                            signature: String::from("get"),
                            receiver: Box::new(Expression {
                                span: Span::new(4, 11),
                                kind: ExpressionKind::Reference,
                            }),
                            kind: MessageKind::Unary,
                        })
                    })
                }
            })
        }
    );
}

#[test]
fn block_test() {
    const CODE: &str = "[ :test | |local| local := 'this is correct'. local println. ]";

    let tokens: Vec<Token> = Lexer::new(CODE).skip_whitespace(true).collect();

    let parser = block(CODE);
    let result = parser.parse(tokens.as_slice());

    assert!(result.is_some(), "input did not parse successfully");
    let (block, rest) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    assert_eq!(
        block,
        Expression {
            span: Span::new(0, 62),
            kind: ExpressionKind::Block(Block {
                parameters: vec![Span::new(3, 7)],
                locals: vec![Span::new(11, 16)],
                body: Body {
                    exprs: vec![
                        Expression {
                            span: Span::new(18, 44),
                            kind: ExpressionKind::Assignment(
                                Span::new(18, 23),
                                Box::new(Expression {
                                    span: Span::new(27, 44),
                                    kind: ExpressionKind::Literal(Literal::String(Span::new(
                                        28, 43
                                    )))
                                }),
                            )
                        },
                        Expression {
                            span: Span::new(46, 59),
                            kind: ExpressionKind::Message(Message {
                                receiver: Box::new(Expression {
                                    span: Span::new(46, 51),
                                    kind: ExpressionKind::Reference
                                }),
                                signature: String::from("println"),
                                kind: MessageKind::Unary,
                            })
                        }
                    ],
                    full_stopped: true,
                }
            })
        },
    );
}

#[test]
fn expression_test_2() {
    const CODE: &str =
        "( 3 == 3 ) ifTrue: [ 'this is correct' println. ] ifFalse: [ 'oh no' println ]";

    let tokens: Vec<Token> = Lexer::new(CODE).skip_whitespace(true).collect();

    let parser = expression(CODE);
    let result = parser.parse(tokens.as_slice());

    assert!(result.is_some(), "input did not parse successfully");
    let (expression, rest) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    assert_eq!(
        expression,
        Expression {
            span: Span::new(0, 78),
            kind: ExpressionKind::Message(Message {
                receiver: Box::new(Expression {
                    span: Span::new(0, 10),
                    kind: ExpressionKind::Term(Term {
                        body: Body {
                            exprs: vec![Expression {
                                span: Span::new(2, 8),
                                kind: ExpressionKind::Message(Message {
                                    receiver: Box::new(Expression {
                                        span: Span::new(2, 3),
                                        kind: ExpressionKind::Literal(Literal::Integer)
                                    }),
                                    signature: String::from("=="),
                                    kind: MessageKind::Binary {
                                        rhs: Box::new(Expression {
                                            span: Span::new(7, 8),
                                            kind: ExpressionKind::Literal(Literal::Integer)
                                        }),
                                    },
                                })
                            }],
                            full_stopped: false,
                        }
                    })
                }),
                signature: String::from("ifTrue:ifFalse:"),
                kind: MessageKind::Positional {
                    keywords: vec![Span::new(11, 18), Span::new(50, 58),],
                    values: vec![
                        Expression {
                            span: Span::new(19, 49),
                            kind: ExpressionKind::Block(Block {
                                parameters: vec![],
                                locals: vec![],
                                body: Body {
                                    exprs: vec![Expression {
                                        span: Span::new(21, 46),
                                        kind: ExpressionKind::Message(Message {
                                            receiver: Box::new(Expression {
                                                span: Span::new(21, 38),
                                                kind: ExpressionKind::Literal(Literal::String(
                                                    Span::new(22, 37)
                                                ))
                                            }),
                                            signature: String::from("println"),
                                            kind: MessageKind::Unary,
                                        })
                                    }],
                                    full_stopped: true,
                                }
                            })
                        },
                        Expression {
                            span: Span::new(59, 78),
                            kind: ExpressionKind::Block(Block {
                                parameters: vec![],
                                locals: vec![],
                                body: Body {
                                    exprs: vec![Expression {
                                        span: Span::new(61, 76),
                                        kind: ExpressionKind::Message(Message {
                                            receiver: Box::new(Expression {
                                                span: Span::new(61, 68),
                                                kind: ExpressionKind::Literal(Literal::String(
                                                    Span::new(62, 67),
                                                ))
                                            }),
                                            signature: String::from("println"),
                                            kind: MessageKind::Unary,
                                        })
                                    }],
                                    full_stopped: false,
                                }
                            }),
                        }
                    ],
                }
            }),
        }
    );
}
