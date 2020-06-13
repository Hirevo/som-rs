use som_core::ast::*;
use som_core::span::Span;
use som_lexer::{Token, TokenKind};

use crate::combinators::*;
use crate::parser::Parser;

macro_rules! opaque {
    ($expr:expr) => {{
        move |input: &'a [Token]| $expr.parse(input)
    }};
}

/// A parser that expects to be nothing left in its input.
pub fn eof<'a>() -> impl Parser<'a, ()> {
    move |input: &'a [Token]| {
        if input.is_empty() {
            Some(((), input))
        } else {
            None
        }
    }
}

pub fn exact<'a>(kind: TokenKind) -> impl Parser<'a, Span> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        if head.kind() == kind {
            Some((head.span(), tail))
        } else {
            None
        }
    }
}

pub fn exact_ident<'a, 'b: 'a>(source: &'b str, string: &'b str) -> impl Parser<'a, ()> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head.kind() {
            TokenKind::Identifier if head.span().to_str(source) == string => Some(((), tail)),
            _ => None,
        }
    }
}

pub fn integer<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a [Token]| {
        let (sign, input) = optional(exact(TokenKind::Minus)).parse(input)?;
        let (head, tail) = input.split_first()?;
        match head.kind() {
            TokenKind::LitInteger => {
                let span = if let Some(sign) = sign {
                    Span::between(sign, head.span())
                } else {
                    head.span()
                };
                Some((
                    Expression {
                        span,
                        kind: ExpressionKind::Literal(Literal::Integer),
                    },
                    tail,
                ))
            }
            _ => None,
        }
    }
}

pub fn double<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a [Token]| {
        let (sign, input) = optional(exact(TokenKind::Minus)).parse(input)?;

        let (head, tail) = input.split_first()?;
        match head.kind() {
            TokenKind::LitDouble => {
                let span = if let Some(sign) = sign {
                    Span::between(sign, head.span())
                } else {
                    head.span()
                };
                Some((
                    Expression {
                        span,
                        kind: ExpressionKind::Literal(Literal::Double),
                    },
                    tail,
                ))
            }
            _ => None,
        }
    }
}

pub fn single_operator<'a>() -> impl Parser<'a, Span> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head.kind() {
            TokenKind::Not => Some((head.span(), tail)),
            TokenKind::And => Some((head.span(), tail)),
            TokenKind::Or => Some((head.span(), tail)),
            TokenKind::Star => Some((head.span(), tail)),
            TokenKind::Div => Some((head.span(), tail)),
            TokenKind::Mod => Some((head.span(), tail)),
            TokenKind::Plus => Some((head.span(), tail)),
            TokenKind::Equal => Some((head.span(), tail)),
            TokenKind::More => Some((head.span(), tail)),
            TokenKind::Less => Some((head.span(), tail)),
            TokenKind::Comma => Some((head.span(), tail)),
            TokenKind::At => Some((head.span(), tail)),
            TokenKind::Per => Some((head.span(), tail)),
            TokenKind::Minus => Some((head.span(), tail)),
            _ => None,
        }
    }
}

pub fn operator_sequence<'a>() -> impl Parser<'a, Span> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head.kind() {
            TokenKind::OperatorSequence => Some((head.span(), tail)),
            _ => None,
        }
    }
}

pub fn operator<'a>() -> impl Parser<'a, Span> {
    single_operator().or(operator_sequence())
}

pub fn identifier<'a>() -> impl Parser<'a, Span> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head.kind() {
            TokenKind::Identifier => Some((head.span(), tail)),
            _ => None,
        }
    }
}

pub fn string<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head.kind() {
            TokenKind::LitString(span) => Some((
                Expression {
                    span: head.span(),
                    kind: ExpressionKind::Literal(Literal::String(span)),
                },
                tail,
            )),
            _ => None,
        }
    }
}

pub fn symbol<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head.kind() {
            TokenKind::LitSymbol(span) => Some((
                Expression {
                    span: head.span(),
                    kind: ExpressionKind::Literal(Literal::Symbol(span)),
                },
                tail,
            )),
            _ => None,
        }
    }
}

pub fn array<'a>() -> impl Parser<'a, Expression> {
    exact(TokenKind::NewArray)
        .and(opaque!(many(literal())))
        .and(exact(TokenKind::EndTerm))
        .map(|((start, literals), end)| Expression {
            span: Span::between(start, end),
            kind: ExpressionKind::Literal(Literal::Array(literals)),
        })
}

pub fn literal<'a>() -> impl Parser<'a, Expression> {
    double().or(integer()).or(string()).or(symbol()).or(array())
}

pub fn keyword<'a>() -> impl Parser<'a, Span> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head.kind() {
            TokenKind::Keyword => Some((head.span(), tail)),
            _ => None,
        }
    }
}

pub fn unary_send<'a>(source: &'a str) -> impl Parser<'a, Expression> {
    opaque!(primary(source))
        .and(many(identifier()))
        .map(move |(receiver, signatures)| {
            signatures
                .into_iter()
                .fold(receiver, |receiver, signature| Expression {
                    span: Span::between(receiver.span, signature),
                    kind: ExpressionKind::Message(Message {
                        receiver: Box::new(receiver),
                        signature: signature.to_str(source).to_string(),
                        kind: MessageKind::Unary,
                    }),
                })
        })
}

pub fn binary_send<'a>(source: &'a str) -> impl Parser<'a, Expression> {
    unary_send(source)
        .and(many(operator().and(unary_send(source))))
        .map(move |(lhs, operands)| {
            operands.into_iter().fold(lhs, |lhs, (op, rhs)| Expression {
                span: Span::between(lhs.span, rhs.span),
                kind: ExpressionKind::Message(Message {
                    receiver: Box::new(lhs),
                    signature: op.to_str(source).to_string(),
                    kind: MessageKind::Binary { rhs: Box::new(rhs) },
                }),
            })
        })
}

pub fn positional_send<'a>(source: &'a str) -> impl Parser<'a, Expression> {
    binary_send(source)
        .and(many(keyword().and(binary_send(source))))
        .map(move |(receiver, pairs)| {
            if pairs.is_empty() {
                receiver
            } else {
                let (keywords, values): (Vec<_>, Vec<_>) = pairs.into_iter().unzip();
                let signature = keywords.iter().map(|span| span.to_str(source)).collect();

                Expression {
                    span: values.last().map_or(receiver.span, |expr| {
                        Span::between(receiver.span, expr.span)
                    }),
                    kind: ExpressionKind::Message(Message {
                        receiver: Box::new(receiver),
                        signature,
                        kind: MessageKind::Positional { keywords, values },
                    }),
                }
            }
        })
}

pub fn body<'a>(source: &'a str) -> impl Parser<'a, Body> {
    sep_by(exact(TokenKind::Period), exit(source).or(statement(source)))
        .and(optional(exact(TokenKind::Period)))
        .map(|(exprs, stopped)| Body {
            exprs,
            full_stopped: stopped.is_some(),
        })
}

pub fn locals<'a>() -> impl Parser<'a, Vec<Span>> {
    between(
        exact(TokenKind::Or),
        many(identifier()),
        exact(TokenKind::Or),
    )
}

pub fn parameter<'a>() -> impl Parser<'a, Span> {
    exact(TokenKind::Colon).and_right(identifier())
}

pub fn parameters<'a>() -> impl Parser<'a, Vec<Span>> {
    some(parameter()).and_left(exact(TokenKind::Or))
}

pub fn block<'a>(source: &'a str) -> impl Parser<'a, Expression> {
    exact(TokenKind::NewBlock)
        .and(default(parameters()))
        .and(default(locals()))
        .and(body(source))
        .and(exact(TokenKind::EndBlock))
        .map(|((((start, parameters), locals), body), end)| Expression {
            span: Span::between(start, end),
            kind: ExpressionKind::Block(Block {
                parameters,
                locals,
                body,
            }),
        })
}

pub fn term<'a>(source: &'a str) -> impl Parser<'a, Expression> {
    exact(TokenKind::NewTerm)
        .and(body(source))
        .and(exact(TokenKind::EndTerm))
        .map(|((start, body), end)| Expression {
            span: Span::between(start, end),
            kind: ExpressionKind::Term(Term { body }),
        })
}

pub fn exit<'a>(source: &'a str) -> impl Parser<'a, Expression> {
    exact(TokenKind::Exit)
        .and(statement(source))
        .map(|(start, expr)| Expression {
            span: Span::between(start, expr.span),
            kind: ExpressionKind::Exit(Box::new(expr)),
        })
}

pub fn expression<'a>(source: &'a str) -> impl Parser<'a, Expression> {
    positional_send(source)
}

pub fn primary<'a>(source: &'a str) -> impl Parser<'a, Expression> {
    (identifier().map(|span| Expression {
        span,
        kind: ExpressionKind::Reference,
    }))
    .or(term(source))
    .or(block(source))
    .or(literal())
}

pub fn assignment<'a>(source: &'a str) -> impl Parser<'a, Expression> {
    identifier()
        .and_left(exact(TokenKind::Assign))
        .and(opaque!(statement(source)))
        .map(|(name, expr)| Expression {
            span: Span::between(name, expr.span),
            kind: ExpressionKind::Assignment(name, Box::new(expr)),
        })
}

pub fn statement<'a>(source: &'a str) -> impl Parser<'a, Expression> {
    assignment(source).or(expression(source))
}

pub fn primitive<'a>() -> impl Parser<'a, MethodBody> {
    exact(TokenKind::Primitive).map(|span| MethodBody {
        span,
        kind: MethodBodyKind::Primitive,
    })
}

pub fn method_body<'a>(source: &'a str) -> impl Parser<'a, MethodBody> {
    exact(TokenKind::NewTerm)
        .and(default(locals()))
        .and(body(source))
        .and(exact(TokenKind::EndTerm))
        .map(|(((start, locals), body), end)| MethodBody {
            span: Span::between(start, end),
            kind: MethodBodyKind::Body { locals, body },
        })
}

pub fn unary_method_def<'a>(source: &'a str) -> impl Parser<'a, MethodDef> {
    identifier()
        .and_left(exact(TokenKind::Equal))
        .and(primitive().or(method_body(source)))
        .map(move |(signature, body)| MethodDef {
            span: Span::between(signature, body.span),
            kind: MethodKind::Unary,
            signature: signature.to_str(source).to_string(),
            body,
        })
}

pub fn positional_method_def<'a>(source: &'a str) -> impl Parser<'a, MethodDef> {
    some(keyword().and(identifier()))
        .and_left(exact(TokenKind::Equal))
        .and(primitive().or(method_body(source)))
        .map(move |(pairs, body)| {
            let (keywords, parameters): (Vec<_>, Vec<_>) = pairs.into_iter().unzip();
            let signature = keywords.iter().map(|span| span.to_str(source)).collect();

            MethodDef {
                span: Span::between(keywords[0], body.span),
                kind: MethodKind::Positional {
                    keywords,
                    parameters,
                },
                signature,
                body,
            }
        })
}

pub fn operator_method_def<'a>(source: &'a str) -> impl Parser<'a, MethodDef> {
    operator()
        .and(identifier())
        .and_left(exact(TokenKind::Equal))
        .and(primitive().or(method_body(source)))
        .map(move |((signature, rhs), body)| MethodDef {
            span: Span::between(signature, body.span),
            kind: MethodKind::Operator { rhs },
            signature: signature.to_str(source).to_string(),
            body,
        })
}

pub fn method_def<'a>(source: &'a str) -> impl Parser<'a, MethodDef> {
    unary_method_def(source)
        .or(positional_method_def(source))
        .or(operator_method_def(source))
}

pub fn class_def<'a>(source: &'a str) -> impl Parser<'a, ClassDef> {
    identifier()
        .and_left(exact(TokenKind::Equal))
        .and(optional(identifier()))
        .and(between(
            exact(TokenKind::NewTerm),
            default(locals()).and(many(method_def(source))).and(default(
                exact(TokenKind::Separator)
                    .and_right(default(locals()).and(many(method_def(source)))),
            )),
            exact(TokenKind::EndTerm),
        ))
        .map(|((name, super_class), (instance_defns, static_defns))| {
            let (instance_locals, instance_methods) = instance_defns;
            let (static_locals, static_methods) = static_defns;

            ClassDef {
                name,
                super_class,
                instance_locals,
                instance_methods,
                static_locals,
                static_methods,
            }
        })
}

pub fn file<'a>(source: &'a str) -> impl Parser<'a, ClassDef> {
    class_def(source).and_left(eof())
}
