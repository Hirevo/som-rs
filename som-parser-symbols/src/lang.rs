use som_core::ast::*;
use som_lexer::Token;

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

pub fn exact<'a>(ch: Token) -> impl Parser<'a, ()> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        if *head == ch {
            Some(((), tail))
        } else {
            None
        }
    }
}

pub fn exact_ident<'a, 'b: 'a>(string: &'b str) -> impl Parser<'a, ()> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Identifier(ref ident) if ident.as_str() == string => Some(((), tail)),
            _ => None,
        }
    }
}

pub fn integer<'a>() -> impl Parser<'a, i64> {
    move |input: &'a [Token]| {
        let (sign, input) = optional(exact(Token::Minus)).parse(input)?;
        let sign = if sign.is_some() { -1 } else { 1 };

        let (head, tail) = input.split_first()?;
        match head {
            Token::LitInteger(value) => Some((*value * sign, tail)),
            _ => None,
        }
    }
}

pub fn double<'a>() -> impl Parser<'a, f64> {
    move |input: &'a [Token]| {
        let (sign, input) = optional(exact(Token::Minus)).parse(input)?;
        let sign = if sign.is_some() { -1.0 } else { 1.0 };

        let (head, tail) = input.split_first()?;
        match head {
            Token::LitDouble(value) => Some((*value * sign, tail)),
            _ => None,
        }
    }
}

pub fn single_operator<'a>() -> impl Parser<'a, &'static str> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Not => Some(("~", tail)),
            Token::And => Some(("&", tail)),
            Token::Or => Some(("|", tail)),
            Token::Star => Some(("*", tail)),
            Token::Div => Some(("/", tail)),
            Token::Mod => Some(("\\", tail)),
            Token::Plus => Some(("+", tail)),
            Token::Equal => Some(("=", tail)),
            Token::More => Some((">", tail)),
            Token::Less => Some(("<", tail)),
            Token::Comma => Some((",", tail)),
            Token::At => Some(("@", tail)),
            Token::Per => Some(("%", tail)),
            Token::Minus => Some(("-", tail)),
            _ => None,
        }
    }
}

pub fn operator_sequence<'a>() -> impl Parser<'a, String> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::OperatorSequence(seq) => Some((seq.clone(), tail)),
            _ => None,
        }
    }
}

pub fn operator<'a>() -> impl Parser<'a, String> {
    single_operator().map(String::from).or(operator_sequence())
}

pub fn identifier<'a>() -> impl Parser<'a, String> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Identifier(value) => Some((value.clone(), tail)),
            _ => None,
        }
    }
}

pub fn string<'a>() -> impl Parser<'a, String> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::LitString(value) => Some((value.clone(), tail)),
            _ => None,
        }
    }
}

pub fn symbol<'a>() -> impl Parser<'a, String> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::LitSymbol(value) => Some((value.clone(), tail)),
            _ => None,
        }
    }
}

pub fn array<'a>() -> impl Parser<'a, Vec<Literal>> {
    move |input: &'a [Token]| {
        between(
            exact(Token::NewArray),
            many(literal()),
            exact(Token::EndTerm),
        )
        .parse(input)
    }
}

pub fn literal<'a>() -> impl Parser<'a, Literal> {
    (double().map(Literal::Double))
        .or(integer().map(Literal::Integer))
        .or(string().map(Literal::String))
        .or(symbol().map(Literal::Symbol))
        .or(array().map(Literal::Array))
}

pub fn keyword<'a>() -> impl Parser<'a, String> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Keyword(value) => Some((value.clone(), tail)),
            _ => None,
        }
    }
}

pub fn unary_send<'a>() -> impl Parser<'a, Expression> {
    opaque!(primary())
        .and(many(identifier()))
        .map(|(receiver, signatures)| {
            signatures
                .into_iter()
                .fold(receiver, |receiver, signature| {
                    Expression::Message(Message {
                        receiver: Box::new(receiver),
                        signature,
                        values: Vec::new(),
                    })
                })
        })
}

pub fn binary_send<'a>() -> impl Parser<'a, Expression> {
    unary_send()
        .and(many(operator().and(unary_send().map(Box::new))))
        .map(|(lhs, operands)| {
            operands.into_iter().fold(lhs, |lhs, (op, rhs)| {
                Expression::BinaryOp(BinaryOp {
                    lhs: Box::new(lhs),
                    op,
                    rhs,
                })
            })
        })
}

pub fn positional_send<'a>() -> impl Parser<'a, Expression> {
    binary_send()
        .and(many(keyword().and(binary_send())))
        .map(|(receiver, pairs)| {
            if pairs.is_empty() {
                receiver
            } else {
                let (signature, values) = pairs.into_iter().unzip();

                Expression::Message(Message {
                    receiver: Box::new(receiver),
                    signature,
                    values,
                })
            }
        })
}

pub fn body<'a>() -> impl Parser<'a, Body> {
    sep_by(exact(Token::Period), exit().or(statement()))
        .and(optional(exact(Token::Period)))
        .map(|(exprs, stopped)| Body {
            exprs,
            full_stopped: stopped.is_some(),
        })
}

pub fn locals<'a>() -> impl Parser<'a, Vec<String>> {
    between(exact(Token::Or), many(identifier()), exact(Token::Or))
}

pub fn parameter<'a>() -> impl Parser<'a, String> {
    exact(Token::Colon).and_right(identifier())
}

pub fn parameters<'a>() -> impl Parser<'a, Vec<String>> {
    some(parameter()).and_left(exact(Token::Or))
}

pub fn block<'a>() -> impl Parser<'a, Expression> {
    between(
        exact(Token::NewBlock),
        default(parameters()).and(default(locals())).and(body()),
        exact(Token::EndBlock),
    )
    .map(|((parameters, locals), body)| {
        Expression::Block(Block {
            parameters,
            locals,
            body,
        })
    })
}

pub fn term<'a>() -> impl Parser<'a, Expression> {
    between(exact(Token::NewTerm), body(), exact(Token::EndTerm))
        .map(|body| Expression::Term(Term { body }))
}

pub fn exit<'a>() -> impl Parser<'a, Expression> {
    exact(Token::Exit)
        .and_right(statement())
        .map(|expr| Expression::Exit(Box::new(expr)))
}

pub fn expression<'a>() -> impl Parser<'a, Expression> {
    positional_send()
}

pub fn primary<'a>() -> impl Parser<'a, Expression> {
    (identifier().map(Expression::Reference))
        .or(term())
        .or(block())
        .or(literal().map(Expression::Literal))
}

pub fn assignment<'a>() -> impl Parser<'a, Expression> {
    identifier()
        .and_left(exact(Token::Assign))
        .and(opaque!(statement()))
        .map(|(name, expr)| Expression::Assignment(name, Box::new(expr)))
}

pub fn statement<'a>() -> impl Parser<'a, Expression> {
    assignment().or(expression())
}

pub fn primitive<'a>() -> impl Parser<'a, MethodBody> {
    exact(Token::Primitive).map(|_| MethodBody::Primitive)
}

pub fn method_body<'a>() -> impl Parser<'a, MethodBody> {
    between(
        exact(Token::NewTerm),
        default(locals()).and(body()),
        exact(Token::EndTerm),
    )
    .map(|(locals, body)| MethodBody::Body { locals, body })
}

pub fn unary_method_def<'a>() -> impl Parser<'a, MethodDef> {
    identifier()
        .and_left(exact(Token::Equal))
        .and(primitive().or(method_body()))
        .map(|(signature, body)| MethodDef {
            kind: MethodKind::Unary,
            signature,
            body,
        })
}

pub fn positional_method_def<'a>() -> impl Parser<'a, MethodDef> {
    some(keyword().and(identifier()))
        .and_left(exact(Token::Equal))
        .and(primitive().or(method_body()))
        .map(|(pairs, body)| {
            let (signature, parameters) = pairs.into_iter().unzip();

            MethodDef {
                kind: MethodKind::Positional { parameters },
                signature,
                body,
            }
        })
}

pub fn operator_method_def<'a>() -> impl Parser<'a, MethodDef> {
    operator()
        .and(identifier())
        .and_left(exact(Token::Equal))
        .and(primitive().or(method_body()))
        .map(|((signature, rhs), body)| MethodDef {
            kind: MethodKind::Operator { rhs },
            signature,
            body,
        })
}

pub fn method_def<'a>() -> impl Parser<'a, MethodDef> {
    unary_method_def()
        .or(positional_method_def())
        .or(operator_method_def())
}

pub fn class_def<'a>() -> impl Parser<'a, ClassDef> {
    identifier()
        .and_left(exact(Token::Equal))
        .and(optional(identifier()))
        .and(between(
            exact(Token::NewTerm),
            default(locals()).and(many(method_def())).and(default(
                exact(Token::Separator).and_right(default(locals()).and(many(method_def()))),
            )),
            exact(Token::EndTerm),
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

pub fn file<'a>() -> impl Parser<'a, ClassDef> {
    class_def().and_left(eof())
}
