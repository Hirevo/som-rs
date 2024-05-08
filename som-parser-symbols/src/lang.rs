use som_core::ast::*;
use som_lexer::Token;
use som_parser_core::combinators::*;
use som_parser_core::Parser;

macro_rules! opaque {
    ($expr:expr) => {{
        move |input: &'a [Token]| $expr.parse(input)
    }};
}

/// A parser that expects to be nothing left in its input.
pub fn eof<'a>() -> impl Parser<(), &'a [Token]> {
    move |input: &'a [Token]| {
        if input.is_empty() {
            Some(((), input))
        } else {
            None
        }
    }
}

pub fn exact<'a>(ch: Token) -> impl Parser<(), &'a [Token]> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        if *head == ch {
            Some(((), tail))
        } else {
            None
        }
    }
}

pub fn exact_ident<'a, 'b: 'a>(string: &'b str) -> impl Parser<(), &'a [Token]> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Identifier(ref ident) if ident.as_str() == string => Some(((), tail)),
            _ => None,
        }
    }
}

pub fn big_integer<'a>() -> impl Parser<String, &'a [Token]> {
    move |input: &'a [Token]| {
        let (sign, input) = optional(exact(Token::Minus)).parse(input)?;
        let sign = if sign.is_some() { "-" } else { "" };

        let (head, tail) = input.split_first()?;
        match head {
            Token::LitBigInteger(value) => Some((format!("{}{}", sign, value), tail)),
            _ => None,
        }
    }
}

pub fn integer<'a>() -> impl Parser<i32, &'a [Token]> {
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

pub fn double<'a>() -> impl Parser<f64, &'a [Token]> {
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

pub fn single_operator<'a>() -> impl Parser<&'static str, &'a [Token]> {
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

pub fn operator_sequence<'a>() -> impl Parser<String, &'a [Token]> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::OperatorSequence(seq) => Some((seq.clone(), tail)),
            _ => None,
        }
    }
}

pub fn operator<'a>() -> impl Parser<String, &'a [Token]> {
    single_operator().map(String::from).or(operator_sequence())
}

pub fn identifier<'a>() -> impl Parser<String, &'a [Token]> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Identifier(value) => Some((value.clone(), tail)),
            _ => None,
        }
    }
}

pub fn string<'a>() -> impl Parser<String, &'a [Token]> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::LitString(value) => Some((value.clone(), tail)),
            _ => None,
        }
    }
}

pub fn symbol<'a>() -> impl Parser<String, &'a [Token]> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::LitSymbol(value) => Some((value.clone(), tail)),
            _ => None,
        }
    }
}

pub fn array<'a>() -> impl Parser<Vec<Literal>, &'a [Token]> {
    move |input: &'a [Token]| {
        between(
            exact(Token::NewArray),
            many(literal()),
            exact(Token::EndTerm),
        )
        .parse(input)
    }
}

pub fn literal<'a>() -> impl Parser<Literal, &'a [Token]> {
    (double().map(Literal::Double))
        .or(integer().map(Literal::Integer))
        .or(big_integer().map(Literal::BigInteger))
        .or(string().map(Literal::String))
        .or(symbol().map(Literal::Symbol))
        .or(array().map(Literal::Array))
}

pub fn keyword<'a>() -> impl Parser<String, &'a [Token]> {
    move |input: &'a [Token]| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Keyword(value) => Some((value.clone(), tail)),
            _ => None,
        }
    }
}

pub fn unary_send<'a>() -> impl Parser<Expression, &'a [Token]> {
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

pub fn binary_send<'a>() -> impl Parser<Expression, &'a [Token]> {
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

pub fn positional_send<'a>() -> impl Parser<Expression, &'a [Token]> {
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

pub fn body<'a>() -> impl Parser<Body, &'a [Token]> {
    sep_by(exact(Token::Period), exit().or(statement()))
        .and(optional(exact(Token::Period)))
        .map(|(exprs, stopped)| Body {
            exprs,
            full_stopped: stopped.is_some(),
        })
}

pub fn locals<'a>() -> impl Parser<Vec<String>, &'a [Token]> {
    between(exact(Token::Or), many(identifier()), exact(Token::Or))
}

pub fn parameter<'a>() -> impl Parser<String, &'a [Token]> {
    exact(Token::Colon).and_right(identifier())
}

pub fn parameters<'a>() -> impl Parser<Vec<String>, &'a [Token]> {
    some(parameter()).and_left(exact(Token::Or))
}

pub fn block<'a>() -> impl Parser<Expression, &'a [Token]> {
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

pub fn term<'a>() -> impl Parser<Expression, &'a [Token]> {
    between(
        exact(Token::NewTerm),
        assignment().or(expression()),
        exact(Token::EndTerm),
    )
}

pub fn exit<'a>() -> impl Parser<Expression, &'a [Token]> {
    exact(Token::Exit)
        .and_right(statement())
        .map(|expr| Expression::Exit(Box::new(expr)))
}

pub fn expression<'a>() -> impl Parser<Expression, &'a [Token]> {
    positional_send()
}

pub fn primary<'a>() -> impl Parser<Expression, &'a [Token]> {
    (identifier().map(Expression::Reference))
        .or(term())
        .or(block())
        .or(literal().map(Expression::Literal))
}

pub fn assignment<'a>() -> impl Parser<Expression, &'a [Token]> {
    identifier()
        .and_left(exact(Token::Assign))
        .and(opaque!(statement()))
        .map(|(name, expr)| Expression::Assignment(name, Box::new(expr)))
}

pub fn statement<'a>() -> impl Parser<Expression, &'a [Token]> {
    assignment().or(expression())
}

pub fn primitive<'a>() -> impl Parser<MethodBody, &'a [Token]> {
    exact(Token::Primitive).map(|_| MethodBody::Primitive)
}

pub fn method_body<'a>() -> impl Parser<MethodBody, &'a [Token]> {
    between(
        exact(Token::NewTerm),
        default(locals()).and(body()),
        exact(Token::EndTerm),
    )
    .map(|(locals, body)| MethodBody::Body { locals, body })
}

pub fn unary_method_def<'a>() -> impl Parser<MethodDef, &'a [Token]> {
    identifier()
        .and_left(exact(Token::Equal))
        .and(primitive().or(method_body()))
        .map(|(signature, body)| MethodDef {
            kind: MethodKind::Unary,
            signature,
            body,
        })
}

pub fn positional_method_def<'a>() -> impl Parser<MethodDef, &'a [Token]> {
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

pub fn operator_method_def<'a>() -> impl Parser<MethodDef, &'a [Token]> {
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

pub fn method_def<'a>() -> impl Parser<MethodDef, &'a [Token]> {
    unary_method_def()
        .or(positional_method_def())
        .or(operator_method_def())
}

pub fn class_def<'a>() -> impl Parser<ClassDef, &'a [Token]> {
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

pub fn file<'a>() -> impl Parser<ClassDef, &'a [Token]> {
    class_def().and_left(eof())
}
