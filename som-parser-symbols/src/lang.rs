use som_core::ast::*;
use som_lexer::Symbol;

use crate::combinators::*;
use crate::parser::Parser;

#[allow(unused)]
pub(crate) fn fst<A, B>((a, _): (A, B)) -> A {
    a
}
#[allow(unused)]
pub(crate) fn snd<A, B>((_, b): (A, B)) -> B {
    b
}

/// A parser that expects to be nothing left in its input.
pub fn eof<'a>() -> impl Parser<'a, ()> {
    move |input: &'a [Symbol]| {
        if input.is_empty() {
            Some(((), input))
        } else {
            None
        }
    }
}

pub fn exact<'a>(ch: Symbol) -> impl Parser<'a, ()> {
    move |input: &'a [Symbol]| {
        let (head, tail) = input.split_first()?;
        if *head == ch {
            Some(((), tail))
        } else {
            None
        }
    }
}

pub fn exact_ident<'a, 'b: 'a>(string: &'b str) -> impl Parser<'a, ()> {
    move |input: &'a [Symbol]| {
        let (head, tail) = input.split_first()?;
        match head {
            Symbol::Identifier(ref ident) if ident.as_str() == string => Some(((), tail)),
            _ => None,
        }
    }
}

pub fn integer<'a>() -> impl Parser<'a, i64> {
    move |input: &'a [Symbol]| {
        let (sign, input) = optional(exact(Symbol::Minus)).parse(input)?;
        let sign = if sign.is_some() { -1 } else { 1 };

        let (head, tail) = input.split_first()?;
        match head {
            Symbol::LitInteger(value) => Some((*value * sign, tail)),
            _ => None,
        }
    }
}

pub fn double<'a>() -> impl Parser<'a, f64> {
    move |input: &'a [Symbol]| {
        let (sign, input) = optional(exact(Symbol::Minus)).parse(input)?;
        let sign = if sign.is_some() { -1.0 } else { 1.0 };

        let (head, tail) = input.split_first()?;
        match head {
            Symbol::LitDouble(value) => Some((*value * sign, tail)),
            _ => None,
        }
    }
}

pub fn single_operator<'a>() -> impl Parser<'a, char> {
    move |input: &'a [Symbol]| {
        let (head, tail) = input.split_first()?;
        match head {
            Symbol::Not => Some(('~', tail)),
            Symbol::And => Some(('&', tail)),
            Symbol::Or => Some(('|', tail)),
            Symbol::Star => Some(('*', tail)),
            Symbol::Div => Some(('/', tail)),
            Symbol::Mod => Some(('\\', tail)),
            Symbol::Plus => Some(('+', tail)),
            Symbol::Equal => Some(('=', tail)),
            Symbol::More => Some(('>', tail)),
            Symbol::Less => Some(('<', tail)),
            Symbol::Comma => Some((',', tail)),
            Symbol::At => Some(('@', tail)),
            Symbol::Per => Some(('%', tail)),
            Symbol::Minus => Some(('-', tail)),
            _ => None,
        }
    }
}

pub fn operator<'a>() -> impl Parser<'a, String> {
    map(some(single_operator()), |chars| chars.into_iter().collect())
}

pub fn identifier<'a>() -> impl Parser<'a, String> {
    move |input: &'a [Symbol]| {
        let (head, tail) = input.split_first()?;
        match head {
            Symbol::Identifier(value) => Some((value.clone(), tail)),
            _ => None,
        }
    }
}

pub fn string<'a>() -> impl Parser<'a, String> {
    move |input: &'a [Symbol]| {
        let (head, tail) = input.split_first()?;
        match head {
            Symbol::LitString(value) => Some((value.clone(), tail)),
            _ => None,
        }
    }
}

pub fn symbol<'a>() -> impl Parser<'a, String> {
    move |input: &'a [Symbol]| {
        let (head, tail) = input.split_first()?;
        match head {
            Symbol::LitSymbol(value) => Some((value.clone(), tail)),
            _ => None,
        }
    }
}

pub fn array<'a>() -> impl Parser<'a, Vec<Literal>> {
    move |input: &'a [Symbol]| {
        let (_, input) = exact(Symbol::NewArray).parse(input)?;
        let (literals, input) = many(literal()).parse(input)?;
        let (_, input) = exact(Symbol::EndTerm).parse(input)?;

        Some((literals, input))
    }
}

pub fn literal<'a>() -> impl Parser<'a, Literal> {
    map(double(), Literal::Double)
        .or(map(integer(), Literal::Integer))
        .or(map(string(), Literal::String))
        .or(map(symbol(), Literal::Symbol))
        .or(map(array(), Literal::Array))
}

pub fn keyword<'a>() -> impl Parser<'a, String> {
    move |input: &'a [Symbol]| {
        let (head, tail) = input.split_first()?;
        match head {
            Symbol::Keyword(value) => Some((value.clone(), tail)),
            _ => None,
        }
    }
}

pub fn unary_send<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a [Symbol]| {
        let (mut receiver, input) = primary().parse(input)?;
        let (signatures, input) = many(identifier()).parse(input)?;

        for signature in signatures {
            receiver = Expression::Message(Message {
                receiver: Box::new(receiver),
                signature,
                values: Vec::new(),
            });
        }
        Some((receiver, input))
    }
}

pub fn binary_send<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a [Symbol]| {
        let (mut lhs, input) = unary_send().parse(input)?;
        let (operands, input) = many(operator().and(map(unary_send(), Box::new))).parse(input)?;

        for (op, rhs) in operands {
            lhs = Expression::BinaryOp(BinaryOp {
                lhs: Box::new(lhs),
                op,
                rhs,
            });
        }
        Some((lhs, input))
    }
}

pub fn positional_send<'a>() -> impl Parser<'a, Expression> {
    let parameters = move |input: &'a [Symbol]| {
        let (keyword, input) = keyword().parse(input)?;
        let (value, input) = binary_send().parse(input)?;
        Some(((keyword, value), input))
    };
    move |input: &'a [Symbol]| {
        let (receiver, input) = binary_send().parse(input)?;
        let (pairs, input) = many(parameters).parse(input)?;

        if pairs.is_empty() {
            Some((receiver, input))
        } else {
            let mut signature = String::new();
            let mut values = Vec::new();
            for (keyword, value) in pairs {
                signature.push_str(keyword.as_str());
                values.push(value);
            }
            let message = Expression::Message(Message {
                receiver: Box::new(receiver),
                signature,
                values,
            });
            Some((message, input))
        }
    }
}

pub fn body<'a>() -> impl Parser<'a, Body> {
    move |input: &'a [Symbol]| {
        let (exprs, input) = sep_by(exact(Symbol::Period), exit().or(statement())).parse(input)?;
        let (stopped, input) = optional(exact(Symbol::Period)).parse(input)?;

        let body = Body {
            exprs,
            full_stopped: stopped.is_some(),
        };
        Some((body, input))
    }
}

pub fn locals<'a>() -> impl Parser<'a, Vec<String>> {
    move |input: &'a [Symbol]| {
        let (_, input) = exact(Symbol::Or).parse(input)?;
        let (locals, input) = many(identifier()).parse(input)?;
        let (_, input) = exact(Symbol::Or).parse(input)?;
        Some((locals, input))
    }
}

pub fn parameter<'a>() -> impl Parser<'a, String> {
    map(exact(Symbol::Colon).and(identifier()), snd)
}

pub fn parameters<'a>() -> impl Parser<'a, Vec<String>> {
    map(some(parameter()).and(exact(Symbol::Or)), fst)
}

pub fn block<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a [Symbol]| {
        let (_, input) = exact(Symbol::NewBlock).parse(input)?;
        let (parameters, input) =
            map(optional(parameters()), Option::unwrap_or_default).parse(input)?;
        let (locals, input) = map(optional(locals()), Option::unwrap_or_default).parse(input)?;
        let (body, input) = body().parse(input)?;
        let (_, input) = exact(Symbol::EndBlock).parse(input)?;

        let block = Expression::Block(Block {
            parameters,
            locals,
            body,
        });
        Some((block, input))
    }
}

pub fn term<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a [Symbol]| {
        let (_, input) = exact(Symbol::NewTerm).parse(input)?;
        let (body, input) = body().parse(input)?;
        let (_, input) = exact(Symbol::EndTerm).parse(input)?;

        let term = Term { body };
        let term = Expression::Term(term);
        Some((term, input))
    }
}

pub fn exit<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a [Symbol]| {
        let (_, input) = exact(Symbol::Exit).parse(input)?;
        let (expr, input) = statement().parse(input)?;

        let exit = Expression::Exit(Box::new(expr));
        Some((exit, input))
    }
}

pub fn expression<'a>() -> impl Parser<'a, Expression> {
    positional_send()
}

pub fn primary<'a>() -> impl Parser<'a, Expression> {
    map(identifier(), Expression::Reference)
        .or(term())
        .or(block())
        .or(map(literal(), Expression::Literal))
}

pub fn assignment<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a [Symbol]| {
        let (name, input) = identifier().parse(input)?;
        let (_, input) = exact(Symbol::Assign).parse(input)?;
        let (expr, input) = statement().parse(input)?;

        Some((Expression::Assignment(name, Box::new(expr)), input))
    }
}

pub fn statement<'a>() -> impl Parser<'a, Expression> {
    assignment().or(expression())
}

pub fn primitive<'a>() -> impl Parser<'a, MethodBody> {
    map(exact(Symbol::Primitive), |_| MethodBody::Primitive)
}

pub fn method_body<'a>() -> impl Parser<'a, MethodBody> {
    move |input: &'a [Symbol]| {
        let (_, input) = exact(Symbol::NewTerm).parse(input)?;
        let (locals, input) = map(optional(locals()), Option::unwrap_or_default).parse(input)?;
        let (body, input) = body().parse(input)?;
        let (_, input) = exact(Symbol::EndTerm).parse(input)?;

        let body = MethodBody::Body { locals, body };
        Some((body, input))
    }
}

pub fn unary_method_def<'a>() -> impl Parser<'a, MethodDef> {
    move |input: &'a [Symbol]| {
        let (signature, input) = identifier().parse(input)?;
        let (_, input) = exact(Symbol::Equal).parse(input)?;
        let (body, input) = primitive().or(method_body()).parse(input)?;

        let method_def = MethodDef::Unary { signature, body };
        Some((method_def, input))
    }
}

pub fn positional_method_def<'a>() -> impl Parser<'a, MethodDef> {
    let parameter = move |input: &'a [Symbol]| {
        let (keyword, input) = keyword().parse(input)?;
        let (value, input) = identifier().parse(input)?;
        Some(((keyword, value), input))
    };
    move |input: &'a [Symbol]| {
        let (pairs, input) = some(parameter).parse(input)?;
        let (_, input) = exact(Symbol::Equal).parse(input)?;
        let (body, input) = primitive().or(method_body()).parse(input)?;

        let mut signature = String::new();
        let mut parameters = Vec::new();
        for (keyword, value) in pairs {
            signature.push_str(keyword.as_str());
            parameters.push(value);
        }
        let method_def = MethodDef::Positional {
            signature,
            parameters,
            body,
        };
        Some((method_def, input))
    }
}

pub fn operator_method_def<'a>() -> impl Parser<'a, MethodDef> {
    move |input: &'a [Symbol]| {
        let (op, input) = operator().parse(input)?;
        let (rhs, input) = identifier().parse(input)?;
        let (_, input) = exact(Symbol::Equal).parse(input)?;
        let (body, input) = primitive().or(method_body()).parse(input)?;

        let method_def = MethodDef::Operator { op, rhs, body };
        Some((method_def, input))
    }
}

pub fn method_def<'a>() -> impl Parser<'a, MethodDef> {
    unary_method_def()
        .or(positional_method_def())
        .or(operator_method_def())
}

pub fn class_def<'a>() -> impl Parser<'a, ClassDef> {
    let class_section = move |input: &'a [Symbol]| {
        let (locals, input) = map(optional(locals()), Option::unwrap_or_default).parse(input)?;
        let (methods, input) = many(method_def()).parse(input)?;
        Some(((locals, methods), input))
    };
    move |input: &'a [Symbol]| {
        let (name, input) = identifier().parse(input)?;
        let (_, input) = exact(Symbol::Equal).parse(input)?;
        let (super_class, input) = optional(identifier()).parse(input)?;
        let (_, input) = exact(Symbol::NewTerm).parse(input)?;
        let ((instance_locals, instance_methods), input) = class_section.parse(input)?;
        let (static_section, input) =
            optional(map(exact(Symbol::Separator).and(class_section), snd)).parse(input)?;
        let (_, input) = exact(Symbol::EndTerm).parse(input)?;

        let (static_locals, static_methods) = static_section.unwrap_or((Vec::new(), Vec::new()));
        let class_def = ClassDef {
            name,
            super_class,
            instance_locals,
            instance_methods,
            static_locals,
            static_methods,
        };
        Some((class_def, input))
    }
}

pub fn file<'a>() -> impl Parser<'a, ClassDef> {
    map(class_def().and(eof()), fst)
}
