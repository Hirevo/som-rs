use som_core::ast::*;

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

pub fn eof<'a>() -> impl Parser<'a, ()> {
    move |input: &'a [char]| {
        if input.is_empty() {
            Some(((), input))
        } else {
            None
        }
    }
}

pub fn exact<'a>(ch: char) -> impl Parser<'a, char> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        if *head == ch {
            Some((*head, tail))
        } else {
            None
        }
    }
}

pub fn exact_str<'a, 'b: 'a>(string: &'b str) -> impl Parser<'a, ()> {
    move |mut input: &'a [char]| {
        for parser in string.chars().map(exact) {
            let (_, new_input) = parser.parse(input)?;
            input = new_input;
        }
        Some(((), input))
    }
}

pub fn one_of<'a, 'b: 'a>(string: &'b str) -> impl Parser<'a, char> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        if let Some(ch) = string.chars().find(|ch| ch == head) {
            Some((ch, tail))
        } else {
            None
        }
    }
}

pub fn not_exact<'a>(ch: char) -> impl Parser<'a, char> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        if *head != ch {
            Some((*head, tail))
        } else {
            None
        }
    }
}

pub fn whitespace<'a>() -> impl Parser<'a, char> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        if head.is_whitespace() {
            Some((*head, tail))
        } else {
            None
        }
    }
}

pub fn comment<'a>() -> impl Parser<'a, String> {
    map(
        between(exact('"'), many(not_exact('"')), exact('"')),
        |chars| chars.into_iter().collect(),
    )
}

pub fn separator<'a>() -> impl Parser<'a, ()> {
    map(
        exact('-')
            .and(exact('-'))
            .and(exact('-'))
            .and(exact('-'))
            .and(many(exact('-'))),
        |_| (),
    )
}

pub fn spacing<'a>() -> impl Parser<'a, ()> {
    map(whitespace(), |_| ()).or(map(comment(), |_| ()))
}

pub fn digit<'a>() -> impl Parser<'a, i64> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        match head {
            '0' => Some((0, tail)),
            '1' => Some((1, tail)),
            '2' => Some((2, tail)),
            '3' => Some((3, tail)),
            '4' => Some((4, tail)),
            '5' => Some((5, tail)),
            '6' => Some((6, tail)),
            '7' => Some((7, tail)),
            '8' => Some((8, tail)),
            '9' => Some((9, tail)),
            _ => None,
        }
    }
}

pub fn integer<'a>() -> impl Parser<'a, i64> {
    map(optional(exact('-')).and(some(digit())), |(sign, digits)| {
        let sign = if sign.is_some() { -1 } else { 1 };
        digits.into_iter().fold(0, |acc, el| acc * 10 + el) * sign
    })
}

pub fn double<'a>() -> impl Parser<'a, f64> {
    move |input: &'a [char]| {
        let (sign, input) = optional(exact('-')).parse(input)?;
        let sign = if sign.is_some() { "-" } else { "" };
        let (int_part, input) = integer().parse(input)?;
        let (_, input) = exact('.').parse(input)?;
        let (dec_part, input) = integer().parse(input)?;
        let number = format!("{}{}.{}", sign, int_part, dec_part);
        let number = number.parse().ok()?;
        Some((number, input))
    }
}

pub fn lower<'a>() -> impl Parser<'a, char> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        if head.is_lowercase() {
            Some((*head, tail))
        } else {
            None
        }
    }
}

pub fn upper<'a>() -> impl Parser<'a, char> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        if head.is_uppercase() {
            Some((*head, tail))
        } else {
            None
        }
    }
}

pub fn digitc<'a>() -> impl Parser<'a, char> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        match head {
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => Some((*head, tail)),
            _ => None,
        }
    }
}

pub fn single_operator<'a>() -> impl Parser<'a, char> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        match head {
            '~' | '&' | '|' | '*' | '/' | '\\' | '+' | '=' | '>' | '<' | ',' | '@' | '%' | '-' => {
                Some((*head, tail))
            }
            _ => None,
        }
    }
}

pub fn operator<'a>() -> impl Parser<'a, String> {
    map(some(single_operator()), |chars| chars.into_iter().collect())
}

pub fn identifier<'a>() -> impl Parser<'a, String> {
    move |input: &'a [char]| {
        let parser = lower()
            .or(upper())
            .and(many(lower().or(upper()).or(digitc()).or(exact('_'))));
        let ((fst, tail), input) = parser.parse(input)?;
        let ident: String = std::iter::once(fst).chain(tail.into_iter()).collect();
        Some((ident, input))
    }
}

// TODO: Correctly handle control characters (`\n`, `\t`, etc...)
pub fn string<'a>() -> impl Parser<'a, String> {
    move |input: &'a [char]| {
        let content = map(exact('\\').and(one_of("tbnrf\'\\")), |(a, b)| match b {
            't' => vec!['\t'],
            'b' => vec!['\x08'],
            'n' => vec!['\n'],
            'r' => vec!['\r'],
            'f' => vec!['\x12'],
            '\'' => vec!['\''],
            '\\' => vec!['\\'],
            _ => vec![a, b],
        })
        .or(map(not_exact('\''), |a| vec![a]));
        let parser = between(exact('\''), many(content), exact('\''));
        let (value, input) = parser.parse(input)?;
        let value: String = value.into_iter().flatten().collect();
        Some((value, input))
    }
}

pub fn symbol<'a>() -> impl Parser<'a, String> {
    move |input: &'a [char]| {
        let (_, input) = exact('#').parse(input)?;
        let (symbol, input) = map(some(keyword()), |words| words.into_iter().collect())
            .or(identifier())
            .or(string())
            .or(operator())
            .parse(input)?;

        Some((symbol, input))
    }
}

pub fn array<'a>() -> impl Parser<'a, Vec<Literal>> {
    move |input: &'a [char]| {
        let (_, input) = exact('#').parse(input)?;
        let (_, input) = exact('(').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (literals, input) = sep_by(some(spacing()), literal()).parse(input)?;
        let (_, input) = exact(')').parse(input)?;

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
    map(identifier().and(exact(':')), fst)
}

pub fn unary_send<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a [char]| {
        let (mut receiver, input) = primary().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (signatures, input) = sep_by(
            many(spacing()),
            map(identifier().and(peek(not(exact(':')))), fst),
        )
        .parse(input)?;

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
    move |input: &'a [char]| {
        let (mut lhs, input) = unary_send().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (operands, input) = many(
            map(operator().and(many(spacing())), fst)
                .and(map(map(unary_send(), Box::new).and(many(spacing())), fst)),
        )
        .parse(input)?;

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
    let parameters = move |input: &'a [char]| {
        let (_, input) = many(spacing()).parse(input)?;
        let (keyword, input) = keyword().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (value, input) = binary_send().parse(input)?;
        Some(((keyword, value), input))
    };
    move |input: &'a [char]| {
        let (receiver, input) = binary_send().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (pairs, input) = many(parameters).parse(input)?;

        if pairs.is_empty() {
            Some((receiver, input))
        } else {
            let mut signature = String::new();
            let mut values = Vec::new();
            for (keyword, value) in pairs {
                let keyword = format!("{}:", keyword);
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

pub fn locals<'a>() -> impl Parser<'a, Vec<String>> {
    move |input: &'a [char]| {
        let (_, input) = exact('|').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (locals, input) = sep_by(some(whitespace()), identifier()).parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact('|').parse(input)?;
        Some((locals, input))
    }
}

pub fn body<'a>() -> impl Parser<'a, Body> {
    move |input: &'a [char]| {
        let (exprs, input) = sep_by(
            exact('.').and(many(spacing())),
            map(exit().or(statement()).and(many(spacing())), fst),
        )
        .parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (stopped, input) = optional(exact('.')).parse(input)?;

        let body = Body {
            exprs,
            full_stopped: stopped.is_some(),
        };
        Some((body, input))
    }
}

pub fn block<'a>() -> impl Parser<'a, Expression> {
    let parameters = move |input: &'a [char]| {
        let parameter = move |input: &'a [char]| {
            let (_, input) = exact(':').parse(input)?;
            let (_, input) = many(spacing()).parse(input)?;
            let (parameter, input) = identifier().parse(input)?;
            Some((parameter, input))
        };
        let (parameters, input) = sep_by1(some(spacing()), parameter).parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact('|').parse(input)?;
        Some((parameters, input))
    };
    move |input: &'a [char]| {
        let (_, input) = exact('[').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (parameters, input) = map(
            optional(map(parameters.and(many(spacing())), fst)),
            Option::unwrap_or_default,
        )
        .parse(input)?;
        let (locals, input) = map(
            optional(map(locals().and(many(spacing())), fst)),
            Option::unwrap_or_default,
        )
        .parse(input)?;
        let (body, input) = body().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact(']').parse(input)?;

        let block = Expression::Block(Block {
            parameters,
            locals,
            body,
        });
        Some((block, input))
    }
}

pub fn term<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a [char]| {
        let (_, input) = exact('(').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (body, input) = body().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact(')').parse(input)?;

        let term = Term { body };
        let term = Expression::Term(term);
        Some((term, input))
    }
}

pub fn exit<'a>() -> impl Parser<'a, Expression> {
    move |input: &'a [char]| {
        let (_, input) = exact('^').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
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
    move |input: &'a [char]| {
        let (name, input) = identifier().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact_str(":=").parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (expr, input) = statement().parse(input)?;

        Some((Expression::Assignment(name, Box::new(expr)), input))
    }
}

pub fn statement<'a>() -> impl Parser<'a, Expression> {
    assignment().or(expression())
}

pub fn primitive<'a>() -> impl Parser<'a, MethodBody> {
    map(exact_str("primitive"), |_| MethodBody::Primitive)
}

pub fn method_body<'a>() -> impl Parser<'a, MethodBody> {
    move |input: &'a [char]| {
        let (_, input) = exact('(').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (locals, input) = map(
            optional(map(locals().and(many(spacing())), fst)),
            Option::unwrap_or_default,
        )
        .parse(input)?;
        let (body, input) = body().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact(')').parse(input)?;

        let body = MethodBody::Body { locals, body };
        Some((body, input))
    }
}

pub fn unary_method_def<'a>() -> impl Parser<'a, MethodDef> {
    move |input: &'a [char]| {
        let (signature, input) = identifier().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact('=').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (body, input) = primitive().or(method_body()).parse(input)?;

        let method_def = MethodDef::Unary { signature, body };
        Some((method_def, input))
    }
}

pub fn positional_method_def<'a>() -> impl Parser<'a, MethodDef> {
    let parameter = move |input: &'a [char]| {
        let (keyword, input) = keyword().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (value, input) = identifier().parse(input)?;
        Some(((keyword, value), input))
    };
    move |input: &'a [char]| {
        let (pairs, input) = sep_by1(many(spacing()), parameter).parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact('=').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (body, input) = primitive().or(method_body()).parse(input)?;

        let mut signature = String::new();
        let mut parameters = Vec::new();
        for (keyword, value) in pairs {
            let keyword = format!("{}:", keyword);
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
    move |input: &'a [char]| {
        let (op, input) = operator().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (rhs, input) = identifier().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact('=').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
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
    let class_section = move |input: &'a [char]| {
        let (locals, input) = map(
            optional(map(locals().and(many(spacing())), fst)),
            Option::unwrap_or_default,
        )
        .parse(input)?;
        let (methods, input) = sep_by(many(spacing()), method_def()).parse(input)?;
        Some(((locals, methods), input))
    };
    let static_section = move |input: &'a [char]| {
        let (_, input) = separator().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let ((locals, methods), input) = class_section.parse(input)?;
        Some(((locals, methods), input))
    };
    move |input: &'a [char]| {
        let (name, input) = identifier().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact('=').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (super_class, input) =
            optional(map(identifier().and(many(spacing())), fst)).parse(input)?;
        let (_, input) = exact('(').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let ((instance_locals, instance_methods), input) = class_section.parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (static_section, input) = optional(static_section).parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact(')').parse(input)?;

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
    move |input: &'a [char]| {
        let (_, input) = many(spacing()).parse(input)?;
        let (class_def, input) = class_def().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = eof().parse(input)?;
        Some((class_def, input))
    }
}
