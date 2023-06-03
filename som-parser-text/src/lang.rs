use som_core::ast::*;
use som_parser_core::combinators::*;
use som_parser_core::Parser;

pub fn eof<'a>() -> impl Parser<(), &'a [char]> {
    move |input: &'a [char]| {
        if input.is_empty() {
            Some(((), input))
        } else {
            None
        }
    }
}

pub fn exact<'a>(ch: char) -> impl Parser<char, &'a [char]> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        if *head == ch {
            Some((*head, tail))
        } else {
            None
        }
    }
}

pub fn exact_str<'a, 'b: 'a>(string: &'b str) -> impl Parser<(), &'a [char]> {
    move |mut input: &'a [char]| {
        for mut parser in string.chars().map(exact) {
            let (_, new_input) = parser.parse(input)?;
            input = new_input;
        }
        Some(((), input))
    }
}

pub fn one_of<'a, 'b: 'a>(string: &'b str) -> impl Parser<char, &'a [char]> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        if let Some(ch) = string.chars().find(|ch| ch == head) {
            Some((ch, tail))
        } else {
            None
        }
    }
}

pub fn not_exact<'a>(ch: char) -> impl Parser<char, &'a [char]> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        if *head != ch {
            Some((*head, tail))
        } else {
            None
        }
    }
}

pub fn whitespace<'a>() -> impl Parser<char, &'a [char]> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        if head.is_whitespace() {
            Some((*head, tail))
        } else {
            None
        }
    }
}

pub fn comment<'a>() -> impl Parser<String, &'a [char]> {
    between(exact('"'), many(not_exact('"')), exact('"')).map(|chars| chars.into_iter().collect())
}

pub fn separator<'a>() -> impl Parser<(), &'a [char]> {
    exact_str("----").and(many(exact('-'))).map(|_| ())
}

pub fn spacing<'a>() -> impl Parser<(), &'a [char]> {
    whitespace().map(|_| ()).or(comment().map(|_| ()))
}

pub fn digit<'a>() -> impl Parser<i64, &'a [char]> {
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

pub fn integer<'a>() -> impl Parser<i64, &'a [char]> {
    optional(exact('-'))
        .and(some(digit()))
        .map(|(sign, digits)| {
            let sign = if sign.is_some() { -1 } else { 1 };
            digits.into_iter().fold(0, |acc, el| acc * 10 + el) * sign
        })
}

pub fn double<'a>() -> impl Parser<f64, &'a [char]> {
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

pub fn lower<'a>() -> impl Parser<char, &'a [char]> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        if head.is_lowercase() {
            Some((*head, tail))
        } else {
            None
        }
    }
}

pub fn upper<'a>() -> impl Parser<char, &'a [char]> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        if head.is_uppercase() {
            Some((*head, tail))
        } else {
            None
        }
    }
}

pub fn digitc<'a>() -> impl Parser<char, &'a [char]> {
    move |input: &'a [char]| {
        let (head, tail) = input.split_first()?;
        match head {
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => Some((*head, tail)),
            _ => None,
        }
    }
}

pub fn single_operator<'a>() -> impl Parser<char, &'a [char]> {
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

pub fn operator<'a>() -> impl Parser<String, &'a [char]> {
    some(single_operator()).map(|chars| chars.into_iter().collect())
}

pub fn identifier<'a>() -> impl Parser<String, &'a [char]> {
    (lower().or(upper()))
        .and(many(lower().or(upper()).or(digitc()).or(exact('_'))))
        .map(|(fst, tail)| std::iter::once(fst).chain(tail).collect())
}

pub fn string<'a>() -> impl Parser<String, &'a [char]> {
    move |input: &'a [char]| {
        let single_char = exact('\\')
            .and(one_of("tbnrf\'\\"))
            .map(|(a, b)| match b {
                't' => vec!['\t'],
                'b' => vec!['\x08'],
                'n' => vec!['\n'],
                'r' => vec!['\r'],
                'f' => vec!['\x12'],
                '\'' => vec!['\''],
                '\\' => vec!['\\'],
                '0' => vec!['\0'],
                _ => vec![a, b],
            })
            .or(not_exact('\'').map(|a| vec![a]));
        let (value, input) = between(exact('\''), many(single_char), exact('\'')).parse(input)?;
        let value: String = value.into_iter().flatten().collect();
        Some((value, input))
    }
}

pub fn symbol<'a>() -> impl Parser<String, &'a [char]> {
    exact('#').and_right(
        (some(keyword()).map(|words| words.into_iter().collect()))
            .or(identifier())
            .or(string())
            .or(operator()),
    )
}

pub fn array<'a>() -> impl Parser<Vec<Literal>, &'a [char]> {
    move |input: &'a [char]| {
        let (_, input) = exact('#').parse(input)?;
        let (_, input) = exact('(').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (literals, input) = sep_by(some(spacing()), literal()).parse(input)?;
        let (_, input) = exact(')').parse(input)?;

        Some((literals, input))
    }
}

pub fn literal<'a>() -> impl Parser<Literal, &'a [char]> {
    (double().map(Literal::Double))
        .or(integer().map(Literal::Integer))
        .or(string().map(Literal::String))
        .or(symbol().map(Literal::Symbol))
        .or(array().map(Literal::Array))
}

pub fn keyword<'a>() -> impl Parser<String, &'a [char]> {
    (lower().or(upper()))
        .and(many(lower().or(upper()).or(digitc()).or(exact('_'))))
        .and(exact(':'))
        .map(|((fst, tail), colon)| {
            std::iter::once(fst)
                .chain(tail)
                .chain(std::iter::once(colon))
                .collect()
        })
}

pub fn unary_send<'a>() -> impl Parser<Expression, &'a [char]> {
    move |input: &'a [char]| {
        let (receiver, input) = primary().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (signatures, input) = sep_by(
            many(spacing()),
            identifier().and_left(not(peek(exact(':')))),
        )
        .parse(input)?;

        let expr = signatures
            .into_iter()
            .fold(receiver, |receiver, signature| {
                Expression::Message(Message {
                    receiver: Box::new(receiver),
                    signature,
                    values: Vec::new(),
                })
            });

        Some((expr, input))
    }
}

pub fn binary_send<'a>() -> impl Parser<Expression, &'a [char]> {
    move |input: &'a [char]| {
        let (lhs, input) = unary_send().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (operands, input) = many(
            operator()
                .and_left(many(spacing()))
                .and(unary_send().map(Box::new).and_left(many(spacing()))),
        )
        .parse(input)?;

        let expr = operands.into_iter().fold(lhs, |lhs, (op, rhs)| {
            Expression::BinaryOp(BinaryOp {
                lhs: Box::new(lhs),
                op,
                rhs,
            })
        });

        Some((expr, input))
    }
}

pub fn positional_send<'a>() -> impl Parser<Expression, &'a [char]> {
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
            let (signature, values) = pairs.into_iter().unzip();
            let message = Expression::Message(Message {
                receiver: Box::new(receiver),
                signature,
                values,
            });
            Some((message, input))
        }
    }
}

pub fn locals<'a>() -> impl Parser<Vec<String>, &'a [char]> {
    move |input: &'a [char]| {
        let (_, input) = exact('|').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (locals, input) = sep_by(some(whitespace()), identifier()).parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact('|').parse(input)?;
        Some((locals, input))
    }
}

pub fn body<'a>() -> impl Parser<Body, &'a [char]> {
    move |input: &'a [char]| {
        let (exprs, input) = sep_by(
            exact('.').and(many(spacing())),
            exit().or(statement()).and_left(many(spacing())),
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

pub fn block<'a>() -> impl Parser<Expression, &'a [char]> {
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
        let (parameters, input) = default(parameters.and_left(many(spacing()))).parse(input)?;
        let (locals, input) = default(locals().and_left(many(spacing()))).parse(input)?;
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

pub fn term<'a>() -> impl Parser<Expression, &'a [char]> {
    move |input: &'a [char]| {
        let (_, input) = exact('(').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (expr, input) = assignment().or(expression()).parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact(')').parse(input)?;
        Some((expr, input))
    }
}

pub fn exit<'a>() -> impl Parser<Expression, &'a [char]> {
    move |input: &'a [char]| {
        let (_, input) = exact('^').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (expr, input) = statement().parse(input)?;

        let exit = Expression::Exit(Box::new(expr));
        Some((exit, input))
    }
}

pub fn expression<'a>() -> impl Parser<Expression, &'a [char]> {
    positional_send()
}

pub fn primary<'a>() -> impl Parser<Expression, &'a [char]> {
    (identifier().map(Expression::Reference))
        .or(term())
        .or(block())
        .or(literal().map(Expression::Literal))
}

pub fn assignment<'a>() -> impl Parser<Expression, &'a [char]> {
    move |input: &'a [char]| {
        let (name, input) = identifier().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact_str(":=").parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (expr, input) = statement().parse(input)?;

        Some((Expression::Assignment(name, Box::new(expr)), input))
    }
}

pub fn statement<'a>() -> impl Parser<Expression, &'a [char]> {
    assignment().or(expression())
}

pub fn primitive<'a>() -> impl Parser<MethodBody, &'a [char]> {
    exact_str("primitive").map(|_| MethodBody::Primitive)
}

pub fn method_body<'a>() -> impl Parser<MethodBody, &'a [char]> {
    move |input: &'a [char]| {
        let (_, input) = exact('(').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (locals, input) = default(locals().and_left(many(spacing()))).parse(input)?;
        let (body, input) = body().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact(')').parse(input)?;

        let body = MethodBody::Body { locals, body };
        Some((body, input))
    }
}

pub fn unary_method_def<'a>() -> impl Parser<MethodDef, &'a [char]> {
    move |input: &'a [char]| {
        let (signature, input) = identifier().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact('=').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (body, input) = primitive().or(method_body()).parse(input)?;

        let method_def = MethodDef {
            kind: MethodKind::Unary,
            signature,
            body,
        };
        Some((method_def, input))
    }
}

pub fn positional_method_def<'a>() -> impl Parser<MethodDef, &'a [char]> {
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

        let (signature, parameters) = pairs.into_iter().unzip();
        let method_def = MethodDef {
            kind: MethodKind::Positional { parameters },
            signature,
            body,
        };
        Some((method_def, input))
    }
}

pub fn operator_method_def<'a>() -> impl Parser<MethodDef, &'a [char]> {
    move |input: &'a [char]| {
        let (signature, input) = operator().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (rhs, input) = identifier().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = exact('=').parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (body, input) = primitive().or(method_body()).parse(input)?;

        let method_def = MethodDef {
            kind: MethodKind::Operator { rhs },
            signature,
            body,
        };
        Some((method_def, input))
    }
}

pub fn method_def<'a>() -> impl Parser<MethodDef, &'a [char]> {
    unary_method_def()
        .or(positional_method_def())
        .or(operator_method_def())
}

pub fn class_def<'a>() -> impl Parser<ClassDef, &'a [char]> {
    let mut class_section = move |input: &'a [char]| {
        let (locals, input) = default(locals().and_left(many(spacing()))).parse(input)?;
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
        let (super_class, input) = optional(identifier().and_left(many(spacing()))).parse(input)?;
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

pub fn file<'a>() -> impl Parser<ClassDef, &'a [char]> {
    move |input: &'a [char]| {
        let (_, input) = many(spacing()).parse(input)?;
        let (class_def, input) = class_def().parse(input)?;
        let (_, input) = many(spacing()).parse(input)?;
        let (_, input) = eof().parse(input)?;
        Some((class_def, input))
    }
}
