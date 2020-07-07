use crate::Parser;

/// Represents a value of either type A (Left) or type B (Right).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Either<A, B> {
    Left(A),
    Right(B),
}

/// Transforms a parser into a non-consuming one, allowing to parse ahead without consuming anything.
pub fn peek<A, I: Clone>(mut parser: impl Parser<A, I>) -> impl Parser<A, I> {
    move |input: I| {
        let (value, _) = parser.parse(input.clone())?;
        Some((value, input))
    }
}

/// Runs the given parser, fails if it succeeded, and succeeds otherwise.
pub fn not<A, I: Clone>(mut parser: impl Parser<A, I>) -> impl Parser<(), I> {
    move |input: I| match parser.parse(input.clone()) {
        Some(_) => None,
        None => Some(((), input)),
    }
}

/// Sequences two parsers, one after the other, collecting both results.
pub fn sequence<A, B, I>(
    mut fst: impl Parser<A, I>,
    mut snd: impl Parser<B, I>,
) -> impl Parser<(A, B), I> {
    // equivalent to: `fst.and(snd)`
    move |input: I| {
        let (a, input) = fst.parse(input)?;
        let (b, input) = snd.parse(input)?;
        Some(((a, b), input))
    }
}

/// Tries to apply the first parser, if it fails, it tries to apply the second parser.
pub fn alternative<A, I: Clone>(
    mut fst: impl Parser<A, I>,
    mut snd: impl Parser<A, I>,
) -> impl Parser<A, I> {
    move |input: I| fst.parse(input.clone()).or_else(|| snd.parse(input))
}

/// Same as `either`, but allows for different output types for the parsers.
pub fn either<A, B, I: Clone>(
    mut fst: impl Parser<A, I>,
    mut snd: impl Parser<B, I>,
) -> impl Parser<Either<A, B>, I> {
    move |input: I| {
        if let Some((a, input)) = fst.parse(input.clone()) {
            Some((Either::Left(a), input))
        } else if let Some((b, input)) = snd.parse(input) {
            Some((Either::Right(b), input))
        } else {
            None
        }
    }
}

/// Tries to apply a parser, or fallback to a constant value (making it an always-succeeding parser).
pub fn fallback<A: Clone, I: Clone>(def: A, mut parser: impl Parser<A, I>) -> impl Parser<A, I> {
    move |input: I| {
        parser
            .parse(input.clone())
            .or_else(|| Some((def.clone(), input)))
    }
}

/// Tries to apply a parser, or fallback to its default value (making it an always-succeeding parser).
pub fn default<A: Default, I: Clone>(parser: impl Parser<A, I>) -> impl Parser<A, I> {
    optional(parser).map(Option::unwrap_or_default)
}

/// Tries every parser in a slice, from left to right, and returns the output of the first succeeding one.
pub fn any<'a, A, I: Clone>(parsers: &'a mut [impl Parser<A, I>]) -> impl Parser<A, I> + 'a {
    move |input: I| {
        parsers
            .iter_mut()
            .find_map(|parser| parser.parse(input.clone()))
    }
}

/// Applies every parser in a slice, from left to right, and returns the output from all of them.
/// If one parser fails, the whole sequence is considered failed.
pub fn all<'a, A, I>(parsers: &'a mut [impl Parser<A, I>]) -> impl Parser<Vec<A>, I> + 'a {
    move |input: I| {
        let output = Vec::<A>::with_capacity(parsers.len());
        parsers
            .iter_mut()
            .try_fold((output, input), |(mut output, input), parser| {
                let (value, input) = parser.parse(input)?;
                output.push(value);
                Some((output, input))
            })
    }
}

/// Tries to apply a parser, but fails gracefully (with an `Option` output).
pub fn optional<A, I: Clone>(mut parser: impl Parser<A, I>) -> impl Parser<Option<A>, I> {
    move |input: I| {
        if let Some((value, input)) = parser.parse(input.clone()) {
            Some((Some(value), input))
        } else {
            Some((None, input))
        }
    }
}

/// Applies a parser zero or more times.
pub fn many<A, I: Clone>(mut parser: impl Parser<A, I>) -> impl Parser<Vec<A>, I> {
    move |mut input: I| {
        let mut output = Vec::<A>::new();
        while let Some((value, next)) = parser.parse(input.clone()) {
            input = next;
            output.push(value);
        }
        Some((output, input))
    }
}

/// Applies a parser one or more times.
pub fn some<A, I: Clone>(mut parser: impl Parser<A, I>) -> impl Parser<Vec<A>, I> {
    move |input: I| {
        let (value, mut input) = parser.parse(input)?;
        let mut output = vec![value];
        while let Some((value, next)) = parser.parse(input.clone()) {
            input = next;
            output.push(value);
        }
        Some((output, input))
    }
}

/// Parses something that is enclosed between two other things.
pub fn between<A, B, C, I>(
    mut before: impl Parser<A, I>,
    mut within: impl Parser<B, I>,
    mut after: impl Parser<C, I>,
) -> impl Parser<B, I> {
    move |input: I| {
        let (_, input) = before.parse(input)?;
        let (value, input) = within.parse(input)?;
        let (_, input) = after.parse(input)?;
        Some((value, input))
    }
}

/// Parses zero or more things, separated by an arbitrary delimiter.
pub fn sep_by<A, B, I: Clone>(
    mut delim: impl Parser<A, I>,
    mut within: impl Parser<B, I>,
) -> impl Parser<Vec<B>, I> {
    move |input: I| {
        let mut output = Vec::<B>::new();
        if let Some((value, mut input)) = within.parse(input.clone()) {
            output.push(value);
            while let Some((value, next)) = delim
                .parse(input.clone())
                .and_then(|(_, input)| within.parse(input))
            {
                input = next;
                output.push(value);
            }
            Some((output, input))
        } else {
            Some((output, input))
        }
    }
}

/// Parses one or more things, separated by an arbitrary delimiter.
pub fn sep_by1<A, B, I: Clone>(
    mut delim: impl Parser<A, I>,
    mut within: impl Parser<B, I>,
) -> impl Parser<Vec<B>, I> {
    move |input: I| {
        let mut output = Vec::<B>::new();
        let (value, mut input) = within.parse(input)?;
        output.push(value);
        while let Some((value, next)) = delim
            .parse(input.clone())
            .and_then(|(_, input)| within.parse(input))
        {
            input = next;
            output.push(value);
        }
        Some((output, input))
    }
}

/// Transforms the output value of a parser.
pub fn map<A, B, I>(mut parser: impl Parser<A, I>, func: impl Fn(A) -> B) -> impl Parser<B, I> {
    move |input: I| {
        let (value, input) = parser.parse(input)?;
        Some((func(value), input))
    }
}
