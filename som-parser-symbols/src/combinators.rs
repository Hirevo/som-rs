use som_lexer::Symbol;

use crate::parser::Parser;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Either<A, B> {
    Left(A),
    Right(B),
}

pub fn peek<'a, A>(parser: impl Parser<'a, A>) -> impl Parser<'a, A> {
    move |input: &'a [Symbol]| {
        let (value, _) = parser.parse(input)?;
        Some((value, input))
    }
}

pub fn not<'a, A>(parser: impl Parser<'a, A>) -> impl Parser<'a, ()> {
    move |input: &'a [Symbol]| match parser.parse(input) {
        Some(_) => None,
        None => Some(((), input)),
    }
}

/// Sequences two parsers, one after the other, collecting both results.
pub fn sequence<'a, A, B>(
    fst: impl Parser<'a, A>,
    snd: impl Parser<'a, B>,
) -> impl Parser<'a, (A, B)> {
    // equivalent to: `fst.and(snd)`
    move |input: &'a [Symbol]| {
        let (a, input) = fst.parse(input)?;
        let (b, input) = snd.parse(input)?;
        Some(((a, b), input))
    }
}

/// Tries to apply the first parser, if it fails, it tries to apply the second parser.
pub fn alternative<'a, A>(fst: impl Parser<'a, A>, snd: impl Parser<'a, A>) -> impl Parser<'a, A> {
    move |input: &'a [Symbol]| fst.parse(input).or_else(|| snd.parse(input))
}

/// Same as `either`, but allows for different output types for the parsers.
pub fn either<'a, A, B>(
    fst: impl Parser<'a, A>,
    snd: impl Parser<'a, B>,
) -> impl Parser<'a, Either<A, B>> {
    move |input: &'a [Symbol]| {
        if let Some((a, input)) = fst.parse(input) {
            Some((Either::Left(a), input))
        } else if let Some((b, input)) = snd.parse(input) {
            Some((Either::Right(b), input))
        } else {
            None
        }
    }
}

/// Tries to apply a parser, or fallback to a constant value (making it an always-succeeding parser).
pub fn fallback<'a, A: Clone>(def: A, parser: impl Parser<'a, A>) -> impl Parser<'a, A> {
    move |input: &'a [Symbol]| parser.parse(input).or(Some((def.clone(), input)))
}

/// Tries every parser in a slice, from left to right, and returns the output of the first succeeding one.
pub fn any<'a: 'b, 'b, A>(parsers: &'b [impl Parser<'a, A>]) -> impl Parser<'a, A> + 'b {
    move |input: &'a [Symbol]| parsers.iter().find_map(|parser| parser.parse(input))
}

/// Applies every parser in a slice, from left to right, and returns the output from all of them.
/// If one parser fails, the whole sequence is considered failed.
pub fn all<'a: 'b, 'b, A>(parsers: &'b [impl Parser<'a, A>]) -> impl Parser<'a, Vec<A>> + 'b {
    move |input: &'a [Symbol]| {
        let output = Vec::<A>::with_capacity(parsers.len());
        parsers
            .iter()
            .try_fold((output, input), |(mut output, input), parser| {
                let (value, input) = parser.parse(input)?;
                output.push(value);
                Some((output, input))
            })
    }
}

/// Tries to apply a parser, but fails gracefully (with an `Option` output).
pub fn optional<'a, A>(parser: impl Parser<'a, A>) -> impl Parser<'a, Option<A>> {
    move |input: &'a [Symbol]| {
        if let Some((value, input)) = parser.parse(input) {
            Some((Some(value), input))
        } else {
            Some((None, input))
        }
    }
}

/// Applies a parser zero or more times.
pub fn many<'a, A>(parser: impl Parser<'a, A>) -> impl Parser<'a, Vec<A>> {
    move |mut input: &'a [Symbol]| {
        let mut output = Vec::<A>::new();
        while let Some((value, next)) = parser.parse(input) {
            input = next;
            output.push(value);
        }
        Some((output, input))
    }
}

/// Applies a parser one or more times.
pub fn some<'a, A>(parser: impl Parser<'a, A>) -> impl Parser<'a, Vec<A>> {
    move |input: &'a [Symbol]| {
        let (value, mut input) = parser.parse(input)?;
        let mut output = vec![value];
        while let Some((value, next)) = parser.parse(input) {
            input = next;
            output.push(value);
        }
        Some((output, input))
    }
}

/// Parses something that is enclosed between two other things.
pub fn between<'a, A, B, C>(
    before: impl Parser<'a, A>,
    within: impl Parser<'a, B>,
    after: impl Parser<'a, C>,
) -> impl Parser<'a, B> {
    move |input: &'a [Symbol]| {
        let (_, input) = before.parse(input)?;
        let (value, input) = within.parse(input)?;
        let (_, input) = after.parse(input)?;
        Some((value, input))
    }
}

pub fn sep_by<'a, A, B>(
    delim: impl Parser<'a, A>,
    within: impl Parser<'a, B>,
) -> impl Parser<'a, Vec<B>> {
    move |input: &'a [Symbol]| {
        let mut output = Vec::<B>::new();
        if let Some((value, mut input)) = within.parse(input) {
            output.push(value);
            while let Some((value, next)) = delim
                .parse(input)
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

pub fn sep_by1<'a, A, B>(
    delim: impl Parser<'a, A>,
    within: impl Parser<'a, B>,
) -> impl Parser<'a, Vec<B>> {
    move |input: &'a [Symbol]| {
        let mut output = Vec::<B>::new();
        let (value, mut input) = within.parse(input)?;
        output.push(value);
        while let Some((value, next)) = delim
            .parse(input)
            .and_then(|(_, input)| within.parse(input))
        {
            input = next;
            output.push(value);
        }
        Some((output, input))
    }
}

/// Transforms the output value of a parser.
pub fn map<'a, A, B>(parser: impl Parser<'a, A>, func: impl Fn(A) -> B) -> impl Parser<'a, B> {
    move |input: &'a [Symbol]| {
        let (value, input) = parser.parse(input)?;
        Some((func(value), input))
    }
}
