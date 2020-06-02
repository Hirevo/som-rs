use std::marker::PhantomData;

/// Defines a parser.
///
/// It is basically a function that takes an input and returns a parsed result along with the rest of input (which can be parsed further).
pub trait Parser<'a, T>: Sized {
    /// Applies the parser on some input.
    ///
    /// It returns the parsed value and the rest of the unparsed input as `Some`, if successful.  
    /// Failing that, it returns `None`.  
    fn parse(&self, input: &'a [char]) -> Option<(T, &'a [char])>;

    /// Sequences two parsers, one after the other, collecting both results.
    fn and<U, P: Parser<'a, U>>(self, parser: P) -> And<Self, P> {
        And {
            p1: self,
            p2: parser,
        }
    }

    /// Tries to apply the first parser, if it fails, it tries to apply the second parser.
    fn or<P: Parser<'a, T>>(self, parser: P) -> Or<Self, P> {
        Or {
            p1: self,
            p2: parser,
        }
    }

    /// Maps a function over the output value of the parser.
    fn map<F: Fn(T) -> U, U>(self, func: F) -> Map<Self, F, T> {
        Map {
            parser: self,
            func,
            _phantom: PhantomData,
        }
    }

    /// Sequences two parsers, one after the other, but discards the output of the second one.
    fn and_left<P: Parser<'a, U>, U>(self, parser: P) -> AndLeft<Self, P, U> {
        AndLeft {
            p1: self,
            p2: parser,
            _phantom: PhantomData,
        }
    }

    /// Sequences two parsers, one after the other, but discards the output of the first one.
    fn and_right<P: Parser<'a, U>, U>(self, parser: P) -> AndRight<Self, P, T> {
        AndRight {
            p1: self,
            p2: parser,
            _phantom: PhantomData,
        }
    }
}

/// Sequences two parsers, one after the other, collecting both results.
pub struct And<A, B> {
    p1: A,
    p2: B,
}

impl<'a, T1, T2, A, B> Parser<'a, (T1, T2)> for And<A, B>
where
    A: Parser<'a, T1>,
    B: Parser<'a, T2>,
{
    fn parse(&self, input: &'a [char]) -> Option<((T1, T2), &'a [char])> {
        let (v1, input) = self.p1.parse(input)?;
        let (v2, input) = self.p2.parse(input)?;
        Some(((v1, v2), input))
    }
}

/// Tries to apply the first parser, if it fails, it tries to apply the second parser.
pub struct Or<A, B> {
    p1: A,
    p2: B,
}

impl<'a, T, A, B> Parser<'a, T> for Or<A, B>
where
    A: Parser<'a, T>,
    B: Parser<'a, T>,
{
    fn parse(&self, input: &'a [char]) -> Option<(T, &'a [char])> {
        self.p1.parse(input).or_else(|| self.p2.parse(input))
    }
}

/// Maps a function over the output value of the parser.
pub struct Map<P, F, T> {
    parser: P,
    func: F,
    _phantom: PhantomData<T>,
}

impl<'a, P, T, F, U> Parser<'a, U> for Map<P, F, T>
where
    P: Parser<'a, T>,
    F: Fn(T) -> U,
{
    fn parse(&self, input: &'a [char]) -> Option<(U, &'a [char])> {
        let (value, input) = self.parser.parse(input)?;
        Some(((self.func)(value), input))
    }
}

/// Sequences two parsers, one after the other, but discards the output of the second one.
pub struct AndLeft<A, B, U> {
    p1: A,
    p2: B,
    _phantom: PhantomData<U>,
}

impl<'a, A, B, T, U> Parser<'a, T> for AndLeft<A, B, U>
where
    A: Parser<'a, T>,
    B: Parser<'a, U>,
{
    fn parse(&self, input: &'a [char]) -> Option<(T, &'a [char])> {
        let (value, input) = self.p1.parse(input)?;
        let (_, input) = self.p2.parse(input)?;
        Some((value, input))
    }
}

/// Sequences two parsers, one after the other, but discards the output of the first one.
pub struct AndRight<A, B, T> {
    p1: A,
    p2: B,
    _phantom: PhantomData<T>,
}

impl<'a, A, B, T, U> Parser<'a, U> for AndRight<A, B, T>
where
    A: Parser<'a, T>,
    B: Parser<'a, U>,
{
    fn parse(&self, input: &'a [char]) -> Option<(U, &'a [char])> {
        let (_, input) = self.p1.parse(input)?;
        let (value, input) = self.p2.parse(input)?;
        Some((value, input))
    }
}

/// Because a `Parser` is basically a function of the following signature.
/// ```text
/// (&[char]) -> (T, &[char])
/// ```
/// We can implement it for any `Fn(&[char]) -> (T, &[char])`.
impl<'a, T, F> Parser<'a, T> for F
where
    F: Fn(&'a [char]) -> Option<(T, &'a [char])>,
{
    fn parse(&self, input: &'a [char]) -> Option<(T, &'a [char])> {
        (self)(input)
    }
}
