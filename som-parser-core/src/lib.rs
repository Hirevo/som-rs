use std::marker::PhantomData;

/// Generic parser combinators.
pub mod combinators;

/// Defines a parser.
///
/// It is basically a function that takes an input and returns a parsed result along with the rest of input (which can be parsed further).
pub trait Parser<T, I>: Sized {
    /// Applies the parser on some input.
    ///
    /// It returns the parsed value and the rest of the unparsed input as `Some`, if successful.  
    /// Failing that, it returns `None`.  
    fn parse(&mut self, input: I) -> Option<(T, I)>;

    /// Sequences two parsers, one after the other, collecting both results.
    fn and<U, P: Parser<U, I>>(self, parser: P) -> And<Self, P> {
        And {
            p1: self,
            p2: parser,
        }
    }

    /// Tries to apply the first parser, if it fails, it tries to apply the second parser.
    fn or<P: Parser<T, I>>(self, parser: P) -> Or<Self, P> {
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
    fn and_left<P: Parser<U, I>, U>(self, parser: P) -> AndLeft<Self, P, U> {
        AndLeft {
            p1: self,
            p2: parser,
            _phantom: PhantomData,
        }
    }

    /// Sequences two parsers, one after the other, but discards the output of the first one.
    fn and_right<P: Parser<U, I>, U>(self, parser: P) -> AndRight<Self, P, T> {
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

impl<T1, T2, A, B, I> Parser<(T1, T2), I> for And<A, B>
where
    A: Parser<T1, I>,
    B: Parser<T2, I>,
{
    fn parse<'a>(&mut self, input: I) -> Option<((T1, T2), I)> {
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

impl<T, A, B, I> Parser<T, I> for Or<A, B>
where
    I: Clone,
    A: Parser<T, I>,
    B: Parser<T, I>,
{
    fn parse(&mut self, input: I) -> Option<(T, I)> {
        self.p1
            .parse(input.clone())
            .or_else(|| self.p2.parse(input))
    }
}

/// Maps a function over the output value of the parser.
pub struct Map<P, F, T> {
    parser: P,
    func: F,
    _phantom: PhantomData<T>,
}

impl<P, T, F, U, I> Parser<U, I> for Map<P, F, T>
where
    P: Parser<T, I>,
    F: Fn(T) -> U,
{
    fn parse<'a>(&mut self, input: I) -> Option<(U, I)> {
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

impl<A, B, T, U, I> Parser<T, I> for AndLeft<A, B, U>
where
    A: Parser<T, I>,
    B: Parser<U, I>,
{
    fn parse(&mut self, input: I) -> Option<(T, I)> {
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

impl<A, B, T, U, I> Parser<U, I> for AndRight<A, B, T>
where
    A: Parser<T, I>,
    B: Parser<U, I>,
{
    fn parse(&mut self, input: I) -> Option<(U, I)> {
        let (_, input) = self.p1.parse(input)?;
        let (value, input) = self.p2.parse(input)?;
        Some((value, input))
    }
}

/// Because a `Parser` is basically a function of the following signature.
/// ```text
/// (I) -> (T, I)
/// ```
/// We can implement it for any bare `Fn(I) -> (T, I)`.
impl<T, F, I> Parser<T, I> for F
where
    F: FnMut(I) -> Option<(T, I)>,
{
    fn parse(&mut self, input: I) -> Option<(T, I)> {
        (self)(input)
    }
}
