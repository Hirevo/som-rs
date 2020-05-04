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
