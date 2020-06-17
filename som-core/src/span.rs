/// Represents a region of source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub from: usize,
    pub to: usize,
}

impl Span {
    /// Construct a span given its lower and upper bounds.
    pub fn new(from: usize, to: usize) -> Self {
        Self { from, to }
    }

    /// Construct the span going from the beginning of the first span to the end of the second span.
    pub fn between(s1: Self, s2: Self) -> Self {
        Self {
            from: s1.from,
            to: s2.to,
        }
    }

    /// Get the string slice corresponding to this span.
    pub fn to_str(self, source: &str) -> &str {
        &source[self.from..self.to]
    }
}
