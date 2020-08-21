//!
//! This crate serves as the syntactical analyser (parser) for the Simple Object Machine.
//!
//! This particular version of the parser works with by directly reading the source code text, thus it does not need any lexical analysis stage before it.
//!

/// SOM-specific parser combinators.
pub mod lang;

use som_core::ast::ClassDef;
use som_parser_core::Parser;

/// Parses the input of an entire file into an AST.
pub fn parse_file(input: &[char]) -> Option<ClassDef> {
    self::apply(lang::file(), input)
}

/// Applies a parser and returns the output value if the entirety of the input has been parsed successfully.
pub fn apply<'a, A, P>(mut parser: P, input: &'a [char]) -> Option<A>
where
    P: Parser<A, &'a [char]>,
{
    match parser.parse(input) {
        Some((output, tail)) if tail.is_empty() => Some(output),
        Some(_) | None => None,
    }
}
