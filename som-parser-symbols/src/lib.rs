//!
//! This crate serves as the syntactical analyser (parser) for the Simple Object Machine.
//!
//! This particular version of the parser works with the tokens outputted by the lexical analyser, instead of directly reading text.
//!

/// Generic parser combinators.
pub mod combinators;
/// SOM-specific parser combinators.
pub mod lang;
/// Parser combinator primitives.
pub mod parser;

pub use crate::parser::Parser;

use som_core::ast::ClassDef;
use som_lexer::Token;

/// Parses the input of an entire file into an AST.
pub fn parse_file(source: &str, input: &[Token]) -> Option<ClassDef> {
    let (class, _) = lang::file(source).parse(input)?;
    Some(class)
}
