//!
//! This crate serves as the syntactical analyser (parser) for the Simple Object Machine.
//!
//! This particular version of the parser works with by directly reading the source code text, thus it does not need any lexical analysis stage before it.
//!

/// The collection of generic parser combinators.
pub mod combinators;
/// The collection of language-specific parsers.
pub mod lang;
/// The base `Parser` definitions.
pub mod parser;

pub use crate::parser::Parser;

use som_core::ast::ClassDef;

/// Parses the input of an entire file into an AST.
pub fn parse_file(input: &[char]) -> Option<ClassDef> {
    let (class, _) = lang::file().parse(input)?;
    Some(class)
}
