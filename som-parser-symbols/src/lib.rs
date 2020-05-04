//!
//! The SOM Parser (symbols)
//! ========================
//!
//! This crate serves as the syntactical analyser (parser) for the Simple Object Machine.
//!
//! This particular version of the parser works with the tokens outputted by the lexical analyser, instead of directly reading text.
//!

pub mod combinators;
pub mod lang;
pub mod parser;

pub use crate::parser::Parser;

use som_core::ast::ClassDef;
use som_lexer::Symbol;

/// Parses the input of an entire file into an AST.
pub fn parse_file(input: &[Symbol]) -> Option<ClassDef> {
    lang::file().parse(input).map(lang::fst)
}
