//!
//! The SOM Parser (text)
//! =====================
//!
//! This crate serves as the syntactical analyser (parser) for the Simple Object Machine.
//!
//! This particular version of the parser works with by directly reading the source code text, thus it does not need any lexical analysis stage before it.
//!

pub mod combinators;
pub mod lang;
pub mod parser;

pub use crate::parser::Parser;

use som_core::ast::ClassDef;

/// Parses the input of an entire file into an AST.
pub fn parse_file(input: &[char]) -> Option<ClassDef> {
    lang::file().parse(input).map(lang::fst)
}
