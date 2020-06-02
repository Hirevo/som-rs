//!
//! This crate serves as the lexical analyser for the Simple Object Machine.
//!

/// The lexer module.
mod lexer;
/// The token definitions.
mod token;

pub use crate::lexer::Lexer;
pub use crate::token::Token;
