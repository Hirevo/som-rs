//!
//! The SOM Lexical Analyser
//! ========================
//!
//! This crate serves as the lexical analyser for the Simple Object Machine.
//!

mod lexer;
mod symbol;

pub use crate::lexer::Lexer;
pub use crate::symbol::Symbol;
