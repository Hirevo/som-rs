//!
//! The SOM Interpreter
//! ===================
//!
//! This is the interpreter for the Simple Object Machine.
//!

use std::fs;
use std::io;
use std::path::PathBuf;
use std::time::Instant;

use structopt::StructOpt;

use som_lexer::{Lexer, Symbol};

#[derive(Debug, Clone, PartialEq, StructOpt)]
#[structopt(about, author)]
struct Options {
    /// Files to evaluate.
    #[structopt(name = "FILES")]
    files: Vec<PathBuf>,

    /// Set search path for application classes.
    #[structopt(short = "cp")]
    class_path: Vec<PathBuf>,

    // /// enable disassembling
    // #[structopt(short = "d")]
    // disassembling: bool,

    /// Enable verbose output (with timing information).
    #[structopt(short = "v")]
    verbose: bool,
}

fn main() -> io::Result<()> {
    let opts: Options = Options::from_args();

    let path = match opts.files.into_iter().next() {
        Some(file) => file,
        None => {
            eprintln!("error: missing file.");
            std::process::exit(1);
        }
    };

    let contents = fs::read_to_string(path.as_path())?;
    let lexer = Lexer::new(contents.as_str())
        .skip_comments(true)
        .skip_whitespace(true);

    let start = Instant::now();
    let symbols: Vec<Symbol> = lexer.collect();
    let elapsed = start.elapsed();
    if opts.verbose {
        println!("Lexing time: {} ms ({} µs)", elapsed.as_millis(), elapsed.as_micros());
    }

    let start = Instant::now();
    let results = som_parser::parse_file(symbols.as_slice());
    let elapsed = start.elapsed();
    if opts.verbose {
        println!("Parsing time: {} ms ({} µs)", elapsed.as_millis(), elapsed.as_micros());
    }

    println!();
    match results {
        Some(ast) => {
            println!("success: the file has been successfully parsed.");
            println!();
            println!("{:?}", ast);
        }
        None => {
            eprintln!("error: failed to parse input file.");
            std::process::exit(1);
        }
    }

    Ok(())
}
