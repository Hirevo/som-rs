//!
//! This is the interpreter for the Simple Object Machine.
//!
// #![allow(dead_code, unused_variables, unused_imports)]

use std::path::PathBuf;
use std::rc::Rc;

use structopt::StructOpt;

mod shell;

use som_interpreter::invokable::Return;
use som_interpreter::universe::Universe;
use som_interpreter::value::Value;

#[derive(Debug, Clone, PartialEq, StructOpt)]
#[structopt(about, author)]
struct Options {
    /// Files to evaluate.
    #[structopt(name = "FILE")]
    file: Option<PathBuf>,

    #[structopt(name = "ARGS")]
    args: Vec<String>,

    /// Set search path for application classes.
    #[structopt(short, long)]
    classpath: Vec<PathBuf>,

    // /// enable disassembling
    // #[structopt(short = "d")]
    // disassembling: bool,
    /// Enable verbose output (with timing information).
    #[structopt(short = "v")]
    verbose: bool,
}

fn main() -> anyhow::Result<()> {
    let opts: Options = Options::from_args();

    let mut universe = Universe::with_classpath(opts.classpath)?;

    match opts.file {
        None => shell::interactive(&mut universe, opts.verbose)?,
        Some(file) => {
            let args = std::iter::once(
                file.file_stem()
                    .and_then(|stem| stem.to_str())
                    .map(String::from)
                    .expect("no file stem ?"),
            )
            .chain(opts.args.iter().cloned())
            .map(Rc::new)
            .map(Value::String)
            .collect();

            let output = universe.initialize(args).unwrap_or_else(|| {
                Return::Exception(format!("could not find 'System>>#initialize:'"))
            });

            match output {
                Return::Exception(message) => println!("ERROR: {}", message),
                Return::Restart => println!("ERROR: asked for a restart to the top-level"),
                _ => {}
            }
        }
    }

    Ok(())
}
