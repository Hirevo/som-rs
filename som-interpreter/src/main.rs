//!
//! This is the interpreter for the Simple Object Machine.
//!
// #![allow(dead_code, unused_variables, unused_imports)]

use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use structopt::StructOpt;

mod shell;

use som_interpreter::invokable::Invoke;
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
    dbg!(std::mem::size_of::<som_core::ast::MethodDef>());
    dbg!(std::mem::size_of::<som_core::ast::ClassDef>());
    dbg!(std::mem::size_of::<som_interpreter::invokable::Invokable>());
    dbg!(std::mem::size_of::<som_interpreter::value::Value>());
    dbg!(std::mem::size_of::<som_interpreter::block::Block>());
    dbg!(std::mem::size_of::<som_interpreter::class::Class>());
    dbg!(std::mem::size_of::<som_interpreter::instance::Instance>());

    // return Ok(());

    let opts: Options = Options::from_args();

    let mut universe = Universe::from_classpath(opts.classpath)?;

    match opts.file {
        None => shell::interactive(&mut universe, opts.verbose)?,
        Some(file) => {
            let initialize = Value::System
                .lookup_method(&universe, "initialize:")
                .expect("could not find 'System>>#initialize:'");
            initialize.invoke(
                &mut universe,
                vec![
                    Value::System,
                    Value::Array(Rc::new(RefCell::new(
                        std::iter::once(
                            file.file_stem()
                                .and_then(|stem| stem.to_str())
                                .map(String::from)
                                .expect("no file stem ?"),
                        )
                        .chain(opts.args.iter().cloned())
                        .map(Rc::new)
                        .map(Value::String)
                        .collect(),
                    ))),
                ],
            );
        }
    }

    Ok(())
}
