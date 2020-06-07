//!
//! This is the interpreter for the Simple Object Machine.
//!
// #![allow(dead_code, unused_variables, unused_imports)]

use std::path::PathBuf;
use std::rc::Rc;

use anyhow::anyhow;
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

    match opts.file {
        None => {
            let mut universe = Universe::with_classpath(opts.classpath)?;
            shell::interactive(&mut universe, opts.verbose)?
        },
        Some(file) => {
            let file_stem = file
                .file_stem()
                .ok_or_else(|| anyhow!("the given path has no file stem"))?;
            let file_stem = file_stem
                .to_str()
                .ok_or_else(|| anyhow!("the given path contains invalid UTF-8 in its file stem"))?;

            let mut classpath = opts.classpath;
            if let Some(directory) = file.parent() {
                classpath.push(directory.to_path_buf());
            }

            let mut universe = Universe::with_classpath(classpath)?;

            let args = std::iter::once(String::from(file_stem))
                .chain(opts.args.iter().cloned())
                .map(Rc::new)
                .map(Value::String)
                .collect();

            let output = universe.initialize(args).unwrap_or_else(|| {
                Return::Exception(format!("could not find 'System>>#initialize:'"))
            });

            // let class = universe.load_class_from_path(file)?;
            // let instance = som_interpreter::instance::Instance::from_class(class);
            // let instance = Value::Instance(Rc::new(std::cell::RefCell::new(instance)));

            // let invokable = instance.lookup_method(&universe, "run").unwrap();
            // let output = som_interpreter::invokable::Invoke::invoke(invokable.as_ref(), &mut universe, vec![instance]);

            match output {
                Return::Exception(message) => println!("ERROR: {}", message),
                Return::Restart => println!("ERROR: asked for a restart to the top-level"),
                _ => {}
            }
        }
    }

    Ok(())
}
