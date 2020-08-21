//!
//! This is the interpreter for the Simple Object Machine.
//!
#![warn(missing_docs)]

use std::path::PathBuf;

use anyhow::anyhow;
use structopt::StructOpt;

mod shell;

use som_interpreter_bc::interpreter::Interpreter;
use som_interpreter_bc::universe::Universe;
use som_interpreter_bc::value::Value;

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

    let mut interpreter = Interpreter::new();

    match opts.file {
        None => {
            let mut universe = Universe::with_classpath(opts.classpath)?;
            shell::interactive(&mut interpreter, &mut universe, opts.verbose)?
        }
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

            // let class = universe.load_class("System");
            // if let Ok(class) = class {
            //     for method in class.borrow().methods.values() {
            //         println!("System>>#{}", method.signature);
            //         if let som_interpreter_bc::method::MethodKind::Defined(env) = &method.kind {
            //             for bytecode in &env.body {
            //                 println!("    {}", bytecode);
            //             }
            //         }
            //     }
            // }

            let args = std::iter::once(String::from(file_stem))
                .chain(opts.args.iter().cloned())
                .map(Into::into)
                .map(Value::String)
                .collect();

            universe
                .initialize(&mut interpreter, args)
                .expect("issue running program");

            interpreter.run(&mut universe);

            // let class = universe.load_class_from_path(file)?;
            // let instance = som_interpreter::instance::Instance::from_class(class);
            // let instance = Value::Instance(Rc::new(std::cell::RefCell::new(instance)));

            // let invokable = instance.lookup_method(&universe, "run").unwrap();
            // let output = som_interpreter::invokable::Invoke::invoke(invokable.as_ref(), &mut universe, vec![instance]);

            // match output {
            //     Return::Exception(message) => println!("ERROR: {}", message),
            //     Return::Restart => println!("ERROR: asked for a restart to the top-level"),
            //     _ => {}
            // }
        }
    }

    Ok(())
}
