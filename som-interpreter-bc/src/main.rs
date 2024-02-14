//!
//! This is the interpreter for the Simple Object Machine.
//!
#![warn(missing_docs)]

use std::path::PathBuf;
use std::rc::Rc;

use anyhow::{bail, Context};
use structopt::StructOpt;

#[cfg(feature = "jemalloc")]
use jemallocator::Jemalloc;

mod shell;

use som_interpreter_bc::disassembler::disassemble_method_body;
use som_interpreter_bc::interpreter::Interpreter;
use som_interpreter_bc::method::{Method, MethodKind};
use som_interpreter_bc::universe::Universe;
use som_interpreter_bc::value::Value;

#[cfg(feature = "profiler")]
use som_interpreter_bc::profiler::Profiler;

#[cfg(feature = "jemalloc")]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

#[derive(Debug, Clone, PartialEq, StructOpt)]
#[structopt(about, author)]
struct Options {
    /// File to evaluate.
    file: Option<PathBuf>,

    /// Arguments to pass to the `#run:` function.
    args: Vec<String>,

    /// Set search path for application classes.
    #[structopt(long, short)]
    classpath: Vec<PathBuf>,

    /// Disassemble the class, instead of executing.
    #[structopt(long, short)]
    disassemble: bool,

    /// Enable verbose output (with timing information).
    #[structopt(short = "v")]
    verbose: bool,
}

fn main() -> anyhow::Result<()> {
    let result = run();
    #[cfg(feature = "profiler")]
    Profiler::global().drop();
    result
}

fn run() -> anyhow::Result<()> {
    let opts: Options = Options::from_args();

    let mut interpreter = Interpreter::new();

    if opts.disassemble {
        return disassemble_class(opts);
    }

    let Some(file) = opts.file else {
        let mut universe = Universe::with_classpath(opts.classpath)?;
        return shell::interactive(&mut interpreter, &mut universe, opts.verbose);
    };

    let file_stem = file
        .file_stem()
        .context("the given path has no file stem")?
        .to_str()
        .context("the given path contains invalid UTF-8 in its file stem")?;

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

    Ok(())
}

fn disassemble_class(opts: Options) -> anyhow::Result<()> {
    let Some(file) = opts.file else {
        bail!("no class specified for disassembly");
    };

    let file_stem = file
        .file_stem()
        .context("the given path has no file stem")?
        .to_str()
        .context("the given path contains invalid UTF-8 in its file stem")?;

    let mut classpath = opts.classpath;
    if let Some(directory) = file.parent() {
        classpath.push(directory.to_path_buf());
    }
    let mut universe = Universe::with_classpath(classpath)?;

    let class = universe.load_class(file_stem)?;

    let methods: Vec<Rc<Method>> = if opts.args.is_empty() {
        class.borrow().methods.values().cloned().collect()
    } else {
        opts.args
            .iter()
            .filter_map(|signature| {
                let symbol = universe.intern_symbol(signature);
                let maybe_method = class.borrow().methods.get(&symbol).cloned();

                if maybe_method.is_none() {
                    eprintln!("No method named `{signature}` found in class `{file_stem}`.");
                }

                maybe_method
            })
            .collect()
    };

    for method in methods {
        match &method.kind {
            MethodKind::Defined(env) => {
                println!(
                    "{class}>>#{signature} ({num_locals} locals, {num_literals} literals)",
                    class = file_stem,
                    signature = method.signature(),
                    num_locals = env.locals.len(),
                    num_literals = env.literals.len(),
                );

                disassemble_method_body(&universe, &class.borrow(), env);
            }
            MethodKind::Primitive(_) => {
                println!(
                    "{class}>>#{signature} (primitive)",
                    class = file_stem,
                    signature = method.signature(),
                );
            }
            MethodKind::NotImplemented(_) => {
                println!(
                    "{class}>>#{signature} (not implemented)",
                    class = file_stem,
                    signature = method.signature(),
                );
            }
        }
    }

    Ok(())
}
