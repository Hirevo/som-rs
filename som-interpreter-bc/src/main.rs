//!
//! This is the interpreter for the Simple Object Machine.
//!
#![warn(missing_docs)]

use std::path::PathBuf;

use anyhow::{bail, Context};
use clap::Parser;
#[cfg(feature = "jemalloc")]
use jemallocator::Jemalloc;

mod shell;

use som_gc::{GcHeap, GcParams};

use som_interpreter_bc::disassembler::disassemble_method_body;
use som_interpreter_bc::interpreter::Interpreter;
use som_interpreter_bc::method::MethodKind;
use som_interpreter_bc::universe::Universe;
use som_interpreter_bc::value::Value;

#[cfg(feature = "jemalloc")]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

#[derive(Debug, Clone, PartialEq, clap::StructOpt)]
struct GcOptions {
    /// Minimum amount of bytes allocated to trigger garbage collection.
    #[structopt(long, default_value = "10000000")]
    gc_initial_threshold: usize,

    /// How much heap should grow after unsuccessful garbage collection.
    #[structopt(long, default_value = "0.7")]
    gc_used_space_ratio: f64,

    /// Whether to collect on exit, to avoid leaking memory.
    #[structopt(long)]
    gc_collect_on_exit: bool,

    /// Print GC stats before exiting.
    #[structopt(long)]
    gc_print_stats: bool,

    /// Force garbage collection before printing stats.
    /// Useful for checking for memory leaks.
    /// Does nothing unless --gc-print-stats is specified.
    #[structopt(long)]
    gc_collect_before_stats: bool,
}

#[derive(Debug, Clone, PartialEq, clap::StructOpt)]
#[clap(about, author)]
struct Options {
    /// File to evaluate.
    file: Option<PathBuf>,

    /// Arguments to pass to the `#run:` function.
    args: Vec<String>,

    /// Set search path for application classes.
    #[clap(long, short, multiple_values(true))]
    classpath: Vec<PathBuf>,

    /// Disassemble the class, instead of executing.
    #[clap(long, short)]
    disassemble: bool,

    /// Enable verbose output (with timing information).
    #[clap(short = 'v')]
    verbose: bool,

    #[structopt(flatten)]
    gc: GcOptions,
}

fn main() -> anyhow::Result<()> {
    let opts: Options = Options::parse();

    let mut params = GcParams::default();
    params.threshold = opts.gc.gc_initial_threshold;
    params.used_space_ratio = opts.gc.gc_used_space_ratio;

    let mut heap = GcHeap::with_params(params);
    let mut interpreter = Interpreter::new();

    if opts.disassemble {
        return disassemble_class(&mut heap, opts);
    }

    let Some(file) = opts.file else {
        let mut universe = Universe::with_classpath(&mut heap, opts.classpath)?;
        let result = shell::interactive(&mut heap, &mut interpreter, &mut universe, opts.verbose);

        if opts.gc.gc_print_stats {
            if opts.gc.gc_collect_before_stats {
                heap.collect_garbage(|| {});
            }

            let stats = heap.stats();
            let params = heap.params();

            println!();
            println!("total GC runs: {}", stats.collections_performed);
            println!("total bytes swept: {}", stats.bytes_swept);
            println!("total bytes still allocated: {}", stats.bytes_allocated);
            println!("total GC time: {:?}", stats.total_time_spent);
            println!("final GC threshold: {}", params.threshold);
            println!();
        }

        if opts.gc.gc_collect_on_exit {
            heap.collect_garbage(|| {});
        }

        return result;
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

    let mut universe = Universe::with_classpath(&mut heap, classpath)?;

    let args = std::iter::once(String::from(file_stem))
        .chain(opts.args.iter().cloned())
        .map(|it| Value::new_string(&heap.allocate(it)))
        .collect();

    universe
        .initialize(&mut heap, &mut interpreter, args)
        .expect("issue running program");

    interpreter.run(&mut heap, &mut universe)?;

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

    if opts.gc.gc_print_stats {
        if opts.gc.gc_collect_before_stats {
            heap.collect_garbage(|| {});
        }

        let stats = heap.stats();
        let params = heap.params();

        println!();
        println!("total GC runs: {}", stats.collections_performed);
        println!("total bytes swept: {}", stats.bytes_swept);
        println!("total bytes still allocated: {}", stats.bytes_allocated);
        println!("total GC time: {:?}", stats.total_time_spent);
        println!("final GC threshold: {}", params.threshold);
        println!();
    }

    if opts.gc.gc_collect_on_exit {
        heap.collect_garbage(|| {});
    }

    Ok(())
}

fn disassemble_class(heap: &mut GcHeap, opts: Options) -> anyhow::Result<()> {
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
    let mut universe = Universe::with_classpath(heap, classpath)?;

    let class = universe.load_class(heap, file_stem)?;

    let methods: Vec<_> = if opts.args.is_empty() {
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
