//!
//! This is the interpreter for the Simple Object Machine.
//!
#![warn(missing_docs)]

use std::path::PathBuf;

use anyhow::anyhow;
use gc::Gc;
use structopt::StructOpt;

mod shell;

use som_interpreter_bc::interpreter::Interpreter;
use som_interpreter_bc::universe::Universe;
use som_interpreter_bc::value::Value;

#[derive(Debug, Clone, PartialEq, StructOpt)]
struct GcOptions {
    /// Min bytes allocated to start garbage collection.
    #[structopt(long, default_value = "20000000")]
    gc_initial_threshold: usize,

    /// How much heap should grow after unsuccessful garbage collection.
    #[structopt(long)]
    gc_used_space_ratio: Option<f64>,

    /// Whether to collect on exit, to avoid leaking memory.
    #[structopt(long)]
    gc_collect_on_exit: bool,

    /// Print GC stats before exiting.
    #[structopt(long)]
    gc_print_stats: bool,

    /// Force garbage collection before printing stats.
    /// Useful for checking for memory leaks.
    /// Does nothing useless --gc-print-stats is specified.
    #[structopt(long)]
    gc_collect_before_stats: bool,
}

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

    #[structopt(flatten)]
    gc: GcOptions,
}

fn main() -> anyhow::Result<()> {
    let opts: Options = Options::from_args();

    gc::configure(|config| {
        config.threshold = opts.gc.gc_initial_threshold;
        config.leak_on_drop = !opts.gc.gc_collect_on_exit;
        if let Some(used_space_ratio) = opts.gc.gc_used_space_ratio {
            config.used_space_ratio = used_space_ratio;
        }
    });

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

            let args = std::iter::once(String::from(file_stem))
                .chain(opts.args.iter().cloned())
                .map(Gc::new)
                .map(Value::String)
                .collect();

            universe
                .initialize(&mut interpreter, args)
                .expect("issue running program");

            interpreter.run(&mut universe);

            if opts.gc.gc_collect_before_stats {
                gc::force_collect();
            }

            if opts.gc.gc_print_stats {
                eprintln!("=== GC STATS ===");
                gc::configure(|c| {
                    eprintln!("Final threshold: {}", c.threshold);
                });
                let stats = gc::stats();
                eprintln!("Collections performed: {}", stats.collections_performed);
                eprintln!("Bytes still allocated: {}", stats.bytes_allocated);
            }

        }
    }

    Ok(())
}
