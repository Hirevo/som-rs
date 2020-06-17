use std::cell::RefCell;
use std::io;
use std::io::{BufRead, Write};
use std::rc::Rc;
use std::time::Instant;

use anyhow::Error;

use som_lexer::{Lexer, Token};
use som_parser::lang;
use som_parser::Parser;

use som_interpreter::frame::FrameKind;
use som_interpreter::instance::Instance;
use som_interpreter::invokable::{Invoke, Return};
use som_interpreter::universe::Universe;
use som_interpreter::value::Value;

/// Launches an interactive Read-Eval-Print-Loop within the given universe.
pub fn interactive(universe: &mut Universe, verbose: bool) -> Result<(), Error> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    let it_sym = universe.intern_symbol("it");
    let eval_sym = universe.intern_symbol("eval:");

    let mut counter = 0;
    let mut line = String::new();
    let mut last_value = Value::Nil;
    loop {
        write!(&mut stdout, "({}) SOM Shell | ", counter)?;
        stdout.flush()?;
        line.clear();
        stdin.read_line(&mut line)?;
        if line.is_empty() {
            writeln!(&mut stdout, "exit")?;
            break;
        }
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if line == "exit" {
            break;
        }

        let class = format!("ShellExpr{} = ( eval: it = ( ^ ( {} ) ) )", counter, line);

        let start = Instant::now();
        let tokens: Vec<Token> = Lexer::new(class.as_str())
            .skip_comments(true)
            .skip_whitespace(true)
            .collect();
        let elapsed = start.elapsed();
        if verbose {
            writeln!(
                &mut stdout,
                "Lexing time: {} ms ({} µs)",
                elapsed.as_millis(),
                elapsed.as_micros(),
            )?;
        }

        let start = Instant::now();
        let expr = match som_parser::parse_file(class.as_str(), tokens.as_slice()) {
            Some(expr) => expr,
            None => {
                println!("ERROR: could not fully parse the given expression");
                continue;
            }
        };
        let elapsed = start.elapsed();
        if verbose {
            writeln!(
                &mut stdout,
                "Parsing time: {} ms ({} µs)",
                elapsed.as_millis(),
                elapsed.as_micros(),
            )?;
        }

        let class = universe.load_class_from_memory(class)?;
        let instance = Instance::from_class(class.clone());

        let method = class.borrow().lookup_method(eval_sym);
        if let Some(method) = method {
            let start = Instant::now();
            let kind = FrameKind::Method {
                holder: universe.system_class(),
                self_value: Value::System,
            };

            let output = method.invoke(
                universe,
                vec![Value::Instance(Rc::new(RefCell::new(instance))), last_value.clone()],
            );
            let elapsed = start.elapsed();
            if verbose {
                writeln!(
                    &mut stdout,
                    "Execution time: {} ms ({} µs)",
                    elapsed.as_millis(),
                    elapsed.as_micros(),
                )?;
                writeln!(&mut stdout)?;
            }
            match output {
                Return::Local(value) => {
                    println!(
                        "returned: {} ({})",
                        value.as_display(universe),
                        value.as_debug(universe),
                    );
                    last_value = value;
                }
                Return::NonLocal(value, frame) => {
                    println!(
                        "returned (non-local, escaped): {} ({})",
                        value.as_display(universe),
                        value.as_debug(universe),
                    );
                    println!("intended for frame: {:?}", frame);
                    last_value = value;
                }
                Return::Exception(message) => println!("ERROR: {}", message),
                Return::Restart => println!("ERROR: asked for a restart to the top-level"),
            }
        } else {
            panic!("SOM shell's `eval` method not found ??");
        }

        counter += 1;
    }

    Ok(())
}
