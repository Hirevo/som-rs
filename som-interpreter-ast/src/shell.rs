use std::io;
use std::io::{BufRead, Write};
use std::time::Instant;

use anyhow::Error;

use som_lexer::{Lexer, Token};
use som_parser::lang;

use som_interpreter_ast::evaluate::Evaluate;
use som_interpreter_ast::frame::FrameKind;
use som_interpreter_ast::invokable::Return;
use som_interpreter_ast::universe::Universe;
use som_interpreter_ast::value::Value;

/// Launches an interactive Read-Eval-Print-Loop within the given universe.
pub fn interactive(universe: &mut Universe, verbose: bool) -> Result<(), Error> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

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

        let start = Instant::now();
        let tokens: Vec<Token> = Lexer::new(line)
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
        let expr = match som_parser::apply(lang::expression(), tokens.as_slice()) {
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

        let start = Instant::now();
        let kind = FrameKind::Method {
            holder: universe.system_class(),
            self_value: Value::System,
        };
        let output = universe.with_frame(kind, |universe| {
            universe
                .current_frame()
                .borrow_mut()
                .bindings
                .insert("it".into(), last_value.clone());

            expr.evaluate(universe)
        });
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
                println!("returned: {} ({:?})", value.to_string(&universe), value);
                last_value = value;
            }
            Return::NonLocal(value, frame) => {
                println!(
                    "returned (non-local, escaped): {} ({:?})",
                    value.to_string(&universe),
                    value
                );
                println!("intended for frame: {:?}", frame);
                last_value = value;
            }
            Return::Exception(message) => println!("ERROR: {}", message),
            Return::Restart => println!("ERROR: asked for a restart to the top-level"),
        }
        counter += 1;
    }

    Ok(())
}
