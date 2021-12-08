use std::convert::TryFrom;

use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, reverse};

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("printString:", self::print_string, true),
    ("printNewline", self::print_newline, true),
    ("load:", self::load, true),
    ("ticks", self::ticks, true),
    ("time", self::time, true),
    ("fullGC", self::full_gc, true),
    ("exit:", self::exit, true),
    ("global:", self::global, true),
    ("global:put:", self::global_put, true),
];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn print_string(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#printString:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        value => value,
    ]);

    let string = match value {
        Value::String(ref string) => string,
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': wrong type", SIGNATURE),
    };

    print!("{}", string);
    use std::io::Write;
    std::io::stdout().flush().unwrap();
    interpreter.stack.push(Value::System)
}

fn print_newline(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &'static str = "System>>#printNewline";

    expect_args!(SIGNATURE, interpreter, [Value::System]);

    println!();
    interpreter.stack.push(Value::Nil)
}

fn load(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#load:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        Value::Symbol(sym) => sym,
    ]);

    let name = universe.lookup_symbol(sym).to_string();
    match universe.load_class(name) {
        Ok(class) => interpreter.stack.push(Value::Class(class)),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn global(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#global:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        Value::Symbol(sym) => sym,
    ]);

    interpreter
        .stack
        .push(universe.lookup_global(sym).unwrap_or(Value::Nil))
}

fn global_put(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#global:put:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        Value::Symbol(sym) => sym,
        value => value,
    ]);

    universe.assign_global(sym, value.clone());
    interpreter.stack.push(value)
}

fn exit(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "System>>#exit:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        Value::Integer(code) => code,
    ]);

    match i32::try_from(code) {
        Ok(code) => std::process::exit(code),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn ticks(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "System>>#ticks";

    expect_args!(SIGNATURE, interpreter, [Value::System]);

    match i64::try_from(interpreter.start_time.elapsed().as_micros()) {
        Ok(micros) => interpreter.stack.push(Value::Integer(micros)),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn time(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "System>>#time";

    expect_args!(SIGNATURE, interpreter, [Value::System]);

    match i64::try_from(interpreter.start_time.elapsed().as_millis()) {
        Ok(micros) => interpreter.stack.push(Value::Integer(micros)),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn full_gc(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "System>>#fullGC";

    expect_args!(SIGNATURE, interpreter, [Value::System]);

    // We don't do any garbage collection at all, so we return false.
    interpreter.stack.push(Value::Boolean(false))
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<PrimitiveFn> {
    INSTANCE_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<PrimitiveFn> {
    CLASS_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}
