use std::convert::TryFrom;
use std::fs;
use std::rc::Rc;

use crate::expect_args;
use crate::frame::FrameKind;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("loadFile:", self::load_file, true),
    ("printString:", self::print_string, true),
    ("printNewline", self::print_newline, true),
    ("errorPrint:", self::error_print, true),
    ("errorPrintln:", self::error_println, true),
    ("load:", self::load, true),
    ("ticks", self::ticks, true),
    ("time", self::time, true),
    ("fullGC", self::full_gc, true),
    ("exit:", self::exit, true),
    ("global:", self::global, true),
    ("global:put:", self::global_put, true),
    ("hasGlobal:", self::has_global, true),
    ("printStackTrace", self::print_stack_trace, true),
];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn load_file(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#loadFile:";

    expect_args!(SIGNATURE, args, [
        Value::System,
        value => value,
    ]);

    let path = match value {
        Value::String(ref string) => string,
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => return Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    };

    match fs::read_to_string(path) {
        Ok(value) => Return::Local(Value::String(Rc::new(value))),
        Err(_) => Return::Local(Value::Nil),
    }
}

fn print_string(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#printString:";

    expect_args!(SIGNATURE, args, [
        Value::System,
        value => value,
    ]);

    let string = match value {
        Value::String(ref string) => string,
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => return Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    };

    print!("{}", string);
    Return::Local(Value::System)
}

fn print_newline(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "System>>#printNewline";

    expect_args!(SIGNATURE, args, [Value::System]);

    println!();
    Return::Local(Value::Nil)
}

fn error_print(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#errorPrint:";

    expect_args!(SIGNATURE, args, [
        Value::System,
        value => value,
    ]);

    let string = match value {
        Value::String(ref string) => string,
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => return Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    };

    eprint!("{}", string);
    Return::Local(Value::System)
}

fn error_println(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#errorPrintln:";

    expect_args!(SIGNATURE, args, [
        Value::System,
        value => value,
    ]);

    let string = match value {
        Value::String(ref string) => string,
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => return Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    };

    eprintln!("{}", string);
    Return::Local(Value::System)
}

fn load(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#load:";

    expect_args!(SIGNATURE, args, [
        Value::System,
        Value::Symbol(sym) => sym,
    ]);

    let name = universe.lookup_symbol(sym).to_string();
    match universe.load_class(name) {
        Ok(class) => Return::Local(Value::Class(class)),
        Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    }
}

fn has_global(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#hasGlobal:";

    expect_args!(SIGNATURE, args, [
        Value::System,
        Value::Symbol(sym) => sym,
    ]);

    let symbol = universe.lookup_symbol(sym);
    Return::Local(Value::Boolean(universe.has_global(symbol)))
}

fn global(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#global:";

    expect_args!(SIGNATURE, args, [
        Value::System,
        Value::Symbol(sym) => sym,
    ]);

    let symbol = universe.lookup_symbol(sym);
    Return::Local(universe.lookup_global(symbol).unwrap_or(Value::Nil))
}

fn global_put(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#global:put:";

    expect_args!(SIGNATURE, args, [
        Value::System,
        Value::Symbol(sym) => sym,
        value => value,
    ]);

    let symbol = universe.lookup_symbol(sym).to_string();
    universe.assign_global(symbol, value.clone());
    Return::Local(value)
}

fn exit(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#exit:";

    expect_args!(SIGNATURE, args, [
        Value::System,
        Value::Integer(code) => code,
    ]);

    match i32::try_from(code) {
        Ok(code) => std::process::exit(code),
        Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    }
}

fn ticks(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#ticks";

    expect_args!(SIGNATURE, args, [Value::System]);

    match i64::try_from(universe.start_time.elapsed().as_micros()) {
        Ok(micros) => Return::Local(Value::Integer(micros)),
        Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    }
}

fn time(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#time";

    expect_args!(SIGNATURE, args, [Value::System]);

    match i64::try_from(universe.start_time.elapsed().as_millis()) {
        Ok(micros) => Return::Local(Value::Integer(micros)),
        Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    }
}

fn print_stack_trace(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#printStackTrace";

    expect_args!(SIGNATURE, args, [Value::System]);

    for frame in &universe.frames {
        let class = frame.borrow().get_method_holder();
        let signature = frame.borrow().get_method_signature();
        let signature = universe.lookup_symbol(signature);
        let block = match frame.borrow().kind() {
            FrameKind::Block { .. } => "$block",
            _ => "",
        };
        println!("{}>>#{}{}", class.borrow().name(), signature, block);
    }

    Return::Local(Value::Boolean(true))
}

fn full_gc(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#fullGC";

    expect_args!(SIGNATURE, args, [Value::System]);

    // We don't do any garbage collection at all, so we return false.
    Return::Local(Value::Boolean(false))
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
