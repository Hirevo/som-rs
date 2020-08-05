use std::convert::TryFrom;
// use std::io::BufRead;
// use std::rc::Rc;

use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, reverse};

// fn read_line(interpreter: &mut Interpreter, _: &mut Universe) {
//     const SIGNATURE: &str = "System>>#readLine";

// let frame = interpreter.current_frame().expect("no current frame");

//     expect_args!(SIGNATURE, frame, [Value::System]);

//     match std::io::stdin().lock().lines().next() {
//         Some(Ok(line)) => frame.borrow_mut().stack.push(Value::String(Rc::new(line))),
//         Some(Err(err)) => panic!("'{}': {}", SIGNATURE, err),
//         None => panic!("'{}': {}", SIGNATURE, "error"),
//     }
// }

fn print_string(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#printString:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        Value::System,
        value => value,
    ]);

    let string = match value {
        Value::String(ref string) => string,
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': wrong type", SIGNATURE),
    };

    print!("{}", string);
    frame.borrow_mut().stack.push(Value::System)
}

fn print_newline(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &'static str = "System>>#printNewline";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [Value::System]);

    println!();
    frame.borrow_mut().stack.push(Value::Nil)
}

fn load(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#load:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        Value::System,
        Value::Symbol(sym) => sym,
    ]);

    let name = universe.lookup_symbol(sym).to_string();
    match universe.load_class(name) {
        Ok(class) => frame.borrow_mut().stack.push(Value::Class(class)),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn global(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#global:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        Value::System,
        Value::Symbol(sym) => sym,
    ]);

    frame
        .borrow_mut()
        .stack
        .push(universe.lookup_global(sym).unwrap_or(Value::Nil))
}

fn global_put(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#global:put:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        Value::System,
        Value::Symbol(sym) => sym,
        value => value,
    ]);

    universe.assign_global(sym, value.clone());
    frame.borrow_mut().stack.push(value)
}

fn exit(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "System>>#exit:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
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

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [Value::System]);

    match i64::try_from(interpreter.start_time.elapsed().as_micros()) {
        Ok(micros) => frame.borrow_mut().stack.push(Value::Integer(micros)),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn time(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "System>>#time";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [Value::System]);

    match i64::try_from(interpreter.start_time.elapsed().as_millis()) {
        Ok(micros) => frame.borrow_mut().stack.push(Value::Integer(micros)),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn full_gc(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "System>>#fullGC";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [Value::System]);

    // We don't do any garbage collection at all, so we return false.
    frame.borrow_mut().stack.push(Value::Boolean(false))
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        // "readLine" => Some(self::read_line),
        "printString:" => Some(self::print_string),
        "printNewline" => Some(self::print_newline),
        "load:" => Some(self::load),
        "ticks" => Some(self::ticks),
        "time" => Some(self::time),
        "fullGC" => Some(self::full_gc),
        "exit:" => Some(self::exit),
        "global:" => Some(self::global),
        "global:put:" => Some(self::global_put),
        _ => None,
    }
}
