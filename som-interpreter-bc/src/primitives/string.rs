use std::collections::hash_map::DefaultHasher;
use std::convert::TryFrom;
use std::hash::Hasher;
use std::rc::Rc;

use crate::{expect_args, reverse};
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

fn length(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#length";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': invalid self type", SIGNATURE),
    };

    match i64::try_from(value.chars().count()) {
        Ok(idx) => frame.borrow_mut().stack.push(Value::Integer(idx)),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn hashcode(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#hashcode";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': invalid self type", SIGNATURE),
    };

    let mut hasher = DefaultHasher::new();

    hasher.write(value.as_bytes());

    // match i64::try_from(hasher.finish()) {
    //     Ok(hash) => frame.borrow_mut().stack.push(Value::Integer(hash)),
    //     Err(err) => panic!("'{}': {}", SIGNATURE, err),
    // }

    frame.borrow_mut().stack.push(Value::Integer((hasher.finish() as i64).abs()))
}

fn is_letters(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#isLetters";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': invalid self type", SIGNATURE),
    };

    frame.borrow_mut().stack.push(Value::Boolean(
        !value.is_empty() && !value.is_empty() && value.chars().all(char::is_alphabetic),
    ))
}

fn is_digits(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#isDigits";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': invalid self type", SIGNATURE),
    };

    frame.borrow_mut().stack.push(Value::Boolean(
        !value.is_empty() && value.chars().all(char::is_numeric),
    ))
}

fn is_whitespace(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#isWhiteSpace";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': invalid self type", SIGNATURE),
    };

    frame.borrow_mut().stack.push(Value::Boolean(
        !value.is_empty() && value.chars().all(char::is_whitespace),
    ))
}

fn concatenate(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#concatenate:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        s1 => s1,
        s2 => s2,
    ]);

    let s1 = match s1 {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': wrong types", SIGNATURE),
    };
    let s2 = match s2 {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    frame.borrow_mut().stack.push(Value::String(Rc::new(format!("{}{}", s1, s2))))
}

fn as_symbol(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#asSymbol";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        value => value,
    ]);

    match value {
        Value::String(ref value) => {
            frame.borrow_mut().stack.push(Value::Symbol(universe.intern_symbol(value.as_str())))
        }
        Value::Symbol(sym) => frame.borrow_mut().stack.push(Value::Symbol(sym)),
        _ => panic!("'{}': invalid self type", SIGNATURE),
    }
}

fn eq(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "String>>#=";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        s1 => s1,
        s2 => s2,
    ]);

    frame.borrow_mut().stack.push(Value::Boolean(s1 == s2))
}

fn prim_substring_from_to(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#primSubstringFrom:to:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        value => value,
        Value::Integer(from) => from,
        Value::Integer(to) => to,
    ]);

    let (value, from, to) = match (&value, usize::try_from(from - 1), usize::try_from(to)) {
        (Value::String(ref value), Ok(from), Ok(to)) => (value.as_str(), from, to),
        (Value::Symbol(sym), Ok(from), Ok(to)) => (universe.lookup_symbol(*sym), from, to),
        (_, _, _) => panic!("'{}': wrong types", SIGNATURE),
    };

    let string = Rc::new(value.chars().skip(from).take(to - from).collect());

    frame.borrow_mut().stack.push(Value::String(string))
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "length" => Some(self::length),
        "hashcode" => Some(self::hashcode),
        "isLetters" => Some(self::is_letters),
        "isDigits" => Some(self::is_digits),
        "isWhiteSpace" => Some(self::is_whitespace),
        "asSymbol" => Some(self::as_symbol),
        "concatenate:" => Some(self::concatenate),
        "primSubstringFrom:to:" => Some(self::prim_substring_from_to),
        "=" => Some(self::eq),
        _ => None,
    }
}
