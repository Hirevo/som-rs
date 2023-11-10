use std::collections::hash_map::DefaultHasher;
use std::convert::TryFrom;
use std::hash::Hasher;

use som_gc::GcHeap;

use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::{SOMValue, Value};
use crate::{expect_args, reverse};

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("length", self::length, true),
    ("hashcode", self::hashcode, true),
    ("isLetters", self::is_letters, true),
    ("isDigits", self::is_digits, true),
    ("isWhiteSpace", self::is_whitespace, true),
    ("asSymbol", self::as_symbol, true),
    ("concatenate:", self::concatenate, true),
    ("primSubstringFrom:to:", self::prim_substring_from_to, true),
    ("=", self::eq, true),
];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn length(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#length";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': invalid self type", SIGNATURE),
    };

    match i32::try_from(value.chars().count()) {
        Ok(idx) => interpreter.stack.push(SOMValue::new_integer(idx)),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn hashcode(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#hashcode";

    expect_args!(SIGNATURE, interpreter, [
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
    //     Ok(hash) => interpreter.stack.push(SOMValue::new_integer(hash)),
    //     Err(err) => panic!("'{}': {}", SIGNATURE, err),
    // }

    interpreter
        .stack
        .push(SOMValue::new_integer((hasher.finish() as i32).abs()))
}

fn is_letters(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#isLetters";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': invalid self type", SIGNATURE),
    };

    interpreter.stack.push(SOMValue::new_boolean(
        !value.is_empty() && !value.is_empty() && value.chars().all(char::is_alphabetic),
    ))
}

fn is_digits(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#isDigits";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': invalid self type", SIGNATURE),
    };

    interpreter.stack.push(SOMValue::new_boolean(
        !value.is_empty() && value.chars().all(char::is_numeric),
    ))
}

fn is_whitespace(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#isWhiteSpace";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': invalid self type", SIGNATURE),
    };

    interpreter.stack.push(SOMValue::new_boolean(
        !value.is_empty() && value.chars().all(char::is_whitespace),
    ))
}

fn concatenate(interpreter: &mut Interpreter, heap: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#concatenate:";

    expect_args!(SIGNATURE, interpreter, [
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

    interpreter.stack.push(SOMValue::new_string(
        &heap.allocate(format!("{}{}", s1, s2)),
    ))
}

fn as_symbol(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#asSymbol";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    match value {
        Value::String(ref value) => interpreter
            .stack
            .push(SOMValue::new_symbol(universe.intern_symbol(value.as_str()))),
        Value::Symbol(sym) => interpreter.stack.push(SOMValue::new_symbol(sym)),
        _ => panic!("'{}': invalid self type", SIGNATURE),
    }
}

fn eq(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "String>>#=";

    expect_args!(SIGNATURE, interpreter, [
        s1 => s1,
        s2 => s2,
    ]);

    let s1 = match s1 {
        Value::String(ref s1) => s1.as_str(),
        Value::Symbol(s1) => universe.lookup_symbol(s1),
        _ => {
            interpreter.stack.push(SOMValue::new_boolean(false));
            return;
        }
    };

    let s2 = match s2 {
        Value::String(ref s2) => s2.as_str(),
        Value::Symbol(s2) => universe.lookup_symbol(s2),
        _ => {
            interpreter.stack.push(SOMValue::new_boolean(false));
            return;
        }
    };

    interpreter.stack.push(SOMValue::new_boolean(s1 == s2))
}

fn prim_substring_from_to(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
) {
    const SIGNATURE: &str = "String>>#primSubstringFrom:to:";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
        Value::Integer(from) => from,
        Value::Integer(to) => to,
    ]);

    let (value, from, to) = match (&value, usize::try_from(from - 1), usize::try_from(to)) {
        (Value::String(ref value), Ok(from), Ok(to)) => (value.as_str(), from, to),
        (Value::Symbol(sym), Ok(from), Ok(to)) => (universe.lookup_symbol(*sym), from, to),
        (_, _, _) => panic!("'{}': wrong types", SIGNATURE),
    };

    let string = heap.allocate(value.chars().skip(from).take(to - from).collect());

    interpreter.stack.push(SOMValue::new_string(&string))
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
