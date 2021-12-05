use std::collections::hash_map::DefaultHasher;
use std::convert::TryFrom;
use std::hash::Hasher;
use std::rc::Rc;

use crate::expect_args;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

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

fn length(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "String>>#length";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => return Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    };

    match i64::try_from(value.chars().count()) {
        Ok(idx) => Return::Local(Value::Integer(idx)),
        Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    }
}

fn hashcode(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "String>>#hashcode";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => return Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    };

    let mut hasher = DefaultHasher::new();

    hasher.write(value.as_bytes());

    // match i64::try_from(hasher.finish()) {
    //     Ok(hash) => Return::Local(Value::Integer(hash)),
    //     Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    // }

    Return::Local(Value::Integer((hasher.finish() as i64).abs()))
}

fn is_letters(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "String>>#isLetters";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => return Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    };

    Return::Local(Value::Boolean(
        !value.is_empty() && !value.is_empty() && value.chars().all(char::is_alphabetic),
    ))
}

fn is_digits(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "String>>#isDigits";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => return Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    };

    Return::Local(Value::Boolean(
        !value.is_empty() && value.chars().all(char::is_numeric),
    ))
}

fn is_whitespace(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "String>>#isWhiteSpace";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => return Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    };

    Return::Local(Value::Boolean(
        !value.is_empty() && value.chars().all(char::is_whitespace),
    ))
}

fn concatenate(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "String>>#concatenate:";

    expect_args!(SIGNATURE, args, [
        s1 => s1,
        s2 => s2,
    ]);

    let s1 = match s1 {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => return Return::Exception(format!("'{}': wrong types", SIGNATURE)),
    };
    let s2 = match s2 {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => return Return::Exception(format!("'{}': wrong types", SIGNATURE)),
    };

    Return::Local(Value::String(Rc::new(format!("{}{}", s1, s2))))
}

fn as_symbol(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "String>>#asSymbol";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    match value {
        Value::String(ref value) => {
            Return::Local(Value::Symbol(universe.intern_symbol(value.as_str())))
        }
        Value::Symbol(sym) => Return::Local(Value::Symbol(sym)),
        _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    }
}

fn eq(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "String>>#=";

    expect_args!(SIGNATURE, args, [
        s1 => s1,
        s2 => s2,
    ]);
    
    let s1 = match s1 {
        Value::String(ref s1) => s1.as_str(),
        Value::Symbol(s1) => universe.lookup_symbol(s1),
        _ => {
            return Return::Local(Value::Boolean(false));
        }
    };

    let s2 = match s2 {
        Value::String(ref s2) => s2.as_str(),
        Value::Symbol(s2) => universe.lookup_symbol(s2),
        _ => {
            return Return::Local(Value::Boolean(false));
        },
    };

    Return::Local(Value::Boolean(s1 == s2))
}

fn prim_substring_from_to(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "String>>#primSubstringFrom:to:";

    expect_args!(SIGNATURE, args, [
        value => value,
        Value::Integer(from) => from,
        Value::Integer(to) => to,
    ]);

    let (value, from, to) = match (&value, usize::try_from(from - 1), usize::try_from(to)) {
        (Value::String(ref value), Ok(from), Ok(to)) => (value.as_str(), from, to),
        (Value::Symbol(sym), Ok(from), Ok(to)) => (universe.lookup_symbol(*sym), from, to),
        (_, _, _) => return Return::Exception(format!("'{}': wrong types", SIGNATURE)),
    };

    let string = Rc::new(value.chars().skip(from).take(to - from).collect());

    Return::Local(Value::String(string))
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
