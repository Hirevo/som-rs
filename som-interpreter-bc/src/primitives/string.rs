use std::collections::hash_map::DefaultHasher;
use std::convert::{TryFrom, TryInto};
use std::hash::Hasher;

use anyhow::Error;
use num_bigint::BigInt;
use once_cell::sync::Lazy;

use som_gc::GcHeap;

use crate::interpreter::Interpreter;
use crate::primitives::{Primitive, PrimitiveFn, StringLike};
use crate::universe::Universe;
use crate::value::SOMValue;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> = Lazy::new(|| {
    Box::new([
        ("length", self::length.into_func(), true),
        ("hashcode", self::hashcode.into_func(), true),
        ("isLetters", self::is_letters.into_func(), true),
        ("isDigits", self::is_digits.into_func(), true),
        ("isWhiteSpace", self::is_whitespace.into_func(), true),
        ("asSymbol", self::as_symbol.into_func(), true),
        ("concatenate:", self::concatenate.into_func(), true),
        (
            "primSubstringFrom:to:",
            self::prim_substring_from_to.into_func(),
            true,
        ),
        ("=", self::eq.into_func(), true),
    ])
});
pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
    Lazy::new(|| Box::new([]));

fn length(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    receiver: StringLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "String>>#length";

    let string = match receiver {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    let length = string.chars().count();
    let length = match length.try_into() {
        Ok(value) => SOMValue::new_integer(value),
        Err(_) => {
            let allocated = heap.allocate(BigInt::from(length));
            SOMValue::new_big_integer(&allocated)
        }
    };

    interpreter.stack.push(length);

    Ok(())
}

fn hashcode(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    receiver: StringLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "String>>#hashcode";

    let string = match receiver {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    let mut hasher = DefaultHasher::new();
    hasher.write(string.as_bytes());
    let hash = (hasher.finish() as i32).abs();

    interpreter.stack.push(SOMValue::new_integer(hash));

    Ok(())
}

fn is_letters(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    receiver: StringLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "String>>#isLetters";

    let string = match receiver {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    interpreter.stack.push(SOMValue::new_boolean(
        !string.is_empty() && !string.is_empty() && string.chars().all(char::is_alphabetic),
    ));

    Ok(())
}

fn is_digits(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    receiver: StringLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "String>>#isDigits";

    let string = match receiver {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    interpreter.stack.push(SOMValue::new_boolean(
        !string.is_empty() && string.chars().all(char::is_numeric),
    ));

    Ok(())
}

fn is_whitespace(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    receiver: StringLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "String>>#isWhiteSpace";

    let string = match receiver {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    interpreter.stack.push(SOMValue::new_boolean(
        !string.is_empty() && string.chars().all(char::is_whitespace),
    ));

    Ok(())
}

fn concatenate(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    receiver: StringLike,
    other: StringLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "String>>#concatenate:";

    let s1 = match receiver {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    let s2 = match other {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    let allocated = heap.allocate(format!("{s1}{s2}"));
    interpreter.stack.push(SOMValue::new_string(&allocated));

    Ok(())
}

fn as_symbol(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    receiver: StringLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "String>>#asSymbol";

    let symbol = match receiver {
        StringLike::String(ref value) => universe.intern_symbol(value.as_str()),
        StringLike::Symbol(symbol) => symbol,
    };

    interpreter.stack.push(SOMValue::new_symbol(symbol));

    Ok(())
}

fn eq(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    a: SOMValue,
    b: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &str = "String>>#=";

    let Ok(a) = StringLike::try_from(a) else {
        interpreter.stack.push(SOMValue::FALSE);
        return Ok(());
    };

    let Ok(b) = StringLike::try_from(b) else {
        interpreter.stack.push(SOMValue::FALSE);
        return Ok(());
    };

    let a = match a {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    let b = match b {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    interpreter.stack.push(SOMValue::new_boolean(a == b));

    Ok(())
}

fn prim_substring_from_to(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    receiver: StringLike,
    from: i32,
    to: i32,
) -> Result<(), Error> {
    const SIGNATURE: &str = "String>>#primSubstringFrom:to:";

    let from = usize::try_from(from - 1)?;
    let to = usize::try_from(to)?;

    let string = match receiver {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    let string = heap.allocate(string.chars().skip(from).take(to - from).collect());

    interpreter.stack.push(SOMValue::new_string(&string));

    Ok(())
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}
