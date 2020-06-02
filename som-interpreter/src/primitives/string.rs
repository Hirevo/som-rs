use std::convert::TryFrom;
use std::rc::Rc;

use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

fn at(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "String>>#at:";

    let index = match args[1] {
        Value::Integer(value) => match usize::try_from(value) {
            Ok(idx) => idx,
            Err(err) => return Return::Exception(format!("'{}': {}", SIGNATURE, err)),
        },
        _ => return Return::Exception(format!("'{}': invalid index type", SIGNATURE)),
    };
    match args[0] {
        Value::String(ref value) => Return::Local(
            value
                .chars()
                .nth(index as usize)
                .map(|ch| Value::String(Rc::new(ch.to_string())))
                .unwrap_or(Value::Nil),
        ),
        _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    }
}

fn length(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "String>>#length";

    match args[0] {
        Value::String(ref value) => match i64::try_from(value.len()) {
            Ok(idx) => Return::Local(Value::Integer(idx)),
            Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
        },
        _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    }
}

fn concatenate(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "String>>#concatenate:";

    match (&args[0], &args[1]) {
        (Value::String(s1), Value::String(s2)) => {
            Return::Local(Value::String(Rc::new(format!("{}{}", s1, s2))))
        }
        _ => Return::Exception(format!("'{}': wrong types", SIGNATURE)),
    }
}

fn as_symbol(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "String>>#asSymbol";

    match args[0] {
        Value::String(ref value) => {
            Return::Local(Value::Symbol(universe.intern_symbol(value.as_str())))
        }
        _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    }
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "at:" => Some(self::at),
        "length" => Some(self::length),
        "asSymbol" => Some(self::as_symbol),
        "concatenate:" => Some(self::concatenate),
        _ => None,
    }
}
