use std::rc::Rc;

use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

fn from_string(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#fromString:";

    match args[1] {
        Value::String(ref string) => match string.parse() {
            Ok(parsed) => Return::Local(Value::Integer(parsed)),
            Err(err) => Return::Exception(err.to_string()),
        },
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn as_string(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#asString";

    match args[0] {
        Value::Integer(integer) => Return::Local(Value::String(Rc::new(integer.to_string()))),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn plus(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#+";

    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Return::Local(Value::Integer(a + b)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn minus(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#-";

    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Return::Local(Value::Integer(a - b)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn times(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#*";

    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Return::Local(Value::Integer(a * b)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn divide(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#/";

    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Return::Local(Value::Integer(a.div_euclid(*b))),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn divide_float(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#//";

    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => {
            Return::Local(Value::Double((*a as f64) / (*b as f64)))
        }
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn modulo(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#%";

    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Return::Local(Value::Integer(a.rem_euclid(*b))),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn bitand(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#&";

    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Return::Local(Value::Integer(a & b)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn lt(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#<";

    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Return::Local(Value::Boolean(a < b)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn eq(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#=";

    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Return::Local(Value::Boolean(a == b)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "fromString:" => Some(self::from_string),
        "asString" => Some(self::as_string),
        "<" => Some(self::lt),
        "=" => Some(self::eq),
        "+" => Some(self::plus),
        "-" => Some(self::minus),
        "*" => Some(self::times),
        "/" => Some(self::divide),
        "//" => Some(self::divide_float),
        "%" => Some(self::modulo),
        "&" => Some(self::bitand),
        _ => None,
    }
}
