use std::rc::Rc;

use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

fn from_string(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#fromString:";

    match args[1] {
        Value::String(ref string) => match string.parse() {
            Ok(parsed) => Return::Local(Value::Double(parsed)),
            Err(err) => Return::Exception(err.to_string()),
        },
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn as_string(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#asString";

    match args[0] {
        Value::Double(double) => Return::Local(Value::String(Rc::new(double.to_string()))),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn as_integer(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#asInteger";

    match args[0] {
        Value::Double(double) => Return::Local(Value::Integer(double.trunc() as i64)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn sqrt(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#sqrt";

    match args[0] {
        Value::Double(double) => Return::Local(Value::Double(double.sqrt())),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn round(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#round";

    match args[0] {
        Value::Double(double) => Return::Local(Value::Double(double.round())),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn cos(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#cos";

    match args[0] {
        Value::Double(double) => Return::Local(Value::Double(double.cos())),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn sin(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#sin";

    match args[0] {
        Value::Double(double) => Return::Local(Value::Double(double.sin())),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn eq(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#=";

    match (&args[0], &args[1]) {
        (Value::Double(a), Value::Double(b)) => Return::Local(Value::Boolean(a == b)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn lt(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#<";

    match (&args[0], &args[1]) {
        (Value::Double(a), Value::Double(b)) => Return::Local(Value::Boolean(a < b)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn plus(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#+";

    match (&args[0], &args[1]) {
        (Value::Double(a), Value::Double(b)) => Return::Local(Value::Double(a + b)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn minus(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#-";

    match (&args[0], &args[1]) {
        (Value::Double(a), Value::Double(b)) => Return::Local(Value::Double(a - b)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn times(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#*";

    match (&args[0], &args[1]) {
        (Value::Double(a), Value::Double(b)) => Return::Local(Value::Double(a * b)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn divide(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#//";

    match (&args[0], &args[1]) {
        (Value::Double(a), Value::Double(b)) => Return::Local(Value::Double(a / b)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn modulo(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#%";

    match (&args[0], &args[1]) {
        (Value::Double(a), Value::Double(b)) => Return::Local(Value::Double(a % b)),
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "+" => Some(self::plus),
        "-" => Some(self::minus),
        "*" => Some(self::times),
        "//" => Some(self::divide),
        "%" => Some(self::modulo),
        "=" => Some(self::eq),
        "<" => Some(self::lt),
        "sqrt" => Some(self::sqrt),
        "round" => Some(self::round),
        "cos" => Some(self::cos),
        "sin" => Some(self::sin),
        "fromString:" => Some(self::from_string),
        "asString" => Some(self::as_string),
        "asInteger" => Some(self::as_integer),
        _ => None,
    }
}
