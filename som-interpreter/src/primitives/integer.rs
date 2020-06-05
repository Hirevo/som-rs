use std::rc::Rc;

use crate::expect_args;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

fn from_string(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#fromString:";

    expect_args!(SIGNATURE, args, [
        _,
        Value::String(string) => string,
    ]);

    match string.parse() {
        Ok(parsed) => Return::Local(Value::Integer(parsed)),
        Err(err) => Return::Exception(err.to_string()),
    }
}

fn as_string(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#asString";

    expect_args!(SIGNATURE, args, [
        Value::Integer(value) => value,
    ]);

    Return::Local(Value::String(Rc::new(value.to_string())))
}

fn plus(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#+";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    Return::Local(Value::Integer(a + b))
}

fn minus(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#-";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    Return::Local(Value::Integer(a - b))
}

fn times(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#*";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    Return::Local(Value::Integer(a * b))
}

fn divide(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#/";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    Return::Local(Value::Integer(a.div_euclid(b)))
}

fn divide_float(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#//";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    Return::Local(Value::Double((a as f64) / (b as f64)))
}

fn modulo(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#%";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    Return::Local(Value::Integer(a.rem_euclid(b)))
}

fn bitand(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#&";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    Return::Local(Value::Integer(a & b))
}

fn lt(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#<";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    Return::Local(Value::Boolean(a < b))
}

fn eq(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#=";

    expect_args!(SIGNATURE, args, [
        // Value::Integer(a) => a,
        // Value::Integer(b) => b,
        a => a,
        b => b,
    ]);

    Return::Local(Value::Boolean(a == b))
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
