use std::rc::Rc;

use num_traits::ToPrimitive;

use crate::expect_args;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("+", self::plus, true),
    ("-", self::minus, true),
    ("*", self::times, true),
    ("//", self::divide, true),
    ("%", self::modulo, true),
    ("=", self::eq, true),
    ("<", self::lt, true),
    ("sqrt", self::sqrt, true),
    ("round", self::round, true),
    ("cos", self::cos, true),
    ("sin", self::sin, true),
    ("asString", self::as_string, true),
    ("asInteger", self::as_integer, true),
];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("fromString:", self::from_string, true),
    ("PositiveInfinity", self::positive_infinity, true),
];

macro_rules! promote {
    ($signature:expr, $value:expr) => {
        match $value {
            Value::Integer(value) => value as f64,
            Value::BigInteger(value) => match value.to_f64() {
                Some(value) => value,
                None => {
                    return Return::Exception(format!(
                        "'{}': `Integer` too big to be converted to `Double`",
                        $signature
                    ))
                }
            },
            Value::Double(value) => value,
            _ => {
                return Return::Exception(format!(
                    "'{}': wrong type (expected `integer` or `double`)",
                    $signature
                ))
            }
        }
    };
}

fn from_string(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#fromString:";

    expect_args!(SIGNATURE, args, [
        _,
        Value::String(string) => string,
    ]);

    match string.parse() {
        Ok(parsed) => Return::Local(Value::Double(parsed)),
        Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    }
}

fn as_string(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#asString";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    let value = promote!(SIGNATURE, value);

    Return::Local(Value::String(Rc::new(value.to_string())))
}

fn as_integer(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#asInteger";

    expect_args!(SIGNATURE, args, [
        Value::Double(value) => value,
    ]);

    Return::Local(Value::Integer(value.trunc() as i64))
}

fn sqrt(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#sqrt";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    let value = promote!(SIGNATURE, value);

    Return::Local(Value::Double(value.sqrt()))
}

fn round(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#round";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    let value = promote!(SIGNATURE, value);

    Return::Local(Value::Double(value.round()))
}

fn cos(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#cos";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    let value = promote!(SIGNATURE, value);

    Return::Local(Value::Double(value.cos()))
}

fn sin(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#sin";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    let value = promote!(SIGNATURE, value);

    Return::Local(Value::Double(value.sin()))
}

fn eq(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#=";

    expect_args!(SIGNATURE, args, [
        // Value::Double(a) => a,
        // Value::Double(b) => b,
        a => a,
        b => b,
    ]);

    Return::Local(Value::Boolean(a == b))
}

fn lt(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#<";

    expect_args!(SIGNATURE, args, [
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Return::Local(Value::Boolean(a < b))
}

fn plus(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#+";

    expect_args!(SIGNATURE, args, [
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Return::Local(Value::Double(a + b))
}

fn minus(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#-";

    expect_args!(SIGNATURE, args, [
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Return::Local(Value::Double(a - b))
}

fn times(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#*";

    expect_args!(SIGNATURE, args, [
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Return::Local(Value::Double(a * b))
}

fn divide(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#//";

    expect_args!(SIGNATURE, args, [
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Return::Local(Value::Double(a / b))
}

fn modulo(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Double>>#%";

    expect_args!(SIGNATURE, args, [
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Return::Local(Value::Double(a % b))
}

fn positive_infinity(_: &mut Universe, _: Vec<Value>) -> Return {
    const _: &str = "Double>>#positiveInfinity";

    Return::Local(Value::Double(f64::INFINITY))
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
