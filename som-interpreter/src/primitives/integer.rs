use std::rc::Rc;

use rand::distributions::Uniform;
use rand::Rng;

use crate::expect_args;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

macro_rules! promoted_expr {
    ($signature:expr, $a:expr, $b:expr, $expr:expr) => {
        match ($a, $b) {
            (Value::Integer(a), Value::Integer(b)) => Return::Local(Value::Integer({$expr}(a, b))),
            (Value::Integer(a), Value::Double(b)) => Return::Local(Value::Double({$expr}((a as f64), b))),
            (Value::Double(a), Value::Integer(b)) => Return::Local(Value::Double({$expr}(a, (b as f64)))),
            (Value::Double(a), Value::Double(b)) => Return::Local(Value::Double({$expr}(a, b))),
            _ => Return::Exception(format!("'{}': wrong type (expected `integer` or `double`)", $signature)),
        }
    };

    ($signature:expr, $a:expr, $b:expr, $op:tt) => {
        match (a, b) {
            (Value::Integer(a), Value::Integer(b)) => Return::Local(Value::Integer(expr!(a $op b))),
            (Value::Integer(a), Value::Double(b)) => Return::Local(Value::Double(expr!((a as f64) $op b))),
            (Value::Double(a), Value::Integer(b)) => Return::Local(Value::Double(expr!(a $op (b as f64)))),
            (Value::Double(a), Value::Double(b)) => Return::Local(Value::Double(expr!(a $op b))),
            _ => Return::Exception(format!("'{}': wrong type (expected `integer` or `double`)", $signature)),
        }
    };
}

macro_rules! promote {
    ($signature:expr, $value:expr) => {
        match $value {
            Value::Integer(value) => value as f64,
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

fn from_string(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#fromString:";

    expect_args!(SIGNATURE, args, [
        _,
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => return Return::Exception(format!("'{}': wrong types", SIGNATURE)),
    };

    match value.parse() {
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

fn at_random(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#atRandom";

    expect_args!(SIGNATURE, args, [
        Value::Integer(value) => value,
    ]);

    let distribution = Uniform::new(0, value);
    let mut rng = rand::thread_rng();
    let chosen = rng.sample(distribution);

    Return::Local(Value::Integer(chosen))
}

fn as_32bit_signed_value(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#as32BitSignedValue";

    expect_args!(SIGNATURE, args, [
        Value::Integer(value) => value,
    ]);

    Return::Local(Value::Integer(value as i32 as i64))
}

fn as_32bit_unsigned_value(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#as32BitUnsignedValue";

    expect_args!(SIGNATURE, args, [
        Value::Integer(value) => value,
    ]);

    Return::Local(Value::Integer(value as u32 as i64))
}

fn plus(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#+";

    expect_args!(SIGNATURE, args, [
        a => a,
        b => b,
    ]);

    promoted_expr!(SIGNATURE, a, b, |a, b| a + b)
}

fn minus(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#-";

    expect_args!(SIGNATURE, args, [
        a => a,
        b => b,
    ]);

    promoted_expr!(SIGNATURE, a, b, |a, b| a - b)
}

fn times(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#*";

    expect_args!(SIGNATURE, args, [
        a => a,
        b => b,
    ]);

    promoted_expr!(SIGNATURE, a, b, |a, b| a * b)
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
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Return::Local(Value::Double(a / b))
}

fn modulo(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#%";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    let result = a % b;
    if result.signum() != b.signum() {
        Return::Local(Value::Integer(result + b))
    } else {
        Return::Local(Value::Integer(result))
    }
}

fn remainder(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#rem:";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    let result = a % b;
    if result.signum() != a.signum() {
        Return::Local(Value::Integer(result + a))
    } else {
        Return::Local(Value::Integer(result))
    }
}

fn sqrt(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#sqrt";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
    ]);

    let sqrt = (a as f64).sqrt();
    let trucated = sqrt.trunc();

    if sqrt == trucated {
        Return::Local(Value::Integer(trucated as i64))
    } else {
        Return::Local(Value::Double(sqrt))
    }
}

fn bitand(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#&";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    Return::Local(Value::Integer(a & b))
}

fn bitxor(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#bitXor:";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    Return::Local(Value::Integer(a ^ b))
}

fn lt(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#<";

    expect_args!(SIGNATURE, args, [
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Return::Local(Value::Boolean(a < b))
}

fn eq(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#=";

    expect_args!(SIGNATURE, args, [
        a => a,
        b => b,
    ]);

    Return::Local(Value::Boolean(a == b))
}

fn shift_left(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#<<";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    Return::Local(Value::Integer(a << b))
}

fn shift_right(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Integer>>#>>";

    expect_args!(SIGNATURE, args, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    Return::Local(Value::Integer(a >> b))
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "fromString:" => Some(self::from_string),
        "asString" => Some(self::as_string),
        "atRandom" => Some(self::at_random),
        "as32BitSignedValue" => Some(self::as_32bit_signed_value),
        "as32BitUnsignedValue" => Some(self::as_32bit_unsigned_value),
        "<" => Some(self::lt),
        "=" => Some(self::eq),
        "+" => Some(self::plus),
        "-" => Some(self::minus),
        "*" => Some(self::times),
        "/" => Some(self::divide),
        "//" => Some(self::divide_float),
        "%" => Some(self::modulo),
        "rem:" => Some(self::remainder),
        "&" => Some(self::bitand),
        "<<" => Some(self::shift_left),
        ">>>" => Some(self::shift_right),
        "bitXor:" => Some(self::bitxor),
        "sqrt" => Some(self::sqrt),
        _ => None,
    }
}
