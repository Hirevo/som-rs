use std::rc::Rc;

use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use rand::distributions::Uniform;
use rand::Rng;

use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, reverse};

macro_rules! demote {
    ($interpreter:expr, $expr:expr) => {{
        let value = $expr;
        match value.to_i64() {
            Some(value) => {
                $interpreter.stack.push(Value::Integer(value));
                return;
            }
            None => {
                $interpreter.stack.push(Value::BigInteger(value));
                return;
            }
        }
    }};
}

fn from_string(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#fromString:";

    expect_args!(SIGNATURE, interpreter, [
        _,
        value => value,
    ]);

    let value = match value {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    let parsed =
        (value.parse().map(Value::Integer)).or_else(|_| value.parse().map(Value::BigInteger));

    match parsed {
        Ok(parsed) => {
            interpreter.stack.push(parsed);
            return;
        }
        Err(err) => panic!("{}", err),
    }
}

fn as_string(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#asString";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    let value = match value {
        Value::Integer(value) => value.to_string(),
        Value::BigInteger(value) => value.to_string(),
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    {
        interpreter.stack.push(Value::String(Rc::new(value)));
        return;
    }
}

fn at_random(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#atRandom";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    let chosen = match value {
        Value::Integer(value) => {
            let distribution = Uniform::new(0, value);
            let mut rng = rand::thread_rng();
            rng.sample(distribution)
        }
        Value::BigInteger(_) => panic!(
            "'{}': the range is too big to pick a random value from",
            SIGNATURE,
        ),
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    {
        interpreter.stack.push(Value::Integer(chosen));
        return;
    }
}

fn as_32bit_signed_value(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#as32BitSignedValue";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    let value = match value {
        Value::Integer(value) => value as i32 as i64,
        Value::BigInteger(value) => match value.to_u32_digits() {
            (Sign::Minus, values) => -(values[0] as i64),
            (Sign::Plus, values) | (Sign::NoSign, values) => values[0] as i64,
        },
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    {
        interpreter.stack.push(Value::Integer(value));
        return;
    }
}

fn as_32bit_unsigned_value(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#as32BitUnsignedValue";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    let value = match value {
        Value::Integer(value) => value as u32 as i64,
        Value::BigInteger(value) => {
            let (_, values) = value.to_u32_digits();
            values[0] as i64
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    {
        interpreter.stack.push(Value::Integer(value));
        return;
    }
}

fn plus(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#+";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => match a.checked_add(b) {
            Some(value) => {
                interpreter.stack.push(Value::Integer(value));
                return;
            }
            None => demote!(interpreter, BigInt::from(a) + BigInt::from(b)),
        },
        (Value::BigInteger(a), Value::BigInteger(b)) => demote!(interpreter, a + b),
        (Value::BigInteger(a), Value::Integer(b)) | (Value::Integer(b), Value::BigInteger(a)) => {
            demote!(interpreter, a + BigInt::from(b))
        }
        (Value::Double(a), Value::Double(b)) => {
            interpreter.stack.push(Value::Double(a + b));
            return;
        }
        (Value::Integer(a), Value::Double(b)) | (Value::Double(b), Value::Integer(a)) => {
            interpreter.stack.push(Value::Double((a as f64) + b));
            return;
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    }
}

fn minus(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#-";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => match a.checked_sub(b) {
            Some(value) => {
                interpreter.stack.push(Value::Integer(value));
                return;
            }
            None => demote!(interpreter, BigInt::from(a) - BigInt::from(b)),
        },
        (Value::BigInteger(a), Value::BigInteger(b)) => demote!(interpreter, a - b),
        (Value::BigInteger(a), Value::Integer(b)) | (Value::Integer(b), Value::BigInteger(a)) => {
            demote!(interpreter, a - BigInt::from(b))
        }
        (Value::Double(a), Value::Double(b)) => {
            interpreter.stack.push(Value::Double(a - b));
            return;
        }
        (Value::Integer(a), Value::Double(b)) | (Value::Double(b), Value::Integer(a)) => {
            interpreter.stack.push(Value::Double((a as f64) - b));
            return;
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    }
}

fn times(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#*";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => match a.checked_mul(b) {
            Some(value) => {
                interpreter.stack.push(Value::Integer(value));
                return;
            }
            None => demote!(interpreter, BigInt::from(a) * BigInt::from(b)),
        },
        (Value::BigInteger(a), Value::BigInteger(b)) => demote!(interpreter, a * b),
        (Value::BigInteger(a), Value::Integer(b)) | (Value::Integer(b), Value::BigInteger(a)) => {
            demote!(interpreter, a * BigInt::from(b))
        }
        (Value::Double(a), Value::Double(b)) => {
            interpreter.stack.push(Value::Double(a * b));
            return;
        }
        (Value::Integer(a), Value::Double(b)) | (Value::Double(b), Value::Integer(a)) => {
            interpreter.stack.push(Value::Double((a as f64) * b));
            return;
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    }
}

fn divide(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#/";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => match a.checked_div(b) {
            Some(value) => {
                interpreter.stack.push(Value::Integer(value));
                return;
            }
            None => demote!(interpreter, BigInt::from(a) / BigInt::from(b)),
        },
        (Value::BigInteger(a), Value::BigInteger(b)) => demote!(interpreter, a / b),
        (Value::BigInteger(a), Value::Integer(b)) | (Value::Integer(b), Value::BigInteger(a)) => {
            demote!(interpreter, a / BigInt::from(b))
        }
        (Value::Double(a), Value::Double(b)) => {
            interpreter.stack.push(Value::Double(a / b));
            return;
        }
        (Value::Integer(a), Value::Double(b)) | (Value::Double(b), Value::Integer(a)) => {
            interpreter.stack.push(Value::Double((a as f64) / b));
            return;
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    }
}

fn divide_float(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#//";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => {
            interpreter
                .stack
                .push(Value::Double((a as f64) / (b as f64)));
            return;
        }
        (Value::Integer(a), Value::Double(b)) | (Value::Double(b), Value::Integer(a)) => {
            interpreter.stack.push(Value::Double((a as f64) / b));
            return;
        }
        (Value::Double(a), Value::Double(b)) => {
            interpreter.stack.push(Value::Double(a / b));
            return;
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    }
}

fn modulo(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#%";

    expect_args!(SIGNATURE, interpreter, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    let result = a % b;
    if result.signum() != b.signum() {
        {
            interpreter.stack.push(Value::Integer((result + b) % b));
            return;
        }
    } else {
        {
            interpreter.stack.push(Value::Integer(result));
            return;
        }
    }
}

fn remainder(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#rem:";

    expect_args!(SIGNATURE, interpreter, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    let result = a % b;
    if result.signum() != a.signum() {
        {
            interpreter.stack.push(Value::Integer((result + a) % a));
            return;
        }
    } else {
        {
            interpreter.stack.push(Value::Integer(result));
            return;
        }
    }
}

fn sqrt(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#sqrt";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
    ]);

    match a {
        Value::Integer(a) => {
            let sqrt = (a as f64).sqrt();
            let trucated = sqrt.trunc();
            if sqrt == trucated {
                {
                    interpreter.stack.push(Value::Integer(trucated as i64));
                    return;
                }
            } else {
                {
                    interpreter.stack.push(Value::Double(sqrt));
                    return;
                }
            }
        }
        Value::BigInteger(a) => demote!(interpreter, a.sqrt()),
        Value::Double(a) => {
            interpreter.stack.push(Value::Double(a.sqrt()));
            return;
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    }
}

fn bitand(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#&";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => {
            interpreter.stack.push(Value::Integer(a & b));
            return;
        }
        (Value::BigInteger(a), Value::BigInteger(b)) => demote!(interpreter, a & b),
        (Value::BigInteger(a), Value::Integer(b)) | (Value::Integer(b), Value::BigInteger(a)) => {
            demote!(interpreter, a & BigInt::from(b))
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    }
}

fn bitxor(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#bitXor:";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => {
            interpreter.stack.push(Value::Integer(a ^ b));
            return;
        }
        (Value::BigInteger(a), Value::BigInteger(b)) => demote!(interpreter, a ^ b),
        (Value::BigInteger(a), Value::Integer(b)) | (Value::Integer(b), Value::BigInteger(a)) => {
            demote!(interpreter, a ^ BigInt::from(b))
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    }
}

fn lt(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#<";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => {
            interpreter.stack.push(Value::Boolean(a < b));
            return;
        }
        (Value::BigInteger(a), Value::BigInteger(b)) => {
            interpreter.stack.push(Value::Boolean(a < b));
            return;
        }
        (Value::Double(a), Value::Double(b)) => {
            interpreter.stack.push(Value::Boolean(a < b));
            return;
        }
        (Value::Integer(a), Value::Double(b)) | (Value::Double(b), Value::Integer(a)) => {
            interpreter.stack.push(Value::Boolean((a as f64) < b));
            return;
        }
        (Value::BigInteger(a), Value::Integer(b)) => {
            interpreter.stack.push(Value::Boolean(a < BigInt::from(b)));
            return;
        }
        (Value::Integer(a), Value::BigInteger(b)) => {
            interpreter.stack.push(Value::Boolean(b < BigInt::from(a)));
            return;
        }
        (a, b) => panic!("'{}': wrong types ({:?} | {:?})", SIGNATURE, a, b),
    }
}

fn eq(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#=";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => {
            interpreter.stack.push(Value::Boolean(a == b));
            return;
        }
        (Value::BigInteger(a), Value::BigInteger(b)) => {
            interpreter.stack.push(Value::Boolean(a == b));
            return;
        }
        (Value::Double(a), Value::Double(b)) => {
            interpreter.stack.push(Value::Boolean(a == b));
            return;
        }
        (Value::Integer(a), Value::Double(b)) | (Value::Double(b), Value::Integer(a)) => {
            interpreter.stack.push(Value::Boolean((a as f64) == b));
            return;
        }
        _ => {
            interpreter.stack.push(Value::Boolean(false));
            return;
        }
    }
}

fn shift_left(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#<<";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        Value::Integer(b) => b,
    ]);

    match a {
        Value::Integer(a) => match a.checked_shl(b as u32) {
            Some(value) => {
                interpreter.stack.push(Value::Integer(value));
                return;
            }
            None => demote!(interpreter, BigInt::from(a) << (b as usize)),
        },
        Value::BigInteger(a) => demote!(interpreter, a << (b as usize)),
        _ => panic!("'{}': wrong types", SIGNATURE),
    }
}

fn shift_right(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#>>";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        Value::Integer(b) => b,
    ]);

    match a {
        Value::Integer(a) => match a.checked_shr(b as u32) {
            Some(value) => {
                interpreter.stack.push(Value::Integer(value));
                return;
            }
            None => demote!(interpreter, BigInt::from(a) >> (b as usize)),
        },
        Value::BigInteger(a) => demote!(interpreter, a >> (b as usize)),
        _ => panic!("'{}': wrong types", SIGNATURE),
    }
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
