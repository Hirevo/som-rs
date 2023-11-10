use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use rand::distributions::Uniform;
use rand::Rng;

use som_gc::GcHeap;

use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::{SOMValue, Value};
use crate::{expect_args, reverse};

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("<", self::lt, true),
    ("=", self::eq, true),
    ("+", self::plus, true),
    ("-", self::minus, true),
    ("*", self::times, true),
    ("/", self::divide, true),
    ("//", self::divide_float, true),
    ("%", self::modulo, true),
    ("rem:", self::remainder, true),
    ("&", self::bitand, true),
    ("<<", self::shift_left, true),
    (">>>", self::shift_right, true),
    ("bitXor:", self::bitxor, true),
    ("sqrt", self::sqrt, true),
    ("asString", self::as_string, true),
    ("asDouble", self::as_double, true),
    ("atRandom", self::at_random, true),
    ("as32BitSignedValue", self::as_32bit_signed_value, true),
    ("as32BitUnsignedValue", self::as_32bit_unsigned_value, true),
];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] =
    &[("fromString:", self::from_string, true)];

macro_rules! demote {
    ($interpreter:expr, $heap:expr, $expr:expr) => {{
        let value = $expr;
        match value.to_i32() {
            Some(value) => SOMValue::new_integer(value),
            None => SOMValue::new_big_integer(&$heap.allocate(value)),
        }
    }};
}

fn from_string(interpreter: &mut Interpreter, heap: &mut GcHeap, universe: &mut Universe) {
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

    let parsed = value.parse().map(SOMValue::new_integer).or_else(|_| {
        value
            .parse()
            .map(|it| SOMValue::new_big_integer(&heap.allocate(it)))
    });

    match parsed {
        Ok(parsed) => {
            interpreter.stack.push(parsed);
        }
        Err(err) => panic!("{}", err),
    }
}

fn as_string(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#asString";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    let value = match value {
        Value::Integer(value) => value.to_string(),
        Value::BigInteger(value) => value.to_string(),
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter
        .stack
        .push(SOMValue::new_string(&heap.allocate(value)));
}

fn as_double(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#asDouble";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    let value = match value {
        Value::Integer(value) => SOMValue::new_double(value as f64),
        Value::BigInteger(value) => match value.to_i64() {
            Some(value) => SOMValue::new_double(value as f64),
            None => panic!(
                "'{}': `Integer` too big to be converted to `Double`",
                SIGNATURE
            ),
        },
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(value);
}

fn at_random(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
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

    interpreter.stack.push(SOMValue::new_integer(chosen));
}

fn as_32bit_signed_value(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#as32BitSignedValue";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    let value = match value {
        Value::Integer(value) => value,
        Value::BigInteger(value) => match value.to_u32_digits() {
            (Sign::Minus, values) => -(values[0] as i32),
            (Sign::Plus, values) | (Sign::NoSign, values) => values[0] as i32,
        },
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(SOMValue::new_integer(value));
}

fn as_32bit_unsigned_value(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#as32BitUnsignedValue";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    let value = match value {
        Value::Integer(value) => value as u32 as i32,
        Value::BigInteger(value) => {
            let (_, values) = value.to_u32_digits();
            values[0] as i32
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(SOMValue::new_integer(value));
}

fn plus(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#+";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    let value = match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => match a.checked_add(b) {
            Some(value) => SOMValue::new_integer(value),
            None => demote!(interpreter, heap, BigInt::from(a) + BigInt::from(b)),
        },
        (Value::BigInteger(a), Value::BigInteger(b)) => {
            demote!(interpreter, heap, a.as_ref() + b.as_ref())
        }
        (Value::BigInteger(a), Value::Integer(b)) | (Value::Integer(b), Value::BigInteger(a)) => {
            demote!(interpreter, heap, a.as_ref() + BigInt::from(b))
        }
        (Value::Double(a), Value::Double(b)) => SOMValue::new_double(a + b),
        (Value::Integer(a), Value::Double(b)) | (Value::Double(b), Value::Integer(a)) => {
            SOMValue::new_double((a as f64) + b)
        }
        (Value::BigInteger(a), Value::Double(b)) | (Value::Double(b), Value::BigInteger(a)) => {
            match a.to_f64() {
                Some(a) => SOMValue::new_double(a + b),
                None => panic!(
                    "'{}': `Integer` too big to be converted to `Double`",
                    SIGNATURE
                ),
            }
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(value);
}

fn minus(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#-";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    let value = match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => match a.checked_sub(b) {
            Some(value) => SOMValue::new_integer(value),
            None => demote!(interpreter, heap, BigInt::from(a) - BigInt::from(b)),
        },
        (Value::BigInteger(a), Value::BigInteger(b)) => {
            demote!(interpreter, heap, a.as_ref() - b.as_ref())
        }
        (Value::BigInteger(a), Value::Integer(b)) => {
            demote!(interpreter, heap, a.as_ref() - BigInt::from(b))
        }
        (Value::Integer(a), Value::BigInteger(b)) => {
            demote!(interpreter, heap, BigInt::from(a) - b.as_ref())
        }
        (Value::Double(a), Value::Double(b)) => SOMValue::new_double(a - b),
        (Value::Integer(a), Value::Double(b)) | (Value::Double(b), Value::Integer(a)) => {
            SOMValue::new_double((a as f64) - b)
        }
        (Value::BigInteger(a), Value::Double(b)) => match a.to_f64() {
            Some(a) => SOMValue::new_double(a - b),
            None => panic!(
                "'{}': `Integer` too big to be converted to `Double`",
                SIGNATURE
            ),
        },
        (Value::Double(a), Value::BigInteger(b)) => match b.to_f64() {
            Some(b) => SOMValue::new_double(a - b),
            None => panic!(
                "'{}': `Integer` too big to be converted to `Double`",
                SIGNATURE
            ),
        },
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(value);
}

fn times(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#*";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    let value = match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => match a.checked_mul(b) {
            Some(value) => SOMValue::new_integer(value),
            None => demote!(interpreter, heap, BigInt::from(a) * BigInt::from(b)),
        },
        (Value::BigInteger(a), Value::BigInteger(b)) => {
            demote!(interpreter, heap, a.as_ref() * b.as_ref())
        }
        (Value::BigInteger(a), Value::Integer(b)) | (Value::Integer(b), Value::BigInteger(a)) => {
            demote!(interpreter, heap, a.as_ref() * BigInt::from(b))
        }
        (Value::Double(a), Value::Double(b)) => SOMValue::new_double(a * b),
        (Value::Integer(a), Value::Double(b)) | (Value::Double(b), Value::Integer(a)) => {
            SOMValue::new_double((a as f64) * b)
        }
        (Value::BigInteger(a), Value::Double(b)) | (Value::Double(b), Value::BigInteger(a)) => {
            match a.to_f64() {
                Some(a) => SOMValue::new_double(a * b),
                None => panic!(
                    "'{}': `Integer` too big to be converted to `Double`",
                    SIGNATURE
                ),
            }
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(value);
}

fn divide(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#/";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    let value = match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => match a.checked_div(b) {
            Some(value) => SOMValue::new_integer(value),
            None => demote!(interpreter, heap, BigInt::from(a) / BigInt::from(b)),
        },
        (Value::BigInteger(a), Value::BigInteger(b)) => {
            demote!(interpreter, heap, a.as_ref() / b.as_ref())
        }
        (Value::BigInteger(a), Value::Integer(b)) => {
            demote!(interpreter, heap, a.as_ref() / BigInt::from(b))
        }
        (Value::Integer(a), Value::BigInteger(b)) => {
            demote!(interpreter, heap, BigInt::from(a) / b.as_ref())
        }
        (Value::Double(a), Value::Double(b)) => SOMValue::new_double(a / b),
        (Value::Integer(a), Value::Double(b)) | (Value::Double(b), Value::Integer(a)) => {
            SOMValue::new_double((a as f64) / b)
        }
        (Value::BigInteger(a), Value::Double(b)) => match a.to_f64() {
            Some(a) => SOMValue::new_double(a / b),
            None => panic!(
                "'{}': `Integer` too big to be converted to `Double`",
                SIGNATURE
            ),
        },
        (Value::Double(a), Value::BigInteger(b)) => match b.to_f64() {
            Some(b) => SOMValue::new_double(a / b),
            None => panic!(
                "'{}': `Integer` too big to be converted to `Double`",
                SIGNATURE
            ),
        },
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(value);
}

fn divide_float(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#//";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    let a = match a {
        Value::Integer(a) => a as f64,
        Value::BigInteger(a) => match a.to_f64() {
            Some(a) => a,
            None => panic!(
                "'{}': `Integer` too big to be converted to `Double`",
                SIGNATURE
            ),
        },
        Value::Double(a) => a,
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    let b = match b {
        Value::Integer(b) => b as f64,
        Value::BigInteger(b) => match b.to_f64() {
            Some(b) => b,
            None => panic!(
                "'{}': `Integer` too big to be converted to `Double`",
                SIGNATURE
            ),
        },
        Value::Double(b) => b,
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(SOMValue::new_double(a / b));
}

fn modulo(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#%";

    expect_args!(SIGNATURE, interpreter, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    let result = a % b;
    if result.signum() != b.signum() {
        interpreter
            .stack
            .push(SOMValue::new_integer((result + b) % b));
    } else {
        interpreter.stack.push(SOMValue::new_integer(result));
    }
}

fn remainder(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#rem:";

    expect_args!(SIGNATURE, interpreter, [
        Value::Integer(a) => a,
        Value::Integer(b) => b,
    ]);

    let result = a % b;
    if result.signum() != a.signum() {
        interpreter
            .stack
            .push(SOMValue::new_integer((result + a) % a));
    } else {
        interpreter.stack.push(SOMValue::new_integer(result));
    }
}

fn sqrt(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#sqrt";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
    ]);

    let value = match a {
        Value::Integer(a) => {
            let sqrt = (a as f64).sqrt();
            let trucated = sqrt.trunc();
            if sqrt == trucated {
                SOMValue::new_integer(trucated as i32)
            } else {
                SOMValue::new_double(sqrt)
            }
        }
        Value::BigInteger(a) => demote!(interpreter, heap, a.sqrt()),
        Value::Double(a) => SOMValue::new_double(a.sqrt()),
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(value);
}

fn bitand(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#&";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    let value = match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => SOMValue::new_integer(a & b),
        (Value::BigInteger(a), Value::BigInteger(b)) => {
            demote!(interpreter, heap, a.as_ref() & b.as_ref())
        }
        (Value::BigInteger(a), Value::Integer(b)) | (Value::Integer(b), Value::BigInteger(a)) => {
            demote!(interpreter, heap, a.as_ref() & BigInt::from(b))
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(value);
}

fn bitxor(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#bitXor:";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    let value = match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => SOMValue::new_integer(a ^ b),
        (Value::BigInteger(a), Value::BigInteger(b)) => {
            demote!(interpreter, heap, a.as_ref() ^ b.as_ref())
        }
        (Value::BigInteger(a), Value::Integer(b)) | (Value::Integer(b), Value::BigInteger(a)) => {
            demote!(interpreter, heap, a.as_ref() ^ BigInt::from(b))
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(value);
}

fn lt(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#<";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    let value = match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => SOMValue::new_boolean(a < b),
        (Value::BigInteger(a), Value::BigInteger(b)) => SOMValue::new_boolean(a < b),
        (Value::Double(a), Value::Double(b)) => SOMValue::new_boolean(a < b),
        (Value::Integer(a), Value::Double(b)) => SOMValue::new_boolean((a as f64) < b),
        (Value::Double(a), Value::Integer(b)) => SOMValue::new_boolean(a < (b as f64)),
        (Value::BigInteger(a), Value::Integer(b)) => {
            SOMValue::new_boolean(a.as_ref() < &BigInt::from(b))
        }
        (Value::Integer(a), Value::BigInteger(b)) => {
            SOMValue::new_boolean(&BigInt::from(a) < b.as_ref())
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(value);
}

fn eq(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#=";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    let value = match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => SOMValue::new_boolean(a == b),
        (Value::BigInteger(a), Value::BigInteger(b)) => SOMValue::new_boolean(a == b),
        (Value::Double(a), Value::Double(b)) => SOMValue::new_boolean(a == b),
        (Value::Integer(a), Value::Double(b)) => SOMValue::new_boolean((a as f64) == b),
        (Value::Double(a), Value::Integer(b)) => SOMValue::new_boolean(a == (b as f64)),
        _ => SOMValue::FALSE,
    };

    interpreter.stack.push(value);
}

fn shift_left(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#<<";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        Value::Integer(b) => b,
    ]);

    let value = match a {
        Value::Integer(a) => match a.checked_shl(b as u32) {
            Some(value) => SOMValue::new_integer(value),
            None => demote!(interpreter, heap, BigInt::from(a) << (b as usize)),
        },
        Value::BigInteger(a) => demote!(interpreter, heap, a.as_ref() << (b as usize)),
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(value);
}

fn shift_right(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Integer>>#>>";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        Value::Integer(b) => b,
    ]);

    let value = match a {
        Value::Integer(a) => match a.checked_shr(b as u32) {
            Some(value) => SOMValue::new_integer(value),
            None => demote!(interpreter, heap, BigInt::from(a) >> (b as usize)),
        },
        Value::BigInteger(a) => demote!(interpreter, heap, a.as_ref() >> (b as usize)),
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(value);
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
