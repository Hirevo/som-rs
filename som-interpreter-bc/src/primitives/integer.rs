use std::convert::TryFrom;

use anyhow::{bail, Context, Error};
use num_bigint::{BigInt, Sign};
use num_traits::{Signed, ToPrimitive};
use once_cell::sync::Lazy;
use rand::distributions::Uniform;
use rand::Rng;

use som_gc::GcHeap;

use crate::interpreter::Interpreter;
use crate::primitives::{DoubleLike, IntegerLike, Primitive, PrimitiveFn, StringLike};
use crate::universe::Universe;
use crate::value::SOMValue;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> = Lazy::new(|| {
    Box::new([
        ("<", self::lt.into_func(), true),
        ("=", self::eq.into_func(), true),
        ("+", self::plus.into_func(), true),
        ("-", self::minus.into_func(), true),
        ("*", self::times.into_func(), true),
        ("/", self::divide.into_func(), true),
        ("//", self::divide_float.into_func(), true),
        ("%", self::modulo.into_func(), true),
        ("rem:", self::remainder.into_func(), true),
        ("&", self::bitand.into_func(), true),
        ("<<", self::shift_left.into_func(), true),
        (">>>", self::shift_right.into_func(), true),
        ("bitXor:", self::bitxor.into_func(), true),
        ("sqrt", self::sqrt.into_func(), true),
        ("asString", self::as_string.into_func(), true),
        ("asDouble", self::as_double.into_func(), true),
        ("atRandom", self::at_random.into_func(), true),
        (
            "as32BitSignedValue",
            self::as_32bit_signed_value.into_func(),
            true,
        ),
        (
            "as32BitUnsignedValue",
            self::as_32bit_unsigned_value.into_func(),
            true,
        ),
    ])
});
pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
    Lazy::new(|| Box::new([("fromString:", self::from_string.into_func(), true)]));

macro_rules! demote {
    ($interpreter:expr, $heap:expr, $expr:expr) => {{
        let value = $expr;
        match value.to_i32() {
            Some(value) => SOMValue::new_integer(value),
            None => SOMValue::new_big_integer(&$heap.allocate(value)),
        }
    }};
}

fn from_string(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    string: StringLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#fromString:";

    let string = match string {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    let parsed = string.parse().map(SOMValue::new_integer).or_else(|_| {
        string.parse().map(|value: BigInt| {
            let allocated = heap.allocate(value);
            SOMValue::new_big_integer(&allocated)
        })
    })?;

    interpreter.stack.push(parsed);

    Ok(())
}

fn as_string(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    receiver: IntegerLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#asString";

    let receiver = match receiver {
        IntegerLike::Integer(value) => value.to_string(),
        IntegerLike::BigInteger(value) => value.to_string(),
    };

    let allocated = heap.allocate(receiver);
    interpreter.stack.push(SOMValue::new_string(&allocated));

    Ok(())
}

fn as_double(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: IntegerLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#asDouble";

    let receiver = match receiver {
        IntegerLike::Integer(value) => SOMValue::new_double(value as f64),
        IntegerLike::BigInteger(value) => {
            let value = value
                .to_f64()
                .context("could not convert big integer to f64")?;
            SOMValue::new_double(value)
        }
    };

    interpreter.stack.push(receiver);

    Ok(())
}

fn at_random(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: IntegerLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#atRandom";

    let chosen = match receiver {
        IntegerLike::Integer(value) => {
            let distribution = Uniform::new(0, value);
            let mut rng = rand::thread_rng();
            rng.sample(distribution)
        }
        IntegerLike::BigInteger(_) => {
            bail!("'{SIGNATURE}': the range is too big to pick a random value from");
        }
    };

    interpreter.stack.push(SOMValue::new_integer(chosen));

    Ok(())
}

fn as_32bit_signed_value(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: IntegerLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#as32BitSignedValue";

    let receiver = match receiver {
        IntegerLike::Integer(value) => value,
        IntegerLike::BigInteger(value) => match value.to_u32_digits() {
            (Sign::Minus, values) => -(values[0] as i32),
            (Sign::Plus, values) | (Sign::NoSign, values) => values[0] as i32,
        },
    };

    interpreter.stack.push(SOMValue::new_integer(receiver));

    Ok(())
}

fn as_32bit_unsigned_value(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: IntegerLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#as32BitUnsignedValue";

    let receiver = match receiver {
        IntegerLike::Integer(value) => value as u32 as i32,
        IntegerLike::BigInteger(value) => {
            let (_, values) = value.to_u32_digits();
            values[0] as i32
        }
    };

    interpreter.stack.push(SOMValue::new_integer(receiver));

    Ok(())
}

fn plus(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    a: DoubleLike,
    b: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#+";

    let value = match (a, b) {
        (DoubleLike::Integer(a), DoubleLike::Integer(b)) => match a.checked_add(b) {
            Some(value) => SOMValue::new_integer(value),
            None => demote!(interpreter, heap, BigInt::from(a) + BigInt::from(b)),
        },
        (DoubleLike::BigInteger(a), DoubleLike::BigInteger(b)) => {
            demote!(interpreter, heap, a.as_ref() + b.as_ref())
        }
        (DoubleLike::BigInteger(a), DoubleLike::Integer(b))
        | (DoubleLike::Integer(b), DoubleLike::BigInteger(a)) => {
            demote!(interpreter, heap, a.as_ref() + BigInt::from(b))
        }
        (DoubleLike::Double(a), DoubleLike::Double(b)) => SOMValue::new_double(a + b),
        (DoubleLike::Integer(a), DoubleLike::Double(b))
        | (DoubleLike::Double(b), DoubleLike::Integer(a)) => SOMValue::new_double((a as f64) + b),
        (DoubleLike::BigInteger(a), DoubleLike::Double(b))
        | (DoubleLike::Double(b), DoubleLike::BigInteger(a)) => match a.to_f64() {
            Some(a) => SOMValue::new_double(a + b),
            None => panic!(
                "'{}': `Integer` too big to be converted to `Double`",
                SIGNATURE
            ),
        },
    };

    interpreter.stack.push(value);

    Ok(())
}

fn minus(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    a: DoubleLike,
    b: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#-";

    let value = match (a, b) {
        (DoubleLike::Integer(a), DoubleLike::Integer(b)) => match a.checked_sub(b) {
            Some(value) => SOMValue::new_integer(value),
            None => demote!(interpreter, heap, BigInt::from(a) - BigInt::from(b)),
        },
        (DoubleLike::BigInteger(a), DoubleLike::BigInteger(b)) => {
            demote!(interpreter, heap, a.as_ref() - b.as_ref())
        }
        (DoubleLike::BigInteger(a), DoubleLike::Integer(b)) => {
            demote!(interpreter, heap, a.as_ref() - BigInt::from(b))
        }
        (DoubleLike::Integer(a), DoubleLike::BigInteger(b)) => {
            demote!(interpreter, heap, BigInt::from(a) - b.as_ref())
        }
        (DoubleLike::Double(a), DoubleLike::Double(b)) => SOMValue::new_double(a - b),
        (DoubleLike::Integer(a), DoubleLike::Double(b))
        | (DoubleLike::Double(b), DoubleLike::Integer(a)) => SOMValue::new_double((a as f64) - b),
        (DoubleLike::BigInteger(a), DoubleLike::Double(b)) => match a.to_f64() {
            Some(a) => SOMValue::new_double(a - b),
            None => {
                bail!("'{SIGNATURE}': `Integer` too big to be converted to `Double`");
            }
        },
        (DoubleLike::Double(a), DoubleLike::BigInteger(b)) => match b.to_f64() {
            Some(b) => SOMValue::new_double(a - b),
            None => {
                bail!("'{SIGNATURE}': `Integer` too big to be converted to `Double`");
            }
        },
    };

    interpreter.stack.push(value);

    Ok(())
}

fn times(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    a: DoubleLike,
    b: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#*";

    let value = match (a, b) {
        (DoubleLike::Integer(a), DoubleLike::Integer(b)) => match a.checked_mul(b) {
            Some(value) => SOMValue::new_integer(value),
            None => demote!(interpreter, heap, BigInt::from(a) * BigInt::from(b)),
        },
        (DoubleLike::BigInteger(a), DoubleLike::BigInteger(b)) => {
            demote!(interpreter, heap, a.as_ref() * b.as_ref())
        }
        (DoubleLike::BigInteger(a), DoubleLike::Integer(b))
        | (DoubleLike::Integer(b), DoubleLike::BigInteger(a)) => {
            demote!(interpreter, heap, a.as_ref() * BigInt::from(b))
        }
        (DoubleLike::Double(a), DoubleLike::Double(b)) => SOMValue::new_double(a * b),
        (DoubleLike::Integer(a), DoubleLike::Double(b))
        | (DoubleLike::Double(b), DoubleLike::Integer(a)) => SOMValue::new_double((a as f64) * b),
        (DoubleLike::BigInteger(a), DoubleLike::Double(b))
        | (DoubleLike::Double(b), DoubleLike::BigInteger(a)) => match a.to_f64() {
            Some(a) => SOMValue::new_double(a * b),
            None => {
                bail!("'{SIGNATURE}': `Integer` too big to be converted to `Double`");
            }
        },
    };

    interpreter.stack.push(value);

    Ok(())
}

fn divide(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    a: DoubleLike,
    b: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#/";

    let value = match (a, b) {
        (DoubleLike::Integer(a), DoubleLike::Integer(b)) => match a.checked_div(b) {
            Some(value) => SOMValue::new_integer(value),
            None => demote!(interpreter, heap, BigInt::from(a) / BigInt::from(b)),
        },
        (DoubleLike::BigInteger(a), DoubleLike::BigInteger(b)) => {
            demote!(interpreter, heap, a.as_ref() / b.as_ref())
        }
        (DoubleLike::BigInteger(a), DoubleLike::Integer(b)) => {
            demote!(interpreter, heap, a.as_ref() / BigInt::from(b))
        }
        (DoubleLike::Integer(a), DoubleLike::BigInteger(b)) => {
            demote!(interpreter, heap, BigInt::from(a) / b.as_ref())
        }
        (DoubleLike::Double(a), DoubleLike::Double(b)) => SOMValue::new_double(a / b),
        (DoubleLike::Integer(a), DoubleLike::Double(b))
        | (DoubleLike::Double(b), DoubleLike::Integer(a)) => SOMValue::new_double((a as f64) / b),
        (DoubleLike::BigInteger(a), DoubleLike::Double(b)) => match a.to_f64() {
            Some(a) => SOMValue::new_double(a / b),
            None => {
                bail!("'{SIGNATURE}': `Integer` too big to be converted to `Double`");
            }
        },
        (DoubleLike::Double(a), DoubleLike::BigInteger(b)) => match b.to_f64() {
            Some(b) => SOMValue::new_double(a / b),
            None => {
                bail!("'{SIGNATURE}': `Integer` too big to be converted to `Double`");
            }
        },
    };

    interpreter.stack.push(value);

    Ok(())
}

fn divide_float(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    a: DoubleLike,
    b: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#//";

    let a = match a {
        DoubleLike::Double(a) => a,
        DoubleLike::Integer(a) => a as f64,
        DoubleLike::BigInteger(a) => match a.to_f64() {
            Some(a) => a,
            None => {
                bail!("'{SIGNATURE}': `Integer` too big to be converted to `Double`");
            }
        },
    };

    let b = match b {
        DoubleLike::Double(b) => b,
        DoubleLike::Integer(b) => b as f64,
        DoubleLike::BigInteger(b) => match b.to_f64() {
            Some(b) => b,
            None => {
                bail!("'{SIGNATURE}': `Integer` too big to be converted to `Double`");
            }
        },
    };

    interpreter.stack.push(SOMValue::new_double(a / b));

    Ok(())
}

fn modulo(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    a: IntegerLike,
    b: i32,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#%";

    let result = match a {
        IntegerLike::Integer(a) => {
            let result = a % b;
            if result.signum() != b.signum() {
                SOMValue::new_integer((result + b) % b)
            } else {
                SOMValue::new_integer(result)
            }
        }
        IntegerLike::BigInteger(a) => {
            let result = a.as_ref() % b;
            if result.is_positive() != b.is_positive() {
                demote!(interpreter, heap, (result + b) % b)
            } else {
                demote!(interpreter, heap, result)
            }
        }
    };

    interpreter.stack.push(result);

    Ok(())
}

fn remainder(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    a: i32,
    b: i32,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#rem:";

    let result = a % b;
    if result.signum() != a.signum() {
        interpreter
            .stack
            .push(SOMValue::new_integer((result + a) % a));
    } else {
        interpreter.stack.push(SOMValue::new_integer(result));
    }

    Ok(())
}

fn sqrt(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    a: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#sqrt";

    let value = match a {
        DoubleLike::Double(a) => SOMValue::new_double(a.sqrt()),
        DoubleLike::Integer(a) => {
            let sqrt = (a as f64).sqrt();
            let trucated = sqrt.trunc();
            if sqrt == trucated {
                SOMValue::new_integer(trucated as i32)
            } else {
                SOMValue::new_double(sqrt)
            }
        }
        DoubleLike::BigInteger(a) => demote!(interpreter, heap, a.sqrt()),
    };

    interpreter.stack.push(value);

    Ok(())
}

fn bitand(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    a: IntegerLike,
    b: IntegerLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#&";

    let value = match (a, b) {
        (IntegerLike::Integer(a), IntegerLike::Integer(b)) => SOMValue::new_integer(a & b),
        (IntegerLike::BigInteger(a), IntegerLike::BigInteger(b)) => {
            demote!(interpreter, heap, a.as_ref() & b.as_ref())
        }
        (IntegerLike::BigInteger(a), IntegerLike::Integer(b))
        | (IntegerLike::Integer(b), IntegerLike::BigInteger(a)) => {
            demote!(interpreter, heap, a.as_ref() & BigInt::from(b))
        }
    };

    interpreter.stack.push(value);

    Ok(())
}

fn bitxor(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    a: IntegerLike,
    b: IntegerLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#bitXor:";

    let value = match (a, b) {
        (IntegerLike::Integer(a), IntegerLike::Integer(b)) => SOMValue::new_integer(a ^ b),
        (IntegerLike::BigInteger(a), IntegerLike::BigInteger(b)) => {
            demote!(interpreter, heap, a.as_ref() ^ b.as_ref())
        }
        (IntegerLike::BigInteger(a), IntegerLike::Integer(b))
        | (IntegerLike::Integer(b), IntegerLike::BigInteger(a)) => {
            demote!(interpreter, heap, a.as_ref() ^ BigInt::from(b))
        }
    };

    interpreter.stack.push(value);

    Ok(())
}

fn lt(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    a: DoubleLike,
    b: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#<";

    let value = match (a, b) {
        (DoubleLike::Integer(a), DoubleLike::Integer(b)) => SOMValue::new_boolean(a < b),
        (DoubleLike::BigInteger(a), DoubleLike::BigInteger(b)) => SOMValue::new_boolean(a < b),
        (DoubleLike::Double(a), DoubleLike::Double(b)) => SOMValue::new_boolean(a < b),
        (DoubleLike::Integer(a), DoubleLike::Double(b)) => SOMValue::new_boolean((a as f64) < b),
        (DoubleLike::Double(a), DoubleLike::Integer(b)) => SOMValue::new_boolean(a < (b as f64)),
        (DoubleLike::BigInteger(a), DoubleLike::Integer(b)) => {
            SOMValue::new_boolean(a.as_ref() < &BigInt::from(b))
        }
        (DoubleLike::Integer(a), DoubleLike::BigInteger(b)) => {
            SOMValue::new_boolean(&BigInt::from(a) < b.as_ref())
        }
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    interpreter.stack.push(value);

    Ok(())
}

fn eq(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    a: SOMValue,
    b: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#=";

    let Ok(a) = DoubleLike::try_from(a) else {
        interpreter.stack.push(SOMValue::FALSE);
        return Ok(());
    };

    let Ok(b) = DoubleLike::try_from(b) else {
        interpreter.stack.push(SOMValue::FALSE);
        return Ok(());
    };

    let value = match (a, b) {
        (DoubleLike::Integer(a), DoubleLike::Integer(b)) => SOMValue::new_boolean(a == b),
        (DoubleLike::BigInteger(a), DoubleLike::BigInteger(b)) => SOMValue::new_boolean(a == b),
        (DoubleLike::Double(a), DoubleLike::Double(b)) => SOMValue::new_boolean(a == b),
        (DoubleLike::Integer(a), DoubleLike::Double(b)) => SOMValue::new_boolean((a as f64) == b),
        (DoubleLike::Double(a), DoubleLike::Integer(b)) => SOMValue::new_boolean(a == (b as f64)),
        _ => SOMValue::FALSE,
    };

    interpreter.stack.push(value);

    Ok(())
}

fn shift_left(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    a: IntegerLike,
    b: i32,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#<<";

    let value = match a {
        IntegerLike::Integer(a) => match a.checked_shl(b as u32) {
            Some(value) => SOMValue::new_integer(value),
            None => demote!(interpreter, heap, BigInt::from(a) << (b as usize)),
        },
        IntegerLike::BigInteger(a) => demote!(interpreter, heap, a.as_ref() << (b as usize)),
    };

    interpreter.stack.push(value);

    Ok(())
}

fn shift_right(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    a: IntegerLike,
    b: i32,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Integer>>#>>";

    let value = match a {
        IntegerLike::Integer(a) => match a.checked_shr(b as u32) {
            Some(value) => SOMValue::new_integer(value),
            None => demote!(interpreter, heap, BigInt::from(a) >> (b as usize)),
        },
        IntegerLike::BigInteger(a) => demote!(interpreter, heap, a.as_ref() >> (b as usize)),
    };

    interpreter.stack.push(value);

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
