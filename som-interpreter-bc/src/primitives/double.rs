use anyhow::Error;
use num_traits::ToPrimitive;
use once_cell::sync::Lazy;

use som_gc::{Gc, GcHeap};

use crate::interpreter::Interpreter;
use crate::primitives::{DoubleLike, Primitive, PrimitiveFn};
use crate::universe::Universe;
use crate::value::SOMValue;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> = Lazy::new(|| {
    Box::new([
        ("+", self::plus.into_func(), true),
        ("-", self::minus.into_func(), true),
        ("*", self::times.into_func(), true),
        ("//", self::divide.into_func(), true),
        ("%", self::modulo.into_func(), true),
        ("=", self::eq.into_func(), true),
        ("<", self::lt.into_func(), true),
        ("sqrt", self::sqrt.into_func(), true),
        ("round", self::round.into_func(), true),
        ("cos", self::cos.into_func(), true),
        ("sin", self::sin.into_func(), true),
        ("asString", self::as_string.into_func(), true),
        ("asInteger", self::as_integer.into_func(), true),
    ])
});
pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> = Lazy::new(|| {
    Box::new([
        ("fromString:", self::from_string.into_func(), true),
        (
            "PositiveInfinity",
            self::positive_infinity.into_func(),
            true,
        ),
    ])
});

macro_rules! promote {
    ($signature:expr, $value:expr) => {
        match $value {
            DoubleLike::Double(value) => value,
            DoubleLike::Integer(value) => value as f64,
            DoubleLike::BigInteger(value) => match value.to_f64() {
                Some(value) => value,
                None => {
                    panic!(
                        "'{}': `Integer` too big to be converted to `Double`",
                        $signature
                    )
                }
            },
        }
    };
}

fn from_string(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
    string: Gc<String>,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#fromString:";

    let parsed = string.parse()?;
    interpreter.stack.push(SOMValue::new_double(parsed));

    Ok(())
}

fn as_string(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    receiver: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#asString";

    let receiver = promote!(SIGNATURE, receiver);

    let allocated = heap.allocate(receiver.to_string());
    interpreter.stack.push(SOMValue::new_string(&allocated));

    Ok(())
}

fn as_integer(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: f64,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#asInteger";

    interpreter
        .stack
        .push(SOMValue::new_integer(receiver.trunc() as i32));

    Ok(())
}

fn sqrt(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#sqrt";

    let receiver = promote!(SIGNATURE, receiver);

    interpreter
        .stack
        .push(SOMValue::new_double(receiver.sqrt()));

    Ok(())
}

fn round(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#round";

    let receiver = promote!(SIGNATURE, receiver);

    interpreter
        .stack
        .push(SOMValue::new_double(receiver.round()));

    Ok(())
}

fn cos(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#cos";

    let receiver = promote!(SIGNATURE, receiver);
    interpreter.stack.push(SOMValue::new_double(receiver.cos()));

    Ok(())
}

fn sin(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#sin";

    let receiver = promote!(SIGNATURE, receiver);
    interpreter.stack.push(SOMValue::new_double(receiver.sin()));

    Ok(())
}

fn eq(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    a: SOMValue,
    b: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#=";

    interpreter.stack.push(SOMValue::new_boolean(a == b));

    Ok(())
}

fn lt(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    a: DoubleLike,
    b: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#<";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    interpreter.stack.push(SOMValue::new_boolean(a < b));

    Ok(())
}

fn plus(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    a: DoubleLike,
    b: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#+";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    interpreter.stack.push(SOMValue::new_double(a + b));

    Ok(())
}

fn minus(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    a: DoubleLike,
    b: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#-";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    interpreter.stack.push(SOMValue::new_double(a - b));

    Ok(())
}

fn times(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    a: DoubleLike,
    b: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#*";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    interpreter.stack.push(SOMValue::new_double(a * b));

    Ok(())
}

fn divide(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    a: DoubleLike,
    b: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#//";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    interpreter.stack.push(SOMValue::new_double(a / b));

    Ok(())
}

fn modulo(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    a: DoubleLike,
    b: DoubleLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#%";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    interpreter.stack.push(SOMValue::new_double(a % b));

    Ok(())
}

fn positive_infinity(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Double>>#positiveInfinity";

    interpreter.stack.push(SOMValue::new_double(f64::INFINITY));

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
