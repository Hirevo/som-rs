use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, reverse};

macro_rules! promote {
    ($signature:expr, $value:expr) => {
        match $value {
            Value::Integer(value) => value as f64,
            Value::Double(value) => value,
            _ => panic!(
                "'{}': wrong type (expected `integer` or `double`)",
                $signature
            ),
        }
    };
}

fn from_string(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#fromString:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        _,
        Value::String(string) => string,
    ]);

    match string.parse() {
        Ok(parsed) => frame.borrow_mut().stack.push(Value::Double(parsed)),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn as_string(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#asString";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        value => value,
    ]);

    let value = promote!(SIGNATURE, value);

    frame
        .borrow_mut()
        .stack
        .push(Value::String(value.to_string().into()));
}

fn as_integer(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#asInteger";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        Value::Double(value) => value,
    ]);

    frame
        .borrow_mut()
        .stack
        .push(Value::Integer(value.trunc() as i64));
}

fn sqrt(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#sqrt";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        value => value,
    ]);

    let value = promote!(SIGNATURE, value);

    frame.borrow_mut().stack.push(Value::Double(value.sqrt()));
}

fn round(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#round";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        value => value,
    ]);

    let value = promote!(SIGNATURE, value);

    frame.borrow_mut().stack.push(Value::Double(value.round()));
}

fn cos(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#cos";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        value => value,
    ]);

    let value = promote!(SIGNATURE, value);

    frame.borrow_mut().stack.push(Value::Double(value.cos()));
}

fn sin(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#sin";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        value => value,
    ]);

    let value = promote!(SIGNATURE, value);

    frame.borrow_mut().stack.push(Value::Double(value.sin()));
}

fn eq(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#=";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        // Value::Double(a) => a,
        // Value::Double(b) => b,
        a => a,
        b => b,
    ]);

    frame.borrow_mut().stack.push(Value::Boolean(a == b));
}

fn lt(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#<";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    frame.borrow_mut().stack.push(Value::Boolean(a < b));
}

fn plus(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#+";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    frame.borrow_mut().stack.push(Value::Double(a + b));
}

fn minus(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#-";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    frame.borrow_mut().stack.push(Value::Double(a - b));
}

fn times(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#*";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    frame.borrow_mut().stack.push(Value::Double(a * b));
}

fn divide(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#//";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    frame.borrow_mut().stack.push(Value::Double(a / b));
}

fn modulo(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#%";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        a => a,
        b => b,
    ]);

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    frame.borrow_mut().stack.push(Value::Double(a % b));
}

fn positive_infinity(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Double>>#positiveInfinity";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [_]);

    let frame = interpreter.current_frame().expect("no current frame");

    frame.borrow_mut().stack.push(Value::Double(f64::INFINITY));
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
        "PositiveInfinity" => Some(self::positive_infinity),
        _ => None,
    }
}
