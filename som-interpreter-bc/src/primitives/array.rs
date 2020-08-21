use std::cell::RefCell;
use std::convert::TryFrom;
use std::rc::Rc;

use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, reverse};

fn at(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Array>>#at:";

    expect_args!(SIGNATURE, interpreter, [
        Value::Array(values) => values,
        Value::Integer(index) => index,
    ]);

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    };
    let value = values.borrow().get(index).cloned().unwrap_or(Value::Nil);
    interpreter.stack.push(value)
}

fn at_put(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Array>>#at:put:";

    expect_args!(SIGNATURE, interpreter, [
        Value::Array(values) => values,
        Value::Integer(index) => index,
        value => value,
    ]);

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    };
    if let Some(location) = values.borrow_mut().get_mut(index) {
        *location = value;
    }
    interpreter.stack.push(Value::Array(values))
}

fn length(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Array>>#length";

    expect_args!(SIGNATURE, interpreter, [
        Value::Array(values) => values,
    ]);

    let length = values.borrow().len();
    match i64::try_from(length) {
        Ok(length) => interpreter.stack.push(Value::Integer(length)),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn new(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Array>>#new:";

    expect_args!(SIGNATURE, interpreter, [
        _,
        Value::Integer(count) => count,
    ]);

    match usize::try_from(count) {
        Ok(length) => interpreter
            .stack
            .push(Value::Array(Rc::new(RefCell::new(vec![
                Value::Nil;
                length
            ])))),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "at:" => Some(self::at),
        "at:put:" => Some(self::at_put),
        "length" => Some(self::length),
        "new:" => Some(self::new),
        _ => None,
    }
}
