use std::cell::RefCell;
use std::convert::TryFrom;
use std::rc::Rc;

use crate::expect_args;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

fn at(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Array>>#at:";

    expect_args!(SIGNATURE, args, [
        Value::Array(values) => values,
        Value::Integer(index) => index,
    ]);

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => return Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    };
    let value = values.borrow().get(index).cloned().unwrap_or(Value::Nil);
    Return::Local(value)
}

fn at_put(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Array>>#at:put:";

    expect_args!(SIGNATURE, args, [
        Value::Array(values) => values,
        Value::Integer(index) => index,
        value => value,
    ]);

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => return Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    };
    if let Some(location) = values.borrow_mut().get_mut(index) {
        *location = value;
    }
    Return::Local(Value::Array(values))
}

fn length(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Array>>#length";

    expect_args!(SIGNATURE, args, [
        Value::Array(values) => values,
    ]);

    let length = values.borrow().len();
    match i64::try_from(length) {
        Ok(length) => Return::Local(Value::Integer(length)),
        Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    }
}

fn new(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Array>>#new:";

    expect_args!(SIGNATURE, args, [
        _,
        Value::Integer(count) => count,
    ]);

    match usize::try_from(count) {
        Ok(length) => Return::Local(Value::Array(Rc::new(RefCell::new(vec![
            Value::Nil;
            length
        ])))),
        Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
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
