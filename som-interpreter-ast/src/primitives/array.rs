use std::cell::RefCell;
use std::convert::TryFrom;
use std::rc::Rc;

use crate::expect_args;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("at:", self::at, true),
    ("at:put:", self::at_put, true),
    ("length", self::length, true),
];

pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[("new:", self::new, true)];

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
