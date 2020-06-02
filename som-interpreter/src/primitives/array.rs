use std::cell::RefCell;
use std::convert::TryFrom;
use std::rc::Rc;

use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

fn at(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Array>>#at:";

    let index = match args[1] {
        Value::Integer(value) => match usize::try_from(value - 1) {
            Ok(index) => index,
            Err(err) => return Return::Exception(format!("'{}': {}", SIGNATURE, err)),
        },
        _ => return Return::Exception(format!("'{}': invalid index type", SIGNATURE)),
    };
    match args[0] {
        Value::Array(ref values) => {
            Return::Local(values.borrow().get(index).cloned().unwrap_or(Value::Nil))
        }
        _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    }
}

fn at_put(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Array>>#at:put:";

    let mut iter = args.into_iter();
    let mut receiver = iter.next().unwrap();
    let index = iter.next().unwrap();
    let value = iter.next().unwrap();
    let index = match index {
        Value::Integer(value) => match usize::try_from(value - 1) {
            Ok(index) => index,
            Err(err) => return Return::Exception(format!("'{}': {}", SIGNATURE, err)),
        },
        _ => return Return::Exception(format!("'{}': invalid index type", SIGNATURE)),
    };
    match receiver {
        Value::Array(ref mut values) => {
            if let Some(location) = values.borrow_mut().get_mut(index as usize) {
                *location = value;
            }
            Return::Local(receiver)
        }
        _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    }
}

fn length(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Array>>#length";

    match args[0] {
        Value::Array(ref values) => match i64::try_from(values.borrow().len()) {
            Ok(length) => Return::Local(Value::Integer(length)),
            Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
        },
        _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    }
}

fn new(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Array>>#new:";

    match args[1] {
        Value::Integer(value) => match usize::try_from(value) {
            Ok(length) => Return::Local(Value::Array(Rc::new(RefCell::new(vec![
                Value::Nil;
                length
            ])))),
            Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
        },
        _ => Return::Exception(format!("'{}': invalid length type", SIGNATURE)),
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
