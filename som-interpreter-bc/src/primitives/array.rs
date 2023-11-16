use std::cell::RefCell;
use std::convert::{TryFrom, TryInto};

use anyhow::Error;
use once_cell::sync::Lazy;

use som_gc::GcHeap;

use crate::interpreter::Interpreter;
use crate::primitives::{Primitive, PrimitiveFn};
use crate::universe::Universe;
use crate::value::SOMValue;
use crate::SOMRef;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> = Lazy::new(|| {
    Box::new([
        ("at:", self::at.into_func(), true),
        ("at:put:", self::at_put.into_func(), true),
        ("length", self::length.into_func(), true),
    ])
});

pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
    Lazy::new(|| Box::new([("new:", self::new.into_func(), true)]));

fn at(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMRef<Vec<SOMValue>>,
    index: i32,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Array>>#at:";

    let index = usize::try_from(index - 1)?;
    let value = receiver
        .borrow()
        .get(index)
        .cloned()
        .unwrap_or(SOMValue::NIL);

    interpreter.stack.push(value);

    Ok(())
}

fn at_put(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMRef<Vec<SOMValue>>,
    index: i32,
    value: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Array>>#at:put:";

    let index = usize::try_from(index - 1)?;

    if let Some(location) = receiver.borrow_mut().get_mut(index) {
        *location = value;
    }

    interpreter.stack.push(SOMValue::new_array(&receiver));

    Ok(())
}

fn length(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMRef<Vec<SOMValue>>,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Array>>#length";

    let length = receiver.borrow().len().try_into()?;
    interpreter.stack.push(SOMValue::new_integer(length));

    Ok(())
}

fn new(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
    count: i32,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Array>>#new:";

    let count = usize::try_from(count)?;
    let allocated = heap.allocate(RefCell::new(vec![SOMValue::NIL; count]));
    interpreter.stack.push(SOMValue::new_array(&allocated));

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
