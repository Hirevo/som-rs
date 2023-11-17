use std::cell::RefCell;
use std::convert::{TryFrom, TryInto};

use anyhow::{Context, Error};
use once_cell::sync::Lazy;

use som_gc::GcHeap;

use crate::convert::Primitive;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
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
    _: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMRef<Vec<SOMValue>>,
    index: i32,
) -> Result<SOMValue, Error> {
    const SIGNATURE: &str = "Array>>#at:";

    let index = usize::try_from(index - 1)?;

    receiver
        .borrow()
        .get(index)
        .cloned()
        .context("index out of bounds")
}

fn at_put(
    _: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMRef<Vec<SOMValue>>,
    index: i32,
    value: SOMValue,
) -> Result<SOMRef<Vec<SOMValue>>, Error> {
    const SIGNATURE: &str = "Array>>#at:put:";

    let index = usize::try_from(index - 1)?;

    if let Some(location) = receiver.borrow_mut().get_mut(index) {
        *location = value;
    }

    Ok(receiver)
}

fn length(
    _: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMRef<Vec<SOMValue>>,
) -> Result<i32, Error> {
    const SIGNATURE: &str = "Array>>#length";

    receiver
        .borrow()
        .len()
        .try_into()
        .context("could not convert `usize` to `i32`")
}

fn new(
    _: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
    count: i32,
) -> Result<SOMRef<Vec<SOMValue>>, Error> {
    const SIGNATURE: &str = "Array>>#new:";

    let count = usize::try_from(count)?;
    let allocated = heap.allocate(RefCell::new(vec![SOMValue::NIL; count]));

    Ok(allocated)
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
