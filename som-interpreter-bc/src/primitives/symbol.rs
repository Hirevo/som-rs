use anyhow::Error;
use once_cell::sync::Lazy;

use som_gc::GcHeap;

use crate::interner::Interned;
use crate::interpreter::Interpreter;
use crate::primitives::{Primitive, PrimitiveFn};
use crate::universe::Universe;
use crate::value::SOMValue;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
    Lazy::new(|| Box::new([("asString", self::as_string.into_func(), true)]));
pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
    Lazy::new(|| Box::new([]));

fn as_string(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    symbol: Interned,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Symbol>>#asString";

    let allocated = heap.allocate(universe.lookup_symbol(symbol).to_owned());
    interpreter.stack.push(SOMValue::new_string(&allocated));

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
