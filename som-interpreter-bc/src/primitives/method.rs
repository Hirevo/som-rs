use anyhow::Error;
use once_cell::sync::Lazy;

use som_gc::{Gc, GcHeap};

use crate::interpreter::Interpreter;
use crate::method::Method;
use crate::primitives::{Primitive, PrimitiveFn};
use crate::universe::Universe;
use crate::value::SOMValue;
use crate::SOMRef;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> = Lazy::new(|| {
    Box::new([
        ("holder", self::holder.into_func(), true),
        ("signature", self::signature.into_func(), true),
        ("invokeOn:with:", self::invoke_on_with.into_func(), true),
    ])
});
pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
    Lazy::new(|| Box::new([]));

fn holder(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    invokable: Gc<Method>,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Method>>#holder";

    interpreter
        .stack
        .push(SOMValue::new_class(&invokable.holder));

    Ok(())
}

fn signature(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    invokable: Gc<Method>,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Method>>#signature";

    let sym = universe.intern_symbol(invokable.signature());
    interpreter.stack.push(SOMValue::new_symbol(sym));

    Ok(())
}

fn invoke_on_with(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    invokable: Gc<Method>,
    receiver: SOMValue,
    arguments: SOMRef<Vec<SOMValue>>,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Method>>#invokeOn:with:";

    Method::invoke(
        invokable,
        interpreter,
        heap,
        universe,
        receiver,
        arguments.take(),
    )
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
