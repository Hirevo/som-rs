use std::cell::RefCell;

use anyhow::Error;
use once_cell::sync::Lazy;

use som_gc::GcHeap;

use crate::class::Class;
use crate::instance::Instance;
use crate::interpreter::Interpreter;
use crate::primitives::{Primitive, PrimitiveFn};
use crate::universe::Universe;
use crate::value::SOMValue;
use crate::SOMRef;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> = Lazy::new(|| {
    Box::new({
        [
            ("new", self::new.into_func(), true),
            ("name", self::name.into_func(), true),
            ("fields", self::fields.into_func(), true),
            ("methods", self::methods.into_func(), true),
            ("superclass", self::superclass.into_func(), true),
        ]
    })
});
pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
    Lazy::new(|| Box::new([]));

fn superclass(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMRef<Class>,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Class>>#superclass";

    let super_class = receiver.borrow().super_class();
    let super_class = super_class.map_or(SOMValue::NIL, |it| SOMValue::new_class(&it));
    interpreter.stack.push(super_class);

    Ok(())
}

fn new(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMRef<Class>,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Class>>#new";

    let instance = Instance::from_class(receiver);
    let instance = heap.allocate(RefCell::new(instance));
    interpreter.stack.push(SOMValue::new_instance(&instance));

    Ok(())
}

fn name(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    receiver: SOMRef<Class>,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Class>>#name";

    let name = universe.intern_symbol(receiver.borrow().name());
    interpreter.stack.push(SOMValue::new_symbol(name));

    Ok(())
}

fn methods(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMRef<Class>,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Class>>#methods";

    let methods = receiver
        .borrow()
        .methods
        .values()
        .map(SOMValue::new_invokable)
        .collect();

    let allocated = heap.allocate(RefCell::new(methods));
    interpreter.stack.push(SOMValue::new_array(&allocated));

    Ok(())
}

fn fields(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMRef<Class>,
) -> Result<(), Error> {
    const SIGNATURE: &str = "Class>>#fields";

    let fields = receiver
        .borrow()
        .locals
        .keys()
        .copied()
        .map(SOMValue::new_symbol)
        .collect();

    let allocated = heap.allocate(RefCell::new(fields));
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
