use std::cell::RefCell;

use anyhow::Error;
use once_cell::sync::Lazy;

use som_gc::GcHeap;

use crate::class::Class;
use crate::convert::Primitive;
use crate::instance::Instance;
use crate::interner::Interned;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
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
    const _: &str = "Class>>#superclass";

    let super_class = receiver.borrow().super_class();
    let super_class = super_class.map_or(Value::NIL, |it| Value::new_class(&it));
    interpreter.stack.push(super_class);

    Ok(())
}

fn new(
    _: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMRef<Class>,
) -> Result<SOMRef<Instance>, Error> {
    const _: &str = "Class>>#new";

    let instance = Instance::from_class(receiver);
    let instance = heap.allocate(RefCell::new(instance));

    Ok(instance)
}

fn name(
    _: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    receiver: SOMRef<Class>,
) -> Result<Interned, Error> {
    const _: &str = "Class>>#name";

    Ok(universe.intern_symbol(receiver.borrow().name()))
}

fn methods(
    _: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMRef<Class>,
) -> Result<SOMRef<Vec<Value>>, Error> {
    const _: &str = "Class>>#methods";

    let methods = receiver
        .borrow()
        .methods
        .values()
        .map(Value::new_invokable)
        .collect();

    Ok(heap.allocate(RefCell::new(methods)))
}

fn fields(
    _: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMRef<Class>,
) -> Result<SOMRef<Vec<Value>>, Error> {
    const _: &str = "Class>>#fields";

    let fields = receiver
        .borrow()
        .locals
        .keys()
        .copied()
        .map(Value::new_symbol)
        .collect();

    Ok(heap.allocate(RefCell::new(fields)))
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
