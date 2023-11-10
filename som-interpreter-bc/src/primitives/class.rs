use std::cell::RefCell;

use som_gc::GcHeap;

use crate::instance::Instance;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::{SOMValue, Value};
use crate::{expect_args, reverse};

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("new", self::new, true),
    ("name", self::name, true),
    ("fields", self::fields, true),
    ("methods", self::methods, true),
    ("superclass", self::superclass, true),
];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn superclass(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Class>>#superclass";

    expect_args!(SIGNATURE, interpreter, [
        Value::Class(class) => class,
    ]);

    let super_class = class.borrow().super_class();
    interpreter.stack.push(
        super_class
            .map(|it| SOMValue::new_class(&it))
            .unwrap_or(SOMValue::NIL),
    );
}

fn new(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Class>>#new";

    expect_args!(SIGNATURE, interpreter, [
        Value::Class(class) => class,
    ]);

    let instance = Instance::from_class(class);
    let instance = heap.allocate(RefCell::new(instance));
    interpreter.stack.push(SOMValue::new_instance(&instance));
}

fn name(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "Class>>#name";

    expect_args!(SIGNATURE, interpreter, [
        Value::Class(class) => class,
    ]);

    let sym = universe.intern_symbol(class.borrow().name());
    interpreter.stack.push(SOMValue::new_symbol(sym));
}

fn methods(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Class>>#methods";

    expect_args!(SIGNATURE, interpreter, [
        Value::Class(class) => class,
    ]);

    let methods = class
        .borrow()
        .methods
        .values()
        .map(|invokable| SOMValue::new_invokable(invokable))
        .collect();

    interpreter
        .stack
        .push(SOMValue::new_array(&heap.allocate(RefCell::new(methods))));
}

fn fields(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Class>>#fields";

    expect_args!(SIGNATURE, interpreter, [
        Value::Class(class) => class,
    ]);

    interpreter.stack.push(SOMValue::new_array(
        &heap.allocate(RefCell::new(
            class
                .borrow()
                .locals
                .keys()
                .copied()
                .map(SOMValue::new_symbol)
                .collect(),
        )),
    ));
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
