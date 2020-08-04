use std::cell::RefCell;
use std::rc::Rc;

use crate::instance::Instance;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, reverse};

fn superclass(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Class>>#superclass";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        Value::Class(class) => class,
    ]);

    let super_class = class.borrow().super_class();
    frame
        .borrow_mut()
        .stack
        .push(super_class.map(Value::Class).unwrap_or(Value::Nil));
}

fn new(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Class>>#new";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        Value::Class(class) => class,
    ]);

    let instance = Instance::from_class(class);
    let instance = Rc::new(RefCell::new(instance));
    frame.borrow_mut().stack.push(Value::Instance(instance));
}

fn name(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "Class>>#name";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        Value::Class(class) => class,
    ]);

    let sym = universe.intern_symbol(class.borrow().name());
    frame.borrow_mut().stack.push(Value::Symbol(sym));
}

fn methods(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Class>>#methods";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        Value::Class(class) => class,
    ]);

    let methods = class
        .borrow()
        .methods
        .values()
        .map(|invokable| Value::Invokable(invokable.clone()))
        .collect();

    frame
        .borrow_mut()
        .stack
        .push(Value::Array(Rc::new(RefCell::new(methods))));
}

fn fields(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Class>>#fields";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        Value::Class(class) => class,
    ]);

    frame
        .borrow_mut()
        .stack
        .push(Value::Array(Rc::new(RefCell::new(
            class
                .borrow()
                .locals
                .keys()
                .copied()
                .map(Value::Symbol)
                .collect(),
        ))));
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "new" => Some(self::new),
        "name" => Some(self::name),
        "fields" => Some(self::fields),
        "methods" => Some(self::methods),
        "superclass" => Some(self::superclass),
        _ => None,
    }
}
