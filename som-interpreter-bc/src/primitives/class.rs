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

    expect_args!(SIGNATURE, interpreter, [
        Value::Class(class) => class,
    ]);

    let super_class = class.borrow().super_class();
    interpreter
        .stack
        .push(super_class.map(Value::Class).unwrap_or(Value::Nil));
}

fn new(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Class>>#new";

    expect_args!(SIGNATURE, interpreter, [
        Value::Class(class) => class,
    ]);

    let instance = Instance::from_class(class);
    let instance = Rc::new(RefCell::new(instance));
    interpreter.stack.push(Value::Instance(instance));
}

fn name(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "Class>>#name";

    expect_args!(SIGNATURE, interpreter, [
        Value::Class(class) => class,
    ]);

    let sym = universe.intern_symbol(class.borrow().name());
    interpreter.stack.push(Value::Symbol(sym));
}

fn methods(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Class>>#methods";

    expect_args!(SIGNATURE, interpreter, [
        Value::Class(class) => class,
    ]);

    let methods = class
        .borrow()
        .methods
        .values()
        .map(|invokable| Value::Invokable(invokable.clone()))
        .collect();

    interpreter
        .stack
        .push(Value::Array(Rc::new(RefCell::new(methods))));
}

fn fields(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Class>>#fields";

    expect_args!(SIGNATURE, interpreter, [
        Value::Class(class) => class,
    ]);

    interpreter.stack.push(Value::Array(Rc::new(RefCell::new(
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
