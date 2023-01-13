use std::rc::Rc;

use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, reverse};

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] =
    &[("asString", self::as_string, true)];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn as_string(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "Symbol>>#asString";

    expect_args!(SIGNATURE, interpreter, [
        Value::Symbol(sym) => sym,
    ]);

    interpreter.stack.push(Value::String(Rc::new(
        universe.lookup_symbol(sym).to_string(),
    )));
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
