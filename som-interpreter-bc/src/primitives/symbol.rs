use std::rc::Rc;

use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, reverse};

fn as_string(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "Symbol>>#asString";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        Value::Symbol(sym) => sym,
    ]);

    frame.borrow_mut().stack.push(Value::String(Rc::new(
        universe.lookup_symbol(sym).to_string(),
    )));
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "asString" => Some(self::as_string),
        _ => None,
    }
}
