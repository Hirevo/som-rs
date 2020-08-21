use std::rc::Rc;

use crate::expect_args;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

fn as_string(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Symbol>>#asString";

    expect_args!(SIGNATURE, args, [
        Value::Symbol(sym) => sym,
    ]);

    Return::Local(Value::String(Rc::new(
        universe.lookup_symbol(sym).to_string(),
    )))
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "asString" => Some(self::as_string),
        _ => None,
    }
}
