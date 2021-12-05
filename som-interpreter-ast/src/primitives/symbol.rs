use std::rc::Rc;

use crate::expect_args;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] =
    &[("asString", self::as_string, true), ("=", self::eq, false)];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn as_string(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Symbol>>#asString";

    expect_args!(SIGNATURE, args, [
        Value::Symbol(sym) => sym,
    ]);

    Return::Local(Value::String(Rc::new(
        universe.lookup_symbol(sym).to_string(),
    )))
}

fn eq(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Symbol>>#=";

    expect_args!(SIGNATURE, args, [
        sym @ Value::Symbol(_) => sym,
        other => other,
    ]);

    Return::Local(Value::Boolean(sym == other))
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
