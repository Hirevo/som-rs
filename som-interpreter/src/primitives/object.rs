use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

fn class(universe: &mut Universe, args: Vec<Value>) -> Return {
    const _: &'static str = "Object>>#class";

    Return::Local(Value::Class(args[0].class(universe)))
}

fn object_size(_: &mut Universe, _: Vec<Value>) -> Return {
    const _: &'static str = "Object>>#objectSize";

    Return::Local(Value::Integer(std::mem::size_of::<Value>() as i64))
}

fn eq(_: &mut Universe, args: Vec<Value>) -> Return {
    const _: &'static str = "Object>>#==";

    Return::Local(Value::Boolean(args[0] == args[1]))
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "class" => Some(self::class),
        "objectSize" => Some(self::object_size),
        "==" => Some(self::eq),
        _ => None,
    }
}
