use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

fn from_string(universe: &mut Universe, args: Vec<Value>) -> Value {
    let arg = args
        .into_iter()
        .next()
        .expect("no arguments to 'Integer>>#fromString:'");
    let value = match arg {
        Value::String(ref string) => string.parse().unwrap(),
        _ => panic!("wrong type"),
    };
    Value::Integer(value)
}

pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "fromString:" => Some(self::from_string),
        _ => None,
    }
}
