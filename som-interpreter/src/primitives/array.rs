use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

// TODO: properly convert `i64` (from `Value::Integer`) to `usize` (using `usize::try_from`)
fn at(universe: &mut Universe, args: Vec<Value>) -> Value {
    let mut iter = args.into_iter();
    let receiver = iter.next().expect("no arguments to 'Array>>#at:'");
    let index = iter.next().expect("no receiver for 'Array>>#at:'");
    let index = match index {
        Value::Integer(value) => value,
        _ => panic!("'Array>>#at:': invalid index type"),
    };
    match receiver {
        Value::Array(ref values) => values.get(index as usize).cloned().unwrap_or(Value::Nil),
        _ => panic!("'Array>>#at:': invalid self type"),
    }
}

// TODO: properly convert `i64` (from `Value::Integer`) to `usize` (using `usize::try_from`)
fn new(universe: &mut Universe, args: Vec<Value>) -> Value {
    let arg = args
        .into_iter()
        .next()
        .expect("no arguments to 'Array>>#new:'");
    let length = match arg {
        Value::Integer(value) => value,
        _ => panic!("'Array>>#new:': invalid length type"),
    };
    Value::Array(vec![Value::Nil; length as usize])
}

pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "at:" => Some(self::at),
        "new:" => Some(self::new),
        _ => None,
    }
}
