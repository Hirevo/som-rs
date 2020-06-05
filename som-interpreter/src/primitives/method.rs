use crate::expect_args;
use crate::invokable::{Invokable, Invoke, Return};
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

fn holder(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Method>>#holder";

    expect_args!(SIGNATURE, args, [
        Value::Invokable(holder, _) => holder,
    ]);

    Return::Local(Value::Class(holder))
}

fn signature(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Method>>#signature";

    expect_args!(SIGNATURE, args, [
        Value::Invokable(_, invokable) => invokable,
    ]);

    let sym = match invokable.as_ref() {
        Invokable::MethodDef(defn) => universe.intern_symbol(defn.signature.as_str()),
        _ => universe.intern_symbol("instance of Primitive"),
    };
    Return::Local(Value::Symbol(sym))
}

fn invoke_on_with(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Method>>#invokeOn:with:";

    expect_args!(SIGNATURE, args, [
        Value::Invokable(_, invokable) => invokable,
        receiver => receiver,
        Value::Array(args) => args,
    ]);

    let args = std::iter::once(receiver.clone())
        .chain(args.borrow().iter().cloned())
        .collect();
    invokable.invoke(universe, args)
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "holder" => Some(self::holder),
        "signature" => Some(self::signature),
        "invokeOn:with:" => Some(self::invoke_on_with),
        _ => None,
    }
}
