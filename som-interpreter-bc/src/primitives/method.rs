use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, reverse};

fn holder(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &str = "Method>>#holder";

    expect_args!(SIGNATURE, interpreter, [
        Value::Invokable(invokable) => invokable,
    ]);

    match invokable.holder().upgrade() {
        Some(holder) => interpreter.stack.push(Value::Class(holder)),
        None => panic!("'{}': method sholder has been collected", SIGNATURE),
    }
}

fn signature(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "Method>>#signature";

    expect_args!(SIGNATURE, interpreter, [
        Value::Invokable(invokable) => invokable,
    ]);

    let sym = universe.intern_symbol(invokable.signature());
    interpreter.stack.push(Value::Symbol(sym))
}

fn invoke_on_with(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &str = "Method>>#invokeOn:with:";

    expect_args!(SIGNATURE, interpreter, [
        Value::Invokable(invokable) => invokable,
        receiver => receiver,
        Value::Array(args) => args,
    ]);

    let args = args.borrow().iter().cloned().collect();
    invokable.invoke(interpreter, universe, receiver, args);
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
