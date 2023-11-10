use som_gc::GcHeap;

use crate::interpreter::Interpreter;
use crate::method::Method;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::{SOMValue, Value};
use crate::{expect_args, reverse};

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("holder", self::holder, true),
    ("signature", self::signature, true),
    ("invokeOn:with:", self::invoke_on_with, true),
];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn holder(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "Method>>#holder";

    expect_args!(SIGNATURE, interpreter, [
        Value::Invokable(invokable) => invokable,
    ]);

    interpreter
        .stack
        .push(SOMValue::new_class(&invokable.holder));
}

fn signature(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "Method>>#signature";

    expect_args!(SIGNATURE, interpreter, [
        Value::Invokable(invokable) => invokable,
    ]);

    let sym = universe.intern_symbol(invokable.signature());
    interpreter.stack.push(SOMValue::new_symbol(sym))
}

fn invoke_on_with(interpreter: &mut Interpreter, heap: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "Method>>#invokeOn:with:";

    expect_args!(SIGNATURE, interpreter, [
        Value::Invokable(invokable) => invokable,
        receiver => receiver,
        Value::Array(args) => args,
    ]);

    let args = args.take().into_iter().map(SOMValue::from).collect();

    Method::invoke(
        invokable,
        interpreter,
        heap,
        universe,
        receiver.into(),
        args,
    );
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
