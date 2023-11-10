use std::collections::hash_map::DefaultHasher;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};

use som_gc::GcHeap;

use crate::interpreter::Interpreter;
use crate::method::Method;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::{SOMValue, Value};
use crate::{expect_args, reverse};

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("class", self::class, true),
    ("objectSize", self::object_size, true),
    ("hashcode", self::hashcode, true),
    ("perform:", self::perform, true),
    ("perform:withArguments:", self::perform_with_arguments, true),
    ("perform:inSuperclass:", self::perform_in_super_class, true),
    (
        "perform:withArguments:inSuperclass:",
        self::perform_with_arguments_in_super_class,
        true,
    ),
    ("instVarAt:", self::inst_var_at, true),
    ("instVarAt:put:", self::inst_var_at_put, true),
    ("==", self::eq, true),
];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn class(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#class";

    expect_args!(SIGNATURE, interpreter, [
        object => object,
    ]);

    interpreter
        .stack
        .push(SOMValue::new_class(&object.class(universe)));
}

fn object_size(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const _: &'static str = "Object>>#objectSize";

    interpreter
        .stack
        .push(SOMValue::new_integer(std::mem::size_of::<Value>() as i32));
}

fn hashcode(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#hashcode";

    expect_args!(SIGNATURE, interpreter, [
        value => value,
    ]);

    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    let hash = (hasher.finish() as i32).abs();

    interpreter.stack.push(SOMValue::new_integer(hash));
}

fn eq(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#==";

    expect_args!(SIGNATURE, interpreter, [
        a => a,
        b => b,
    ]);

    interpreter.stack.push(SOMValue::new_boolean(a == b));
}

fn perform(interpreter: &mut Interpreter, heap: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#perform:";

    expect_args!(SIGNATURE, interpreter, [
        receiver => receiver,
        Value::Symbol(sym) => sym,
    ]);

    let receiver = SOMValue::from(receiver);
    let signature = universe.lookup_symbol(sym);
    let method = receiver.lookup_method(universe, sym);

    match method {
        Some(invokable) => Method::invoke(invokable, interpreter, heap, universe, receiver, vec![]),
        None => {
            let signature = signature.to_string();
            universe
                .does_not_understand(interpreter, heap, receiver.into(), sym, vec![receiver])
                .unwrap_or_else(|| {
                    panic!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        receiver.to_string(universe),
                    )
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_with_arguments(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
) {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:";

    expect_args!(SIGNATURE, interpreter, [
        receiver => receiver,
        Value::Symbol(sym) => sym,
        Value::Array(arr) => arr,
    ]);

    let receiver = SOMValue::from(receiver);
    let signature = universe.lookup_symbol(sym);
    let method = receiver.lookup_method(universe, sym);
    let args = arr.take().into_iter().map(SOMValue::from).collect();

    match method {
        Some(invokable) => {
            Method::invoke(invokable, interpreter, heap, universe, receiver, args);
        }
        None => {
            let signature = signature.to_string();
            let args = std::iter::once(receiver).chain(args).collect();
            universe
                .does_not_understand(interpreter, heap, receiver, sym, args)
                .unwrap_or_else(|| {
                    panic!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        receiver.to_string(universe)
                    )
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_in_super_class(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
) {
    const SIGNATURE: &'static str = "Object>>#perform:inSuperclass:";

    expect_args!(SIGNATURE, interpreter, [
        receiver => receiver,
        Value::Symbol(sym) => sym,
        Value::Class(class) => class,
    ]);

    let receiver = SOMValue::from(receiver);
    let signature = universe.lookup_symbol(sym);
    let method = class.borrow().lookup_method(sym);

    match method {
        Some(invokable) => Method::invoke(invokable, interpreter, heap, universe, receiver, vec![]),
        None => {
            let signature = signature.to_string();
            let args = vec![receiver];
            universe
                .does_not_understand(interpreter, heap, SOMValue::new_class(&class), sym, args)
                .unwrap_or_else(|| {
                    panic!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        receiver.to_string(universe)
                    )
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_with_arguments_in_super_class(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
) {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:inSuperclass:";

    expect_args!(SIGNATURE, interpreter, [
        receiver => receiver,
        Value::Symbol(sym) => sym,
        Value::Array(arr) => arr,
        Value::Class(class) => class,
    ]);

    let receiver = SOMValue::from(receiver);
    let signature = universe.lookup_symbol(sym);
    let method = class.borrow().lookup_method(sym);
    let args = arr.take().into_iter().map(SOMValue::from).collect();

    match method {
        Some(invokable) => Method::invoke(invokable, interpreter, heap, universe, receiver, args),
        None => {
            let args = std::iter::once(receiver).chain(args).collect();
            let signature = signature.to_string();
            universe
                .does_not_understand(interpreter, heap, SOMValue::new_class(&class), sym, args)
                .unwrap_or_else(|| {
                    panic!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        receiver.to_string(universe)
                    )
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn inst_var_at(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#instVarAt:";

    expect_args!(SIGNATURE, interpreter, [
        receiver => receiver,
        Value::Integer(index) => index,
    ]);

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    };

    let receiver = SOMValue::from(receiver);
    let local = receiver.lookup_local(index).unwrap_or(SOMValue::NIL);

    interpreter.stack.push(local);
}

fn inst_var_at_put(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#instVarAt:put:";

    expect_args!(SIGNATURE, interpreter, [
        receiver => receiver,
        Value::Integer(index) => index,
        value => value,
    ]);

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    };

    let mut receiver = SOMValue::from(receiver);
    let value = SOMValue::from(value);
    let local = receiver
        .assign_local(index, value)
        .map(|_| value)
        .unwrap_or(SOMValue::NIL);

    interpreter.stack.push(local);
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
