use std::collections::hash_map::DefaultHasher;
use std::convert::{TryFrom, TryInto};
use std::hash::{Hash, Hasher};

use anyhow::{anyhow, Context, Error};
use once_cell::sync::Lazy;

use som_gc::GcHeap;

use crate::class::Class;
use crate::interner::Interned;
use crate::interpreter::Interpreter;
use crate::method::Method;
use crate::primitives::{Primitive, PrimitiveFn};
use crate::universe::Universe;
use crate::value::SOMValue;
use crate::SOMRef;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> = Lazy::new(|| {
    Box::new([
        ("class", self::class.into_func(), true),
        ("objectSize", self::object_size.into_func(), true),
        ("hashcode", self::hashcode.into_func(), true),
        ("perform:", self::perform.into_func(), true),
        (
            "perform:withArguments:",
            self::perform_with_arguments.into_func(),
            true,
        ),
        (
            "perform:inSuperclass:",
            self::perform_in_super_class.into_func(),
            true,
        ),
        (
            "perform:withArguments:inSuperclass:",
            self::perform_with_arguments_in_super_class.into_func(),
            true,
        ),
        ("instVarAt:", self::inst_var_at.into_func(), true),
        ("instVarAt:put:", self::inst_var_at_put.into_func(), true),
        ("==", self::eq.into_func(), true),
    ])
});
pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
    Lazy::new(|| Box::new([]));

fn class(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    receiver: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "Object>>#class";

    interpreter
        .stack
        .push(SOMValue::new_class(&receiver.class(universe)));

    Ok(())
}

fn object_size(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMValue,
) -> Result<(), Error> {
    const _: &'static str = "Object>>#objectSize";

    let size = std::mem::size_of_val(&receiver).try_into()?;
    interpreter.stack.push(SOMValue::new_integer(size));

    Ok(())
}

fn hashcode(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "Object>>#hashcode";

    let mut hasher = DefaultHasher::new();
    receiver.hash(&mut hasher);
    let hash = (hasher.finish() as i32).abs();

    interpreter.stack.push(SOMValue::new_integer(hash));

    Ok(())
}

fn eq(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMValue,
    other: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "Object>>#==";

    interpreter
        .stack
        .push(SOMValue::new_boolean(receiver == other));

    Ok(())
}

fn perform(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    receiver: SOMValue,
    signature: Interned,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "Object>>#perform:";

    let method = receiver.lookup_method(universe, signature);

    match method {
        Some(invokable) => Method::invoke(invokable, interpreter, heap, universe, receiver, vec![]),
        None => {
            let signature_str = universe.lookup_symbol(signature).to_owned();
            universe
                .does_not_understand(
                    interpreter,
                    heap,
                    receiver.into(),
                    signature,
                    vec![receiver],
                )
                .with_context(|| {
                    anyhow!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature_str,
                        receiver.to_string(universe),
                    )
                })
        }
    }
}

fn perform_with_arguments(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    receiver: SOMValue,
    signature: Interned,
    arguments: SOMRef<Vec<SOMValue>>,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:";

    let method = receiver.lookup_method(universe, signature);

    match method {
        Some(invokable) => Method::invoke(
            invokable,
            interpreter,
            heap,
            universe,
            receiver,
            arguments.take(),
        ),
        None => {
            let signature_str = universe.lookup_symbol(signature).to_owned();
            let args = std::iter::once(receiver).chain(arguments.take()).collect();
            universe
                .does_not_understand(interpreter, heap, receiver, signature, args)
                .with_context(|| {
                    anyhow!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature_str,
                        receiver.to_string(universe)
                    )
                })
        }
    }
}

fn perform_in_super_class(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    receiver: SOMValue,
    signature: Interned,
    class: SOMRef<Class>,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "Object>>#perform:inSuperclass:";

    let method = class.borrow().lookup_method(signature);

    match method {
        Some(invokable) => Method::invoke(invokable, interpreter, heap, universe, receiver, vec![]),
        None => {
            let signature_str = universe.lookup_symbol(signature).to_owned();
            let args = vec![receiver];
            universe
                .does_not_understand(
                    interpreter,
                    heap,
                    SOMValue::new_class(&class),
                    signature,
                    args,
                )
                .with_context(|| {
                    anyhow!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature_str,
                        receiver.to_string(universe)
                    )
                })
        }
    }
}

fn perform_with_arguments_in_super_class(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    receiver: SOMValue,
    signature: Interned,
    arguments: SOMRef<Vec<SOMValue>>,
    class: SOMRef<Class>,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:inSuperclass:";

    let method = class.borrow().lookup_method(signature);

    match method {
        Some(invokable) => Method::invoke(
            invokable,
            interpreter,
            heap,
            universe,
            receiver,
            arguments.take(),
        ),
        None => {
            let signature_str = universe.lookup_symbol(signature).to_owned();
            let args = std::iter::once(receiver).chain(arguments.take()).collect();
            universe
                .does_not_understand(
                    interpreter,
                    heap,
                    SOMValue::new_class(&class),
                    signature,
                    args,
                )
                .with_context(|| {
                    anyhow!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature_str,
                        receiver.to_string(universe)
                    )
                })
        }
    }
}

fn inst_var_at(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: SOMValue,
    index: i32,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "Object>>#instVarAt:";

    let index = usize::try_from(index.saturating_sub(1))?;
    let local = receiver.lookup_local(index).unwrap_or(SOMValue::NIL);
    interpreter.stack.push(local);

    Ok(())
}

fn inst_var_at_put(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    mut receiver: SOMValue,
    index: i32,
    value: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "Object>>#instVarAt:put:";

    let index = usize::try_from(index.saturating_sub(1))?;
    let local = receiver
        .assign_local(index, value)
        .map_or(SOMValue::NIL, |_| value);
    interpreter.stack.push(local);

    Ok(())
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}
