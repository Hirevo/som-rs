use std::collections::hash_map::DefaultHasher;
use std::convert::{TryFrom, TryInto};
use std::hash::{Hash, Hasher};

use anyhow::{Context, Error};
use once_cell::sync::Lazy;

use som_gc::{Gc, GcHeap};

use crate::class::Class;
use crate::convert::Primitive;
use crate::interner::Interned;
use crate::interpreter::Interpreter;
use crate::method::Method;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
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
    _: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    receiver: Value,
) -> Result<SOMRef<Class>, Error> {
    const _: &'static str = "Object>>#class";

    Ok(Gc::clone(&receiver.class(universe)))
}

fn object_size(
    _: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: Value,
) -> Result<i32, Error> {
    const SIGNATURE: &'static str = "Object>>#objectSize";

    std::mem::size_of_val(&receiver)
        .try_into()
        .with_context(|| format!("`{SIGNATURE}`: could not convert `usize` to `i32`"))
}

fn hashcode(
    _: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: Value,
) -> Result<i32, Error> {
    const _: &'static str = "Object>>#hashcode";

    let mut hasher = DefaultHasher::new();
    receiver.hash(&mut hasher);
    let hash = (hasher.finish() as i32).abs();

    Ok(hash)
}

fn eq(
    _: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: Value,
    other: Value,
) -> Result<bool, Error> {
    const _: &'static str = "Object>>#==";

    Ok(receiver == other)
}

fn perform(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    receiver: Value,
    signature: Interned,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "Object>>#perform:";

    let Some(invokable) = receiver.lookup_method(universe, signature) else {
        let signature_str = universe.lookup_symbol(signature).to_owned();
        let args = vec![receiver];
        return universe
            .does_not_understand(interpreter, heap, receiver, signature, args)
            .with_context(|| {
                format!(
                    "`{SIGNATURE}`: method `{signature_str}` not found for `{}`",
                    receiver.to_string(universe),
                )
            });
    };

    Method::invoke(invokable, interpreter, heap, universe, receiver, vec![])
}

fn perform_with_arguments(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    receiver: Value,
    signature: Interned,
    arguments: SOMRef<Vec<Value>>,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:";

    let Some(invokable) = receiver.lookup_method(universe, signature) else {
        let signature_str = universe.lookup_symbol(signature).to_owned();
        let args = std::iter::once(receiver).chain(arguments.take()).collect();
        return universe
            .does_not_understand(interpreter, heap, receiver, signature, args)
            .with_context(|| {
                format!(
                    "`{SIGNATURE}`: method `{signature_str}` not found for `{}`",
                    receiver.to_string(universe)
                )
            });
    };

    Method::invoke(
        invokable,
        interpreter,
        heap,
        universe,
        receiver,
        arguments.take(),
    )
}

fn perform_in_super_class(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    receiver: Value,
    signature: Interned,
    class: SOMRef<Class>,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "Object>>#perform:inSuperclass:";

    let Some(invokable) = class.borrow().lookup_method(signature) else {
        let signature_str = universe.lookup_symbol(signature).to_owned();
        let args = vec![receiver];
        return universe
            .does_not_understand(interpreter, heap, Value::new_class(&class), signature, args)
            .with_context(|| {
                format!(
                    "`{SIGNATURE}`: method `{signature_str}` not found for `{}`",
                    receiver.to_string(universe)
                )
            });
    };

    Method::invoke(invokable, interpreter, heap, universe, receiver, vec![])
}

fn perform_with_arguments_in_super_class(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    receiver: Value,
    signature: Interned,
    arguments: SOMRef<Vec<Value>>,
    class: SOMRef<Class>,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:inSuperclass:";

    let method = class.borrow().lookup_method(signature);

    let Some(invokable) = method else {
        let signature_str = universe.lookup_symbol(signature).to_owned();
        let args = std::iter::once(receiver).chain(arguments.take()).collect();
        return universe
            .does_not_understand(interpreter, heap, Value::new_class(&class), signature, args)
            .with_context(|| {
                format!(
                    "`{SIGNATURE}`: method `{signature_str}` not found for `{}`",
                    receiver.to_string(universe)
                )
            });
    };

    Method::invoke(
        invokable,
        interpreter,
        heap,
        universe,
        receiver,
        arguments.take(),
    )
}

fn inst_var_at(
    _: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    receiver: Value,
    index: i32,
) -> Result<Option<Value>, Error> {
    const _: &'static str = "Object>>#instVarAt:";

    let index = usize::try_from(index.saturating_sub(1))?;

    Ok(receiver.lookup_local(index))
}

fn inst_var_at_put(
    _: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    mut receiver: Value,
    index: i32,
    value: Value,
) -> Result<Option<Value>, Error> {
    const _: &'static str = "Object>>#instVarAt:put:";

    let index = usize::try_from(index.saturating_sub(1))?;

    Ok(receiver.assign_local(index, value).map(|_| value))
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
