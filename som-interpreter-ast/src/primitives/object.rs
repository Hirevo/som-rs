use std::collections::hash_map::DefaultHasher;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};

use crate::class::Class;
use crate::invokable::{Invoke, Return};
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, SOMRef};

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

fn class(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#class";

    expect_args!(SIGNATURE, args, [
        object => object,
    ]);

    Return::Local(Value::Class(object.class(universe)))
}

fn object_size(_: &mut Universe, _: Vec<Value>) -> Return {
    const _: &'static str = "Object>>#objectSize";

    Return::Local(Value::Integer(std::mem::size_of::<Value>() as i32))
}

fn hashcode(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#hashcode";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    let hash = (hasher.finish() as i32).abs();

    Return::Local(Value::Integer(hash))
}

fn eq(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#==";

    expect_args!(SIGNATURE, args, [
        a => a,
        b => b,
    ]);

    Return::Local(Value::Boolean(a == b))
}

fn perform(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#perform:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Symbol(sym) => sym,
    ]);

    let signature = universe.lookup_symbol(sym);
    let method = object.lookup_method(universe, signature);

    match method {
        Some(invokable) => invokable.invoke(universe, vec![object]),
        None => {
            let signature = signature.to_string();
            universe
                .does_not_understand(object.clone(), signature.as_str(), vec![object.clone()])
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    ))
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_with_arguments(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Symbol(sym) => sym,
        Value::Array(arr) => arr,
    ]);

    let signature = universe.lookup_symbol(sym);
    let method = object.lookup_method(universe, signature);

    match method {
        Some(invokable) => {
            let args = std::iter::once(object)
                .chain(arr.replace(Vec::default()).into_iter())
                .collect();
            invokable.invoke(universe, args)
        }
        None => {
            let signature = signature.to_string();
            let args = std::iter::once(object.clone())
                .chain(arr.replace(Vec::default()).into_iter())
                .collect();
            universe
                .does_not_understand(object.clone(), signature.as_str(), args)
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    ))
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_in_super_class(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#perform:inSuperclass:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Symbol(sym) => sym,
        Value::Class(class) => class,
    ]);

    let signature = universe.lookup_symbol(sym);
    let method = class.borrow().lookup_method(signature);

    match method {
        Some(invokable) => invokable.invoke(universe, vec![object]),
        None => {
            let signature = signature.to_string();
            let args = vec![object.clone()];
            universe
                .does_not_understand(Value::Class(class), signature.as_str(), args)
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    ))
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_with_arguments_in_super_class(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:inSuperclass:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Symbol(sym) => sym,
        Value::Array(arr) => arr,
        Value::Class(class) => class,
    ]);

    let signature = universe.lookup_symbol(sym);
    let method = class.borrow().lookup_method(signature);

    match method {
        Some(invokable) => {
            let args = std::iter::once(object)
                .chain(arr.replace(Vec::default()).into_iter())
                .collect();
            invokable.invoke(universe, args)
        }
        None => {
            let args = std::iter::once(object.clone())
                .chain(arr.replace(Vec::default()).into_iter())
                .collect();
            let signature = signature.to_string();
            universe
                .does_not_understand(Value::Class(class), signature.as_str(), args)
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    ))
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn inst_var_at(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#instVarAt:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Integer(index) => index,
    ]);

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => return Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    };

    let locals = gather_locals(universe, object.class(universe));
    let local = locals
        .get(index)
        .and_then(|local| object.lookup_local(local))
        .unwrap_or(Value::Nil);

    Return::Local(local)
}

fn inst_var_at_put(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#instVarAt:put:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Integer(index) => index,
        value => value,
    ]);

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => return Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    };

    let locals = gather_locals(universe, object.class(universe));
    let local = locals
        .get(index)
        .and_then(|local| object.assign_local(local, value.clone()).map(|_| value))
        .unwrap_or(Value::Nil);

    Return::Local(local)
}

fn gather_locals(universe: &mut Universe, class: SOMRef<Class>) -> Vec<String> {
    let mut fields = match class.borrow().super_class() {
        Some(super_class) => gather_locals(universe, super_class),
        None => Vec::new(),
    };
    fields.extend(class.borrow().locals.keys().cloned());
    fields
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
