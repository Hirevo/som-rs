use std::collections::hash_map::DefaultHasher;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};

use crate::class::Class;
use crate::interner::Interned;
use crate::invokable::{Invoke, Return};
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, SOMRef};

fn class(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#class";

    expect_args!(SIGNATURE, args, [
        object => object,
    ]);

    Return::Local(Value::Class(object.class(universe)))
}

fn object_size(_: &mut Universe, _: Vec<Value>) -> Return {
    const _: &'static str = "Object>>#objectSize";

    Return::Local(Value::Integer(std::mem::size_of::<Value>() as i64))
}

fn hashcode(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#hashcode";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    let hash = (hasher.finish() as i64).abs();

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
        Value::Symbol(signature) => signature,
    ]);

    let method = object.lookup_method(universe, signature);

    match method {
        Some(invokable) => invokable.invoke(universe, vec![object]),
        None => {
            universe
                .does_not_understand(object.clone(), signature, vec![object.clone()])
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        universe.lookup_symbol(signature),
                        object.as_display(universe)
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
        Value::Symbol(signature) => signature,
        Value::Array(arr) => arr,
    ]);

    let method = object.lookup_method(universe, signature);

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
            universe
                .does_not_understand(object.clone(), signature, args)
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        universe.lookup_symbol(signature),
                        object.as_display(universe)
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
        Value::Symbol(signature) => signature,
        Value::Class(class) => class,
    ]);

    let method = class.borrow().lookup_method(signature);

    match method {
        Some(invokable) => invokable.invoke(universe, vec![object]),
        None => {
            let args = vec![object.clone()];
            universe
                .does_not_understand(Value::Class(class), signature, args)
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        universe.lookup_symbol(signature),
                        object.as_display(universe)
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
        Value::Symbol(signature) => signature,
        Value::Array(arr) => arr,
        Value::Class(class) => class,
    ]);

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
            universe
                .does_not_understand(Value::Class(class), signature, args)
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        universe.lookup_symbol(signature),
                        object.as_display(universe)
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
        .into_iter()
        .nth(index)
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
        .into_iter()
        .nth(index)
        .and_then(|local| object.assign_local(local, value.clone()).map(|_| value))
        .unwrap_or(Value::Nil);

    Return::Local(local)
}

fn gather_locals(universe: &mut Universe, class: SOMRef<Class>) -> Vec<Interned> {
    let mut fields = match class.borrow().super_class() {
        Some(super_class) => gather_locals(universe, super_class),
        None => Vec::new(),
    };
    fields.extend(class.borrow().locals.keys().copied());
    fields
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "class" => Some(self::class),
        "objectSize" => Some(self::object_size),
        "hashcode" => Some(self::hashcode),
        "perform:" => Some(self::perform),
        "perform:withArguments:" => Some(self::perform_with_arguments),
        "perform:inSuperclass:" => Some(self::perform_in_super_class),
        "perform:withArguments:inSuperclass:" => Some(self::perform_with_arguments_in_super_class),
        "instVarAt:" => Some(self::inst_var_at),
        "instVarAt:put:" => Some(self::inst_var_at_put),
        "==" => Some(self::eq),
        _ => None,
    }
}
