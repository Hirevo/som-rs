use std::collections::hash_map::DefaultHasher;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};

use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, reverse};

fn class(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#class";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        object => object,
    ]);

    frame
        .borrow_mut()
        .stack
        .push(Value::Class(object.class(universe)));
}

fn object_size(interpreter: &mut Interpreter, _: &mut Universe) {
    const _: &'static str = "Object>>#objectSize";

    let frame = interpreter.current_frame().expect("no current frame");

    frame
        .borrow_mut()
        .stack
        .push(Value::Integer(std::mem::size_of::<Value>() as i64));
}

fn hashcode(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#hashcode";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        value => value,
    ]);

    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    let hash = (hasher.finish() as i64).abs();

    frame.borrow_mut().stack.push(Value::Integer(hash));
}

fn eq(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#==";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        a => a,
        b => b,
    ]);

    frame.borrow_mut().stack.push(Value::Boolean(a == b));
}

fn perform(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#perform:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        object => object,
        Value::Symbol(sym) => sym,
    ]);

    let object: Value = object;

    let signature = universe.lookup_symbol(sym);
    let method = object.lookup_method(universe, sym);

    match method {
        Some(invokable) => invokable.invoke(interpreter, universe, object, vec![]),
        None => {
            let signature = signature.to_string();
            universe
                .does_not_understand(interpreter, object.clone(), sym, vec![object.clone()])
                .unwrap_or_else(|| {
                    panic!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe),
                    )
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_with_arguments(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        object => object,
        Value::Symbol(sym) => sym,
        Value::Array(arr) => arr,
    ]);

    let signature = universe.lookup_symbol(sym);
    let method = object.lookup_method(universe, sym);

    match method {
        Some(invokable) => {
            let args = arr.borrow().iter().cloned().collect();
            invokable.invoke(interpreter, universe, object, args)
        }
        None => {
            let signature = signature.to_string();
            let args = std::iter::once(object.clone())
                .chain(arr.borrow().iter().cloned())
                .collect();
            universe
                .does_not_understand(interpreter, object.clone(), sym, args)
                .unwrap_or_else(|| {
                    panic!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    )
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_in_super_class(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#perform:inSuperclass:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        object => object,
        Value::Symbol(sym) => sym,
        Value::Class(class) => class,
    ]);

    let signature = universe.lookup_symbol(sym);
    let method = class.borrow().lookup_method(sym);

    match method {
        Some(invokable) => invokable.invoke(interpreter, universe, object, vec![]),
        None => {
            let signature = signature.to_string();
            let args = vec![object.clone()];
            universe
                .does_not_understand(interpreter, Value::Class(class), sym, args)
                .unwrap_or_else(|| {
                    panic!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    )
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_with_arguments_in_super_class(interpreter: &mut Interpreter, universe: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:inSuperclass:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        object => object,
        Value::Symbol(sym) => sym,
        Value::Array(arr) => arr,
        Value::Class(class) => class,
    ]);

    let signature = universe.lookup_symbol(sym);
    let method = class.borrow().lookup_method(sym);

    match method {
        Some(invokable) => {
            let args = arr.borrow().iter().cloned().collect();
            invokable.invoke(interpreter, universe, object, args)
        }
        None => {
            let args = std::iter::once(object.clone())
                .chain(arr.replace(Vec::default()).into_iter())
                .collect();
            let signature = signature.to_string();
            universe
                .does_not_understand(interpreter, Value::Class(class), sym, args)
                .unwrap_or_else(|| {
                    panic!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    )
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn inst_var_at(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#instVarAt:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        object => object,
        Value::Integer(index) => index,
    ]);

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    };

    let local = object.lookup_local(index).unwrap_or(Value::Nil);

    frame.borrow_mut().stack.push(local);
}

fn inst_var_at_put(interpreter: &mut Interpreter, _: &mut Universe) {
    const SIGNATURE: &'static str = "Object>>#instVarAt:put:";

    let frame = interpreter.current_frame().expect("no current frame");

    expect_args!(SIGNATURE, frame, [
        object => object,
        Value::Integer(index) => index,
        value => value,
    ]);

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    };

    let local = object
        .assign_local(index, value.clone())
        .map(|_| value)
        .unwrap_or(Value::Nil);

    frame.borrow_mut().stack.push(local);
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
