use crate::expect_args;
use crate::invokable::{Invoke, Return};
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

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

    let exception = Return::Exception(format!(
        "'{}': method '{}' not found for '{}'",
        SIGNATURE,
        signature,
        object.to_string(universe)
    ));

    method
        .map(|invokable| invokable.invoke(universe, vec![object]))
        .unwrap_or(exception)
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

    let exception = Return::Exception(format!(
        "'{}': method '{}' not found for '{}'",
        SIGNATURE,
        signature,
        object.to_string(universe)
    ));

    let args = std::iter::once(object)
        .chain(arr.replace(Vec::default()).into_iter())
        .collect();

    method
        .map(|invokable| invokable.invoke(universe, args))
        .unwrap_or(exception)
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

    let exception = Return::Exception(format!(
        "'{}': method '{}' not found for '{}'",
        SIGNATURE,
        signature,
        object.to_string(universe)
    ));

    method
        .map(|invokable| invokable.invoke(universe, vec![object.clone()]))
        .unwrap_or(exception)
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

    let exception = Return::Exception(format!(
        "'{}': method '{}' not found for '{}'",
        SIGNATURE,
        signature,
        object.to_string(universe)
    ));

    let args = std::iter::once(object)
        .chain(arr.replace(Vec::default()).into_iter())
        .collect();

    method
        .map(|invokable| invokable.invoke(universe, args))
        .unwrap_or(exception)
}

fn inst_var_at(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#instVarAt:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Symbol(sym) => sym,
    ]);

    let signature = universe.lookup_symbol(sym);
    let local = object.lookup_local(signature);

    Return::Local(local.unwrap_or(Value::Nil))
}

fn inst_var_at_put(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#instVarAt:put:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Symbol(sym) => sym,
        value => value,
    ]);

    let signature = universe.lookup_symbol(sym);
    let outcome = object.assign_local(signature, value.clone());

    Return::Local(outcome.map(|_| value).unwrap_or(Value::Nil))
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "class" => Some(self::class),
        "objectSize" => Some(self::object_size),
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
