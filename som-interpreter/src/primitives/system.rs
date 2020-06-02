use std::convert::TryFrom;

use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

fn print_string(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#printString:";

    match args.into_iter().nth(1) {
        Some(Value::String(string)) => {
            print!("{}", string);
            Return::Local(Value::String(string))
        }
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn print_newline(_: &mut Universe, _: Vec<Value>) -> Return {
    const _: &'static str = "System>>#printNewline";

    println!();
    Return::Local(Value::Nil)
}

fn load(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#load:";

    match args[1] {
        Value::Symbol(sym) => {
            let name = universe.lookup_symbol(sym).to_string();
            match universe.load_class(name.as_str()) {
                Ok(class) => Return::Local(Value::Class(class)),
                Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
            }
        }
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn global(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#global:";

    dbg!(universe.current_method_frame());
    match &dbg!(&args)[1] {
        Value::Symbol(sym) => {
            let symbol = universe.lookup_symbol(*sym);
            Return::Local(universe.lookup_global(symbol).unwrap_or(Value::Nil))
        }
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn global_put(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#global:put:";

    match args[1] {
        Value::Symbol(sym) => {
            let symbol = universe.lookup_symbol(sym).to_string();
            universe.assign_local(symbol, args[2].clone());
            Return::Local(args[2].clone())
        }
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

fn exit(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "System>>#exit:";

    match args[1] {
        Value::Integer(code) => match i32::try_from(code) {
            Ok(code) => std::process::exit(code),
            Err(err) => Return::Exception(format!("'{}': {}", SIGNATURE, err)),
        },
        _ => Return::Exception(format!("'{}': wrong type", SIGNATURE)),
    }
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "printString:" => Some(self::print_string),
        "printNewline" => Some(self::print_newline),
        "load:" => Some(self::load),
        "exit:" => Some(self::exit),
        "global:" => Some(self::global),
        "global:put:" => Some(self::global_put),
        _ => None,
    }
}
