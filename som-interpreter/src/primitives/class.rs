use std::cell::RefCell;
use std::rc::Rc;

use crate::instance::Instance;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

fn superclass(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Class>>#superclass";

    match args[0] {
        Value::Class(ref class) => Return::Local(Value::Class(class.borrow().super_class())),
        _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    }
}

fn new(_: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Class>>#new";

    match args[0] {
        Value::Class(ref class) => Return::Local(Value::Instance(Rc::new(RefCell::new(
            Instance::from_class(class.clone()),
        )))),
        _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    }
}

fn name(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Class>>#name";

    match args[0] {
        Value::Class(ref class) => {
            Return::Local(Value::Symbol(universe.intern_symbol(class.borrow().name())))
        }
        _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    }
}

fn fields(universe: &mut Universe, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Class>>#fields";

    match args[0] {
        Value::Class(ref class) => Return::Local(Value::Array(Rc::new(RefCell::new(
            class
                .borrow()
                .locals
                .keys()
                .map(|field| Value::Symbol(universe.intern_symbol(field)))
                .collect(),
        )))),
        _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
    }
}

/// Search for a primitive matching the given signature.
pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
    match signature.as_ref() {
        "new" => Some(self::new),
        "name" => Some(self::name),
        "fields" => Some(self::fields),
        // "methods" => Some(self::methods),
        "superclass" => Some(self::superclass),
        _ => None,
    }
}
