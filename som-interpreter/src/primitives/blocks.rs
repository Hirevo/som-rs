use crate::invokable::Invoke;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

/// Primitives for the **Block** and **Block1** class.
pub mod block1 {
    use super::*;

    fn value(universe: &mut Universe, args: Vec<Value>) -> Return {
        const SIGNATURE: &str = "Block1>>#value";

        match args[0] {
            Value::Block(ref block) => block.invoke(universe, args.clone()),
            _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
        }
    }

    fn while_true(universe: &mut Universe, args: Vec<Value>) -> Return {
        const SIGNATURE: &str = "Block>>#whileTrue:";

        match (&args[0], &args[1]) {
            (Value::Block(cond), Value::Block(action)) => loop {
                let value = cond.invoke(universe, vec![args[0].clone()]);
                if let Return::Local(Value::Boolean(true)) = value {
                    action.invoke(universe, vec![args[1].clone()]);
                } else {
                    break value;
                }
            },
            _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
        }
    }

    /// Search for a primitive matching the given signature.
    pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
        match signature.as_ref() {
            "value" => Some(self::value),
            "whileTrue:" => Some(self::while_true),
            // "restart" => Some(self::value),
            _ => None,
        }
    }
}

/// Primitives for the **Block2** class.
pub mod block2 {
    use super::*;

    fn value(universe: &mut Universe, args: Vec<Value>) -> Return {
        const SIGNATURE: &str = "Block2>>#value:";

        match args[0] {
            Value::Block(ref block) => block.invoke(universe, args.clone()),
            _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
        }
    }

    /// Search for a primitive matching the given signature.
    pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
        match signature.as_ref() {
            "value:" => Some(self::value),
            _ => None,
        }
    }
}

/// Primitives for the **Block3** class.
pub mod block3 {
    use super::*;

    fn value_with(universe: &mut Universe, args: Vec<Value>) -> Return {
        const SIGNATURE: &str = "Block3>>#value:with:";

        match args[0] {
            Value::Block(ref block) => block.invoke(universe, args.clone()),
            _ => Return::Exception(format!("'{}': invalid self type", SIGNATURE)),
        }
    }

    /// Search for a primitive matching the given signature.
    pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
        match signature.as_ref() {
            "value:with:" => Some(self::value_with),
            _ => None,
        }
    }
}
