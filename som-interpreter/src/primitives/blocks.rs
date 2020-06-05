use crate::expect_args;
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

        let block_args = args.clone();
        expect_args!(SIGNATURE, args, [
            Value::Block(block) => block,
        ]);

        block.invoke(universe, block_args)
    }

    fn restart(_: &mut Universe, args: Vec<Value>) -> Return {
        const SIGNATURE: &str = "Block>>#restart";

        expect_args!(SIGNATURE, args, [Value::Block(_)]);

        Return::Restart
    }

    /// Search for a primitive matching the given signature.
    pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
        match signature.as_ref() {
            "value" => Some(self::value),
            "restart" => Some(self::restart),
            _ => None,
        }
    }
}

/// Primitives for the **Block2** class.
pub mod block2 {
    use super::*;

    fn value(universe: &mut Universe, args: Vec<Value>) -> Return {
        const SIGNATURE: &str = "Block2>>#value:";

        let block_args = args.clone();
        expect_args!(SIGNATURE, args, [
            Value::Block(block) => block,
            _,
        ]);

        block.invoke(universe, block_args)
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

        let block_args = args.clone();
        expect_args!(SIGNATURE, args, [
            Value::Block(block) => block,
            _,
            _,
        ]);

        block.invoke(universe, block_args)
    }

    /// Search for a primitive matching the given signature.
    pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
        match signature.as_ref() {
            "value:with:" => Some(self::value_with),
            _ => None,
        }
    }
}
