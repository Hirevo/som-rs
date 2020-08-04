use crate::frame::FrameKind;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, reverse};

/// Primitives for the **Block** and **Block1** class.
pub mod block1 {
    use super::*;

    fn value(interpreter: &mut Interpreter, _: &mut Universe) {
        const SIGNATURE: &str = "Block1>>#value";

        let frame = interpreter.current_frame().expect("no current frame");

        expect_args!(SIGNATURE, frame, [
            Value::Block(block) => block,
        ]);

        let kind = FrameKind::Block { block: block.clone() };

        interpreter.push_frame(kind);
    }

    fn restart(interpreter: &mut Interpreter, _: &mut Universe) {
        const SIGNATURE: &str = "Block>>#restart";

        let frame = interpreter.current_frame().expect("no current frame");

        expect_args!(SIGNATURE, frame, [Value::Block(_)]);

        frame.borrow_mut().bytecode_idx = 0;
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

    fn value(interpreter: &mut Interpreter, _: &mut Universe) {
        const SIGNATURE: &str = "Block2>>#value:";

        let frame = interpreter.current_frame().expect("no current frame");

        expect_args!(SIGNATURE, frame, [
            Value::Block(block) => block,
            argument => argument,
        ]);

        let kind = FrameKind::Block { block: block.clone() };

        let frame = interpreter.push_frame(kind);
        frame.borrow_mut().args.push(argument);
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

    fn value_with(interpreter: &mut Interpreter, _: &mut Universe) {
        const SIGNATURE: &str = "Block3>>#value:with:";

        let frame = interpreter.current_frame().expect("no current frame");

        expect_args!(SIGNATURE, frame, [
            Value::Block(block) => block,
            argument1 => argument1,
            argument2 => argument2,
        ]);

        let kind = FrameKind::Block { block: block.clone() };

        let frame = interpreter.push_frame(kind);
        frame.borrow_mut().args.push(argument1);
        frame.borrow_mut().args.push(argument2);
    }

    /// Search for a primitive matching the given signature.
    pub fn get_primitive(signature: impl AsRef<str>) -> Option<PrimitiveFn> {
        match signature.as_ref() {
            "value:with:" => Some(self::value_with),
            _ => None,
        }
    }
}
