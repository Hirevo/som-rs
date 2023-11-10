use som_gc::GcHeap;

use crate::frame::FrameKind;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{expect_args, reverse};

/// Primitives for the **Block** and **Block1** class.
pub mod block1 {
    use super::*;

    pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
        ("value", self::value, true),
        ("restart", self::restart, false),
    ];
    pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

    fn value(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
        const SIGNATURE: &str = "Block1>>#value";

        expect_args!(SIGNATURE, interpreter, [
            Value::Block(block) => block,
        ]);

        let kind = FrameKind::Block {
            block: block.clone(),
        };

        interpreter.push_frame(heap, kind);
    }

    fn restart(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
        const SIGNATURE: &str = "Block>>#restart";

        expect_args!(SIGNATURE, interpreter, [Value::Block(_)]);

        let frame = interpreter.current_frame().expect("no current frame");
        frame.borrow_mut().bytecode_idx = 0;
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
}

/// Primitives for the **Block2** class.
pub mod block2 {
    use super::*;

    pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[("value:", self::value, true)];
    pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

    fn value(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
        const SIGNATURE: &str = "Block2>>#value:";

        expect_args!(SIGNATURE, interpreter, [
            Value::Block(block) => block,
            argument => argument,
        ]);

        let kind = FrameKind::Block {
            block: block.clone(),
        };

        let frame = interpreter.push_frame(heap, kind);
        frame.borrow_mut().args.push(argument.into());
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
}

/// Primitives for the **Block3** class.
pub mod block3 {
    use super::*;

    pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] =
        &[("value:with:", self::value_with, true)];
    pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

    fn value_with(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
        const SIGNATURE: &str = "Block3>>#value:with:";

        expect_args!(SIGNATURE, interpreter, [
            Value::Block(block) => block,
            argument1 => argument1,
            argument2 => argument2,
        ]);

        let kind = FrameKind::Block {
            block: block.clone(),
        };

        let frame = interpreter.push_frame(heap, kind);
        frame.borrow_mut().args.push(argument1.into());
        frame.borrow_mut().args.push(argument2.into());
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
}
