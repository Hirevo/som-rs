use anyhow::{Context, Error};
use once_cell::sync::Lazy;

use som_gc::{Gc, GcHeap};

use crate::block::Block;
use crate::convert::Primitive;
use crate::frame::FrameKind;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::SOMValue;

/// Primitives for the **Block** and **Block1** class.
pub mod block1 {
    use super::*;

    pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
        Lazy::new(|| {
            Box::new([
                ("value", self::value.into_func(), true),
                ("restart", self::restart.into_func(), false),
            ])
        });
    pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
        Lazy::new(|| Box::new([]));

    fn value(
        interpreter: &mut Interpreter,
        heap: &mut GcHeap,
        _: &mut Universe,
        receiver: Gc<Block>,
    ) -> Result<(), Error> {
        const _: &str = "Block1>>#value";

        let kind = FrameKind::Block {
            block: Gc::clone(&receiver),
        };
        interpreter.push_frame(heap, kind);

        Ok(())
    }

    fn restart(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
        _: Gc<Block>,
    ) -> Result<(), Error> {
        const _: &str = "Block>>#restart";

        interpreter
            .current_frame()
            .context("`Block>>#restart` with missing frame")?
            .borrow_mut()
            .bytecode_idx = 0;

        Ok(())
    }

    /// Search for an instance primitive matching the given signature.
    pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        INSTANCE_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }

    /// Search for a class primitive matching the given signature.
    pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        CLASS_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }
}

/// Primitives for the **Block2** class.
pub mod block2 {
    use super::*;

    pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
        Lazy::new(|| Box::new([("value:", self::value.into_func(), true)]));
    pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
        Lazy::new(|| Box::new([]));

    fn value(
        interpreter: &mut Interpreter,
        heap: &mut GcHeap,
        _: &mut Universe,
        receiver: Gc<Block>,
        argument: SOMValue,
    ) -> Result<(), Error> {
        const _: &str = "Block2>>#value:";

        let kind = FrameKind::Block {
            block: Gc::clone(&receiver),
        };
        let frame = interpreter.push_frame(heap, kind);
        frame.borrow_mut().args.push(argument);

        Ok(())
    }

    /// Search for an instance primitive matching the given signature.
    pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        INSTANCE_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }

    /// Search for a class primitive matching the given signature.
    pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        CLASS_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }
}

/// Primitives for the **Block3** class.
pub mod block3 {
    use super::*;

    pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
        Lazy::new(|| Box::new([("value:with:", self::value_with.into_func(), true)]));
    pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
        Lazy::new(|| Box::new([]));

    fn value_with(
        interpreter: &mut Interpreter,
        heap: &mut GcHeap,
        _: &mut Universe,
        receiver: Gc<Block>,
        argument1: SOMValue,
        argument2: SOMValue,
    ) -> Result<(), Error> {
        const _: &str = "Block3>>#value:with:";

        let kind = FrameKind::Block {
            block: Gc::clone(&receiver),
        };

        let frame = interpreter.push_frame(heap, kind);
        frame.borrow_mut().args.push(argument1);
        frame.borrow_mut().args.push(argument2);

        Ok(())
    }

    /// Search for an instance primitive matching the given signature.
    pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        INSTANCE_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }

    /// Search for a class primitive matching the given signature.
    pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        CLASS_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }
}
