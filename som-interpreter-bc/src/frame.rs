use std::rc::Rc;

use som_core::bytecode::Bytecode;

use crate::block::Block;
use crate::class::Class;
use crate::compiler::Literal;
use crate::method::{Method, MethodKind};
use crate::value::Value;
use crate::SOMRef;

/// The kind of a given frame.
#[derive(Clone)]
pub enum FrameKind {
    /// A frame created from a block evaluation.
    Block {
        /// The block instance for the current frame.
        block: Rc<Block>,
    },
    /// A frame created from a method invocation.
    Method {
        /// The holder of the current method (used for lexical self/super).
        holder: SOMRef<Class>,
        /// The current method.
        method: Rc<Method>,
        /// The self value.
        self_value: Value,
    },
}

/// Represents a stack frame.
pub struct Frame {
    /// This frame's kind.
    pub kind: FrameKind,
    /// The arguments within this frame.
    pub args: Vec<Value>,
    /// The bindings within this frame.
    pub locals: Vec<Value>,
    /// Execution stack.
    pub stack: Vec<Value>,
    /// Bytecode index.
    pub bytecode_idx: usize,
}

impl Frame {
    /// Construct a new empty frame from its kind.
    pub fn from_kind(kind: FrameKind) -> Self {
        match &kind {
            FrameKind::Block { block } => {
                let locals = block.blk_info.locals.iter().map(|_| Value::Nil).collect();
                Self {
                    kind,
                    locals,
                    args: vec![],
                    stack: vec![],
                    bytecode_idx: 0,
                }
            }
            FrameKind::Method { method, .. } => {
                if let MethodKind::Defined(env) = method.kind() {
                    let locals = env.locals.iter().map(|_| Value::Nil).collect();
                    Self {
                        kind,
                        locals,
                        args: vec![],
                        stack: vec![],
                        bytecode_idx: 0,
                    }
                } else {
                    Self {
                        kind,
                        locals: vec![],
                        args: vec![],
                        stack: vec![],
                        bytecode_idx: 0,
                    }
                }
            }
        }
    }

    /// Get the frame's kind.
    pub fn kind(&self) -> &FrameKind {
        &self.kind
    }

    /// Get the self value for this frame.
    pub fn get_self(&self) -> Value {
        match &self.kind {
            FrameKind::Method { self_value, .. } => self_value.clone(),
            FrameKind::Block { block, .. } => block.frame.as_ref().unwrap().borrow().get_self(),
        }
    }

    /// Get the holder for this current method.
    pub fn get_method_holder(&self) -> SOMRef<Class> {
        match &self.kind {
            FrameKind::Method { holder, .. } => holder.clone(),
            FrameKind::Block { block, .. } => {
                block.frame.as_ref().unwrap().borrow().get_method_holder()
            }
        }
    }

    /// Get the bytecode at the specified index for the current method.
    pub fn get_bytecode(&self, idx: usize) -> Option<Bytecode> {
        match &self.kind {
            FrameKind::Method { method, .. } => match method.kind() {
                MethodKind::Defined(env) => env.body.get(idx).copied(),
                MethodKind::Primitive(_) => None,
                MethodKind::NotImplemented(_) => None,
            },
            FrameKind::Block { block, .. } => block.blk_info.body.get(idx).copied(),
        }
    }

    /// Get the current bytecode for the current method.
    pub fn get_current_bytecode(&self) -> Option<Bytecode> {
        self.get_bytecode(self.bytecode_idx)
    }

    pub fn lookup_constant(&self, idx: usize) -> Option<Literal> {
        match self.kind() {
            FrameKind::Block { block } => block.blk_info.literals.get(idx).cloned(),
            FrameKind::Method { method, .. } => match method.kind() {
                MethodKind::Defined(env) => env.literals.get(idx).cloned(),
                MethodKind::Primitive(_) => None,
                MethodKind::NotImplemented(_) => None,
            },
        }
    }

    pub fn lookup_argument(&self, idx: usize) -> Option<Value> {
        self.args.get(idx).cloned()
    }

    /// Search for a local binding.
    pub fn lookup_local(&self, idx: usize) -> Option<Value> {
        self.locals.get(idx).cloned()
        // if let Some(value) = self.locals.get(idx).cloned() {
        //     return Some(value);
        // }
        // match &self.kind {
        //     FrameKind::Method {
        //         self_value, holder, ..
        //     } => {
        //         if holder.borrow().is_static {
        //             holder.borrow().lookup_local(idx)
        //         } else {
        //             self_value.lookup_local(idx)
        //         }
        //     }
        //     FrameKind::Block { block, .. } => {
        //         block.frame.as_ref().unwrap().borrow().lookup_local(idx)
        //     }
        // }
    }

    /// Assign to a local binding.
    pub fn assign_local(&mut self, idx: usize, value: Value) -> Option<()> {
        // if let Some(local) = self.locals.get_mut(idx) {
        //     *local = value;
        //     return Some(());
        // }
        // match &mut self.kind {
        //     FrameKind::Method {
        //         self_value, holder, ..
        //     } => {
        //         if holder.borrow().is_static {
        //             holder.borrow_mut().assign_local(idx, value)
        //         } else {
        //             self_value.assign_local(idx, value)
        //         }
        //     }
        //     FrameKind::Block { block, .. } => block
        //         .frame
        //         .as_ref()
        //         .unwrap()
        //         .borrow_mut()
        //         .assign_local(idx, value),
        // }
        self.locals.get_mut(idx).map(|local| *local = value)
    }

    /// Get the method invocation frame for that frame.
    pub fn method_frame(frame: &SOMRef<Frame>) -> SOMRef<Frame> {
        match frame.borrow().kind() {
            FrameKind::Block { block, .. } => Frame::method_frame(block.frame.as_ref().unwrap()),
            FrameKind::Method { .. } => frame.clone(),
        }
    }
}
