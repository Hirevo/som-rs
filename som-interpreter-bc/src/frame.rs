use std::cell::RefCell;

use som_core::bytecode::Bytecode;
use som_gc::{Gc, GcHeap, Trace};

use crate::block::Block;
use crate::class::Class;
use crate::compiler::Literal;
use crate::method::{Method, MethodKind};
use crate::value::Value;
use crate::SOMRef;

/// The kind of a given frame.
#[derive(Clone)]
#[repr(C)]
pub enum FrameKind {
    /// A frame created from a block evaluation.
    Block {
        /// The block instance for the current frame.
        block: Gc<Block>,
    },
    /// A frame created from a method invocation.
    Method {
        /// The holder of the current method (used for lexical self/super).
        holder: SOMRef<Class>,
        /// The current method.
        method: Gc<Method>,
        /// The self value.
        self_value: Value,
    },
}

/// Represents a stack frame.
#[repr(C)]
pub struct Frame {
    /// This frame's kind.
    pub kind: FrameKind,
    /// The arguments within this frame.
    pub args: Vec<Value>,
    /// Bytecode index.
    pub bytecode_idx: usize,
    /// The number of locals this frame has.
    pub nb_locals: usize,
}

impl Trace for FrameKind {
    #[inline]
    fn trace(&self) {
        match self {
            FrameKind::Block { block } => block.trace(),
            FrameKind::Method {
                holder,
                method,
                self_value,
            } => {
                holder.trace();
                method.trace();
                self_value.trace();
            }
        }
    }
}

impl Trace for Frame {
    #[inline]
    fn trace(&self) {
        self.kind.trace();
        self.args.trace();
        for local in self.locals_iter() {
            local.trace();
        }
    }
}

impl Frame {
    /// Construct a new empty frame from its kind.
    pub fn from_kind(heap: &mut GcHeap, kind: FrameKind) -> SOMRef<Self> {
        let value = match &kind {
            FrameKind::Block { block } => {
                let nb_locals = block.blk_info.locals.len();
                Self {
                    kind,
                    args: vec![],
                    bytecode_idx: 0,
                    nb_locals,
                }
            }
            FrameKind::Method { method, .. } => {
                let nb_locals = if let MethodKind::Defined(env) = &method.kind {
                    env.locals.len()
                } else {
                    0
                };
                Self {
                    kind,
                    args: vec![],
                    bytecode_idx: 0,
                    nb_locals,
                }
            }
        };

        let nb_locals = value.nb_locals;
        let frame = heap.allocate_with_additional_size(
            RefCell::new(value),
            std::mem::size_of::<Value>() * nb_locals,
        );

        let ptr = unsafe { RefCell::as_ptr(&*frame).add(1) }.cast::<Value>();
        for idx in 0..nb_locals {
            unsafe { ptr.add(idx).write(Value::Nil) };
        }

        frame
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

    /// Get the current method itself.
    pub fn get_method(&self) -> Gc<Method> {
        match &self.kind {
            FrameKind::Method { method, .. } => method.clone(),
            FrameKind::Block { block, .. } => block.frame.as_ref().unwrap().borrow().get_method(),
        }
    }

    /// Get the bytecode at the specified index for the current method.
    pub fn get_bytecode(&self, idx: usize) -> Option<Bytecode> {
        match &self.kind {
            FrameKind::Method { method, .. } => match &method.kind {
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
            FrameKind::Method { method, .. } => match &method.kind {
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
        (idx < self.nb_locals).then(|| unsafe {
            (&*std::ptr::from_ref(self).add(1).cast::<Value>().add(idx)).clone()
        })
    }

    /// Assign to a local binding.
    pub fn assign_local(&mut self, idx: usize, value: Value) -> Option<()> {
        (idx < self.nb_locals).then(|| unsafe {
            *(&mut *std::ptr::from_mut(self).add(1).cast::<Value>().add(idx)) = value;
        })
    }

    /// Get the method invocation frame for that frame.
    pub fn method_frame(frame: &SOMRef<Frame>) -> SOMRef<Frame> {
        match frame.borrow().kind() {
            FrameKind::Block { block, .. } => Frame::method_frame(block.frame.as_ref().unwrap()),
            FrameKind::Method { .. } => frame.clone(),
        }
    }

    /// Returns an iterator over the frame's locals.
    pub fn locals_iter(&self) -> impl Iterator<Item = &Value> {
        let ptr = unsafe { std::ptr::from_ref(self).add(1) }.cast::<Value>();
        (0..self.nb_locals).map(move |idx| unsafe { &*ptr.add(idx) })
    }

    /// Returns a mutable iterator over the frame's locals.
    pub fn locals_iter_mut(&mut self) -> impl Iterator<Item = &mut Value> {
        let ptr = unsafe { std::ptr::from_mut(self).add(1) }.cast::<Value>();
        (0..self.nb_locals).map(move |idx| unsafe { &mut *ptr.add(idx) })
    }
}
