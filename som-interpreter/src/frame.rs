use std::collections::HashMap;

use crate::value::Value;
use crate::SOMWeakRef;

/// The kind of a given frame.
#[derive(Debug, Clone)]
pub enum FrameKind {
    /// A frame created from a block evaluation.
    Block(
        /// Weak reference to its parent frame.
        SOMWeakRef<Frame>,
    ),
    /// A frame created from a method invocation.
    Method(
        /// The self value.
        Value,
    ),
}

/// Represents a stack frame.
#[derive(Debug)]
pub struct Frame {
    /// This frame's kind.
    pub kind: FrameKind,
    /// The bindings within this frame.
    pub bindings: HashMap<String, Value>,
}

impl Frame {
    /// Construct a new empty frame from its kind.
    pub fn from_kind(kind: FrameKind) -> Self {
        Self {
            kind,
            bindings: HashMap::new(),
        }
    }

    /// Get the frame's kind.
    pub fn kind(&self) -> &FrameKind {
        &self.kind
    }

    /// Get the self value for this frame.
    pub fn get_self(&self) -> Value {
        match &self.kind {
            FrameKind::Method(value) => value.clone(),
            FrameKind::Block(frame_ref) => frame_ref
                .upgrade()
                .map(|frame| frame.borrow().get_self())
                .unwrap_or(Value::Nil),
        }
    }

    /// Search for a local binding.
    pub fn lookup_local(&self, name: impl AsRef<str>) -> Option<Value> {
        let name = name.as_ref();
        if let Some(value) = self.bindings.get(name).cloned() {
            return Some(value);
        }
        match &self.kind {
            FrameKind::Method(value) => value.lookup_local(name),
            FrameKind::Block(frame_ref) => frame_ref
                .upgrade()
                .and_then(|frame| frame.borrow().lookup_local(name)),
        }
    }

    /// Assign to a local binding.
    pub fn assign_local(&mut self, name: impl AsRef<str>, value: Value) -> Option<()> {
        let name = name.as_ref();
        if let Some(local) = self.bindings.get_mut(name) {
            *local = value;
            return Some(());
        }
        match &mut self.kind {
            FrameKind::Method(self_value) => self_value.assign_local(name, value),
            FrameKind::Block(frame_ref) => frame_ref
                .upgrade()
                .and_then(|frame| frame.borrow_mut().assign_local(name, value)),
        }
    }
}
