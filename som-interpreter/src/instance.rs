use std::collections::HashMap;

use crate::class::Class;
use crate::value::Value;
use crate::SOMRef;

/// Represents a generic (non-primitive) class instance.
#[derive(Debug, Clone)]
pub struct Instance {
    /// The class of which this is an instance from.
    pub class: SOMRef<Class>,
    /// This instance's locals.
    pub locals: HashMap<String, Value>,
}

impl Instance {
    /// Construct an instance for a given class.
    pub fn from_class(class: SOMRef<Class>) -> Self {
        let locals = class
            .borrow()
            .locals
            .keys()
            .cloned()
            .zip(std::iter::repeat(Value::Nil))
            .collect();

        Self { class, locals }
    }

    /// Get the class of which this is an instance from.
    pub fn class(&self) -> SOMRef<Class> {
        self.class.clone()
    }

    /// Get the superclass of this instance's class.
    pub fn super_class(&self) -> SOMRef<Class> {
        self.class.borrow().super_class()
    }

    /// Search for a local binding.
    pub fn lookup_local(&self, name: impl AsRef<str>) -> Option<Value> {
        self.locals.get(name.as_ref()).cloned()
    }

    /// Assign a value to a local binding.
    pub fn assign_local(&mut self, name: impl AsRef<str>, value: Value) -> Option<()> {
        *self.locals.get_mut(name.as_ref())? = value;
        Some(())
    }
}
