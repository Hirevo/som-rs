use std::fmt;

use som_gc::Trace;

use crate::class::Class;
use crate::value::Value;
use crate::SOMRef;

/// Represents a generic (non-primitive) class instance.
#[derive(Clone)]
pub struct Instance {
    /// The class of which this is an instance from.
    pub class: SOMRef<Class>,
    /// This instance's locals.
    pub locals: Vec<Value>,
}

impl Trace for Instance {
    #[inline]
    fn trace(&self) {
        self.class.trace();
        self.locals.trace();
    }
}

impl Instance {
    /// Construct an instance for a given class.
    pub fn from_class(class: SOMRef<Class>) -> Self {
        let mut locals = Vec::new();

        fn collect_locals(class: &SOMRef<Class>, locals: &mut Vec<Value>) {
            if let Some(class) = class.borrow().super_class() {
                collect_locals(&class, locals);
            }
            locals.extend(class.borrow().locals.iter().map(|_| Value::NIL));
        }

        collect_locals(&class, &mut locals);

        // let locals = class.borrow().locals.iter().map(|_| Value::Nil).collect();

        Self { class, locals }
    }

    /// Get the class of which this is an instance from.
    pub fn class(&self) -> SOMRef<Class> {
        self.class.clone()
    }

    /// Get the superclass of this instance's class.
    pub fn super_class(&self) -> Option<SOMRef<Class>> {
        self.class.borrow().super_class()
    }

    /// Search for a local binding.
    pub fn lookup_local(&self, idx: usize) -> Option<Value> {
        self.locals.get(idx).cloned()
    }

    /// Assign a value to a local binding.
    pub fn assign_local(&mut self, idx: usize, value: Value) -> Option<()> {
        *self.locals.get_mut(idx)? = value;
        Some(())
    }
}

impl fmt::Debug for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Instance")
            .field("name", &self.class.borrow().name())
            // .field("locals", &self.locals.keys())
            .finish()
    }
}
