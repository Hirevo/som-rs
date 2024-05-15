use std::cell::RefCell;
use std::fmt;

use som_gc::{GcHeap, Trace};

use crate::class::Class;
use crate::value::Value;
use crate::SOMRef;

/// Represents a generic (non-primitive) class instance.
#[derive(Clone)]
#[repr(C)]
pub struct Instance {
    /// The class of which this is an instance from.
    pub class: SOMRef<Class>,
    /// The number of locals this instance has.
    pub nb_locals: usize,
}

impl Trace for Instance {
    #[inline]
    fn trace(&self) {
        self.class.trace();
        for field in self.fields_iter() {
            field.trace();
        }
    }
}

impl Instance {
    /// Construct an instance for a given class.
    pub fn from_class(heap: &mut GcHeap, class: SOMRef<Class>) -> SOMRef<Self> {
        let mut locals = Vec::new();

        fn collect_locals(class: &SOMRef<Class>, locals: &mut Vec<Value>) {
            if let Some(class) = class.borrow().super_class() {
                collect_locals(&class, locals);
            }
            locals.extend(class.borrow().locals.iter().map(|_| Value::Nil));
        }

        collect_locals(&class, &mut locals);

        let nb_locals = locals.len();
        let instance = heap.allocate_with_additional_size(
            RefCell::new(Self { class, nb_locals }),
            std::mem::size_of::<Value>() * nb_locals,
        );

        let ptr = unsafe { RefCell::as_ptr(&*instance).add(1) }.cast::<Value>();
        for idx in 0..nb_locals {
            unsafe { ptr.add(idx).write(Value::Nil) };
        }

        instance
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
        (idx < self.nb_locals).then(|| unsafe {
            (&*std::ptr::from_ref(self).add(1).cast::<Value>().add(idx)).clone()
        })
    }

    /// Assign a value to a local binding.
    pub fn assign_local(&mut self, idx: usize, value: Value) -> Option<()> {
        (idx < self.nb_locals).then(|| unsafe {
            *(&mut *std::ptr::from_mut(self).add(1).cast::<Value>().add(idx)) = value;
        })
    }

    /// Returns an iterator over the instance's fields.
    pub fn fields_iter(&self) -> impl Iterator<Item = &Value> {
        let ptr = unsafe { std::ptr::from_ref(self).add(1) }.cast::<Value>();
        (0..self.nb_locals).map(move |idx| unsafe { &*ptr.add(idx) })
    }

    /// Returns a mutable iterator over the instance's fields.
    pub fn fields_iter_mut(&mut self) -> impl Iterator<Item = &mut Value> {
        let ptr = unsafe { std::ptr::from_mut(self).add(1) }.cast::<Value>();
        (0..self.nb_locals).map(move |idx| unsafe { &mut *ptr.add(idx) })
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
