use std::fmt;

use indexmap::IndexMap;

use som_gc::{Gc, Trace};

use crate::interner::Interned;
use crate::method::Method;
use crate::value::SOMValue;
use crate::SOMRef;

/// Represents a loaded class.
#[derive(Clone)]
pub struct Class {
    /// The class' name.
    pub name: String,
    /// The class of this class.
    pub class: Option<SOMRef<Class>>,
    /// The superclass of this class.
    pub super_class: Option<SOMRef<Class>>,
    /// The class' locals.
    pub locals: IndexMap<Interned, SOMValue>,
    /// The class' methods/invokables.
    pub methods: IndexMap<Interned, Gc<Method>>,
    /// Is this class a static one ?
    pub is_static: bool,
}

impl Trace for Class {
    #[inline]
    fn trace(&self) {
        self.class.trace();
        self.super_class.trace();
        for it in self.locals.values() {
            it.trace();
        }
        for it in self.methods.values() {
            it.trace();
        }
    }
}

impl Class {
    /// Get the class' name.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// Get the class of this class.
    pub fn class(&self) -> SOMRef<Self> {
        self.class.clone().unwrap()
    }

    /// Set the class of this class.
    pub fn set_class(&mut self, class: SOMRef<Self>) {
        self.class = Some(class);
    }

    /// Get the superclass of this class.
    pub fn super_class(&self) -> Option<SOMRef<Self>> {
        self.super_class.clone()
    }

    /// Set the superclass of this class.
    pub fn set_super_class(&mut self, class: SOMRef<Self>) {
        self.super_class = Some(class);
    }

    /// Search for a given method within this class.
    pub fn lookup_method(&self, signature: Interned) -> Option<Gc<Method>> {
        if let Some(method) = self.methods.get(&signature).cloned() {
            return Some(method);
        }

        self.super_class.as_ref()?.borrow().lookup_method(signature)
    }

    /// Search for a local binding.
    pub fn lookup_local(&self, idx: usize) -> Option<SOMValue> {
        if let Some(local) = self.locals.values().nth(idx).cloned() {
            return Some(local);
        }

        self.super_class.as_ref()?.borrow_mut().lookup_local(idx)
    }

    /// Assign a value to a local binding.
    pub fn assign_local(&mut self, idx: usize, value: SOMValue) -> Option<()> {
        if let Some(local) = self.locals.values_mut().nth(idx) {
            *local = value;
            return Some(());
        }

        self.super_class
            .as_ref()?
            .borrow_mut()
            .assign_local(idx, value)
    }
}

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Class")
            .field("name", &self.name)
            // .field("locals", &self.locals.keys())
            // .field("class", &self.class)
            // .field("super_class", &self.super_class)
            .finish()
    }
}
