use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::{Rc, Weak};

use som_core::ast::{ClassDef, MethodBody};

use crate::invokable::Invokable;
use crate::value::Value;
use crate::{SOMRef, SOMWeakRef};

/// A reference that may be either weak or owned/strong.
#[derive(Debug, Clone)]
pub enum MaybeWeak<A> {
    /// An owned reference.
    Owned(SOMRef<A>),
    /// A weak reference.
    Weak(SOMWeakRef<A>),
}

/// Represents a loaded class.
#[derive(Clone)]
pub struct Class {
    /// The class' name.
    pub name: String,
    /// The class of this class.
    pub class: MaybeWeak<Class>,
    /// The superclass of this class.
    pub super_class: SOMWeakRef<Class>,
    /// The class' locals.
    pub locals: HashMap<String, Value>,
    /// The class' methods/invokables.
    pub methods: HashMap<String, Invokable>,
}

impl Class {
    /// Load up a class from its class definition from the AST.
    pub fn from_class_def(defn: ClassDef) -> Self {
        let static_locals = defn
            .static_locals
            .iter()
            .cloned()
            .zip(std::iter::repeat(Value::Nil))
            .collect();

        let static_methods = defn
            .static_methods
            .iter()
            .map(|method| {
                let signature = method.signature.clone();
                let method = match method.body {
                    MethodBody::Primitive => Invokable::primitive_from_signature(
                        defn.name.as_str(),
                        method.signature.as_str(),
                    ),
                    MethodBody::Body { .. } => Invokable::MethodDef(method.clone()),
                };
                (signature, method)
            })
            .collect();

        let instance_locals = defn
            .instance_locals
            .iter()
            .cloned()
            .zip(std::iter::repeat(Value::Nil))
            .collect();

        let instance_methods = defn
            .instance_methods
            .iter()
            .map(|method| {
                let signature = method.signature.clone();
                let method = match method.body {
                    MethodBody::Primitive => Invokable::primitive_from_signature(
                        defn.name.as_str(),
                        method.signature.as_str(),
                    ),
                    MethodBody::Body { .. } => Invokable::MethodDef(method.clone()),
                };
                (signature, method)
            })
            .collect();

        let static_class = Self {
            name: format!("{} class", defn.name),
            class: MaybeWeak::Weak(Weak::new()),
            super_class: Weak::new(),
            locals: static_locals,
            methods: static_methods,
        };

        Self {
            name: defn.name,
            class: MaybeWeak::Owned(Rc::new(RefCell::new(static_class))),
            super_class: Weak::new(),
            locals: instance_locals,
            methods: instance_methods,
        }
    }

    /// Get the class' name.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// Get the class of this class.
    pub fn class(&self) -> SOMRef<Self> {
        match self.class {
            MaybeWeak::Weak(ref weak) => weak.upgrade().unwrap_or_else(|| {
                panic!("superclass dropped, cannot upgrade ref ({})", self.name())
            }),
            MaybeWeak::Owned(ref owned) => owned.clone(),
        }
    }

    /// Set the class of this class (as a weak reference).
    pub fn set_class(&mut self, class: &SOMRef<Self>) {
        self.class = MaybeWeak::Weak(Rc::downgrade(class));
    }

    /// Get the superclass of this class.
    pub fn super_class(&self) -> SOMRef<Self> {
        self.super_class
            .upgrade()
            .unwrap_or_else(|| panic!("superclass dropped, cannot upgrade ref ({})", self.name()))
    }

    /// Set the superclass of this class (as a weak reference).
    pub fn set_super_class(&mut self, class: &SOMRef<Self>) {
        self.super_class = Rc::downgrade(class);
    }

    /// Search for a given method within this class.
    pub fn lookup_method(&self, signature: impl AsRef<str>) -> Option<Invokable> {
        // dbg!(self.signature.as_str());
        // let signature = dbg!(signature.as_ref());
        let signature = signature.as_ref();
        self.methods.get(signature).cloned().or_else(|| {
            self.super_class
                .upgrade()?
                .borrow()
                .lookup_method(signature)
        })
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

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Class")
            .field("name", &self.name)
            .field("locals", &self.locals)
            .field("class", &self.class)
            .field("super_class", &self.super_class)
            .finish()
    }
}
