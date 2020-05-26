use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::{Rc, Weak};

use som_core::ast::{ClassDef, MethodBody, MethodDef};

use crate::invokable::Invokable;
use crate::value::Value;
use crate::SOMRef;

pub struct Class {
    locals: HashMap<String, SOMRef<Value>>,
    super_class: Weak<RefCell<Class>>,
    methods: HashMap<String, Invokable>,
    defn: ClassDef,
}

impl Class {
    pub fn from_class_def(defn: ClassDef) -> Class {
        let locals = defn
            .static_locals
            .iter()
            .cloned()
            .map(|local| (local, Rc::new(RefCell::new(Value::Nil))))
            .collect();
        let methods = defn
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

        Class {
            locals,
            defn,
            methods,
            super_class: Weak::new(),
        }
    }

    pub fn name(&self) -> &str {
        self.defn.name.as_str()
    }

    pub fn defn(&self) -> &ClassDef {
        &self.defn
    }

    pub fn super_class(&self) -> SOMRef<Class> {
        self.super_class
            .upgrade()
            .expect("superclass dropped, cannot upgrade ref")
    }

    pub fn set_super_class(&mut self, super_class: &SOMRef<Class>) {
        self.super_class = Rc::downgrade(super_class);
    }
}

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Class")
            .field("locals", &self.locals)
            .field("super_class", &self.super_class)
            .finish()
    }
}
