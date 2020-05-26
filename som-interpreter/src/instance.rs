use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use crate::class::Class;
use crate::value::Value;
use crate::SOMRef;

#[derive(Debug, Clone)]
pub struct Instance {
    /// Class object.
    pub class: SOMRef<Class>,
    /// Instance locals.
    pub locals: HashMap<String, SOMRef<Value>>,
}

impl Instance {
    pub fn from_class(class: &SOMRef<Class>) -> Instance {
        let class = Rc::clone(class);
        let locals = class
            .borrow()
            .defn()
            .instance_locals
            .iter()
            .cloned()
            .map(|local| (local, Rc::new(RefCell::new(Value::Nil))))
            .collect();

        Instance { class, locals }
    }

    pub fn class(&self) -> SOMRef<Class> {
        self.class.clone()
    }

    pub fn super_class(&self) -> SOMRef<Class> {
        self.class.borrow().super_class()
    }
}
