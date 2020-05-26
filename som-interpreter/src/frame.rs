use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::value::Value;
use crate::SOMRef;

#[derive(Debug)]
pub struct Frame {
    pub self_ref: SOMRef<Value>,
    pub bindings: HashMap<String, SOMRef<Value>>,
}

impl Frame {
    pub fn with_self(self_ref: Value) -> Frame {
        Frame {
            self_ref: Rc::new(RefCell::new(self_ref)),
            bindings: HashMap::new(),
        }
    }

    pub fn get_self(&self) -> SOMRef<Value> {
        self.self_ref.clone()
    }
}
