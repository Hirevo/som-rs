use std::hash::{Hash, Hasher};

use crate::block::Block;
use crate::class::Class;
use crate::instance::Instance;
use crate::method::Method;
use crate::value::Value;

impl Hash for Value {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        match self {
            Value::Nil => {
                hasher.write(b"#nil#");
            }
            Value::System => {
                hasher.write(b"#system#");
            }
            Value::Boolean(value) => {
                hasher.write(b"#bool#");
                value.hash(hasher);
            }
            Value::Integer(value) => {
                hasher.write(b"#int#");
                value.hash(hasher);
            }
            Value::BigInteger(value) => {
                hasher.write(b"#bigint#");
                value.hash(hasher);
            }
            Value::Double(value) => {
                hasher.write(b"#double#");
                let raw_bytes: &[u8] = unsafe {
                    std::slice::from_raw_parts(
                        (value as *const f64) as *const u8,
                        std::mem::size_of::<f64>(),
                    )
                };
                hasher.write(raw_bytes);
            }
            Value::Symbol(value) => {
                hasher.write(b"#sym#");
                value.hash(hasher);
            }
            Value::String(value) => {
                hasher.write(b"#string#");
                value.hash(hasher);
            }
            Value::Array(value) => {
                hasher.write(b"#arr#");
                for value in value.borrow().iter() {
                    value.hash(hasher);
                }
            }
            Value::Block(value) => {
                hasher.write(b"#blk#");
                value.hash(hasher);
            }
            Value::Class(value) => {
                hasher.write(b"#cls#");
                value.borrow().hash(hasher);
            }
            Value::Instance(value) => {
                hasher.write(b"#inst#");
                value.borrow().hash(hasher);
            }
            Value::Invokable(value) => {
                hasher.write(b"#mthd#");
                value.hash(hasher);
            }
        }
    }
}

impl Hash for Class {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.name.hash(hasher);
        self.locals.iter().for_each(|value| {
            value.hash(hasher);
        });
    }
}

impl Hash for Instance {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.class.borrow().hash(hasher);
        self.locals.iter().for_each(|value| {
            value.hash(hasher);
        });
    }
}

impl Hash for Block {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.blk_info.literals.iter().for_each(|it| it.hash(hasher));
        self.blk_info.locals.iter().for_each(|it| it.hash(hasher));
        self.blk_info.nb_params.hash(hasher);
        self.blk_info.body.iter().for_each(|it| it.hash(hasher));
    }
}

impl Hash for Method {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        if let Some(holder) = self.holder().upgrade() {
            holder.borrow().hash(hasher);
        } else {
            hasher.write(b"??");
        }
        hasher.write(b">>");
        self.signature.hash(hasher);
    }
}
