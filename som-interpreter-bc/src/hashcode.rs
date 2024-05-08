use std::hash::{Hash, Hasher};

use crate::block::Block;
use crate::class::Class;
use crate::instance::Instance;
use crate::method::Method;
use crate::value::ValueEnum;

impl Hash for ValueEnum {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        match self {
            ValueEnum::Nil => {
                hasher.write(b"#nil#");
            }
            ValueEnum::System => {
                hasher.write(b"#system#");
            }
            ValueEnum::Boolean(value) => {
                hasher.write(b"#bool#");
                value.hash(hasher);
            }
            ValueEnum::Integer(value) => {
                hasher.write(b"#int#");
                value.hash(hasher);
            }
            ValueEnum::BigInteger(value) => {
                hasher.write(b"#bigint#");
                value.hash(hasher);
            }
            ValueEnum::Double(value) => {
                hasher.write(b"#double#");
                let raw_bytes: &[u8] = unsafe {
                    std::slice::from_raw_parts(
                        (value as *const f64) as *const u8,
                        std::mem::size_of::<f64>(),
                    )
                };
                hasher.write(raw_bytes);
            }
            ValueEnum::Symbol(value) => {
                hasher.write(b"#sym#");
                value.hash(hasher);
            }
            ValueEnum::String(value) => {
                hasher.write(b"#string#");
                value.hash(hasher);
            }
            ValueEnum::Array(value) => {
                hasher.write(b"#arr#");
                for value in value.borrow().iter() {
                    value.hash(hasher);
                }
            }
            ValueEnum::Block(value) => {
                hasher.write(b"#blk#");
                value.hash(hasher);
            }
            ValueEnum::Class(value) => {
                hasher.write(b"#cls#");
                value.borrow().hash(hasher);
            }
            ValueEnum::Instance(value) => {
                hasher.write(b"#inst#");
                value.borrow().hash(hasher);
            }
            ValueEnum::Invokable(value) => {
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
        self.holder.borrow().hash(hasher);
        hasher.write(b">>");
        self.signature.hash(hasher);
    }
}
