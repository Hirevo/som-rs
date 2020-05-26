use som_core::ast::MethodDef;

use crate::primitives;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

#[derive(Debug)]
pub enum Return {
    Local(Value),
    NonLocal(Value),
    Exception(String),
}

pub trait Invoke {
    fn invoke(&self, universe: &mut Universe, args: Vec<Value>) -> Return;
}

pub enum Invokable {
    MethodDef(MethodDef),
    Primitive(PrimitiveFn),
}

impl Invokable {
    pub fn primitive_from_signature(
        class_name: impl AsRef<str>,
        signature: impl AsRef<str>,
    ) -> Invokable {
        let class_name = class_name.as_ref();
        let signature = signature.as_ref();
        println!("loading primitive of '{}>>#{}'", class_name, signature);
        let primitive = match class_name {
            "Integer" => primitives::integer::get_primitive(signature),
            "Double" => primitives::double::get_primitive(signature),
            "Array" => primitives::array::get_primitive(signature),
            _ => todo!("loading primitive of '{}>>#{}'", class_name, signature),
        };
        Invokable::Primitive(
            primitive.unwrap_or_else(|| {
                panic!("primitive not found for '{}>>#{}'", class_name, signature)
            }),
        )
    }

    pub fn is_primitive(&self) -> bool {
        matches!(self, Invokable::Primitive(_))
    }
}

impl Invoke for Invokable {
    fn invoke(&self, universe: &mut Universe, args: Vec<Value>) -> Return {
        match self {
            Invokable::MethodDef(method) => method.invoke(universe, args),
            Invokable::Primitive(func) => Return::Local(func(universe, args)),
        }
    }
}

impl Invoke for MethodDef {
    fn invoke(&self, universe: &mut Universe, args: Vec<Value>) -> Return {
        let (self_ref, params) = {
            let mut iter = args.into_iter();
            let receiver = iter.next().expect("missing receiver for invocation");
            (receiver, iter.collect::<Vec<_>>())
        };

        universe.with_frame(self_ref, |universe| {
            todo!("make use of `Return::NonLocal` enum")
        })
    }
}
