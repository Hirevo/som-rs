use std::rc::Rc;

use som_core::ast;

use crate::block::Block;
use crate::class::Class;
use crate::evaluate::Evaluate;
use crate::frame::Frame;
use crate::frame::FrameKind;
use crate::primitives;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::SOMRef;

/// Represents the kinds of possible returns from an invocation.
#[derive(Debug)]
pub enum Return {
    /// A local return, the value is for the immediate caller.
    Local(Value),
    /// A non-local return, the value is for the parent of the referenced stack frame.
    NonLocal(Value, SOMRef<Frame>),
    /// An exception, expected to bubble all the way up.
    Exception(String),
    /// A request to restart execution from the top of the closest body.
    Restart,
}

/// The trait for invoking methods and primitives.
pub trait Invoke {
    /// Invoke within the given universe and with the given arguments.
    fn invoke(&self, universe: &mut Universe, args: Vec<Value>) -> Return;
}

/// The kind of invocables for a class method.
#[derive(Clone)]
pub enum Invokable {
    /// A user-defined method from the AST.
    MethodDef(ast::MethodDef),
    /// An interpreter primitive.
    Primitive(PrimitiveFn),
    /// A non-implemented primitive.
    NotImplemented(String),
}

impl Invokable {
    /// Return the interpreter primitive matching a given class name and signature.
    pub fn primitive_from_signature(
        class_name: impl AsRef<str>,
        signature: impl AsRef<str>,
    ) -> Self {
        let class_name = class_name.as_ref();
        let signature = signature.as_ref();
        let primitive = match class_name {
            "Object" => primitives::object::get_primitive(signature),
            "Class" => primitives::class::get_primitive(signature),
            "Integer" => primitives::integer::get_primitive(signature),
            "Double" => primitives::double::get_primitive(signature),
            "Array" => primitives::array::get_primitive(signature),
            "String" => primitives::string::get_primitive(signature),
            "Symbol" => primitives::symbol::get_primitive(signature),
            "System" => primitives::system::get_primitive(signature),
            "Method" => primitives::method::get_primitive(signature),
            "Primitive" => primitives::method::get_primitive(signature),
            "Block" => primitives::block1::get_primitive(signature),
            "Block1" => primitives::block1::get_primitive(signature),
            "Block2" => primitives::block2::get_primitive(signature),
            "Block3" => primitives::block3::get_primitive(signature),
            _ => None,
        };
        // println!(
        //     "loading primitive of '{}>>#{}': {}",
        //     class_name,
        //     signature,
        //     primitive.is_some()
        // );
        primitive
            .map(Self::Primitive)
            .unwrap_or_else(|| Self::NotImplemented(format!("{}>>#{}", class_name, signature)))
    }

    pub fn class(&self, universe: &Universe) -> SOMRef<Class> {
        if self.is_primitive() {
            universe.primitive_class()
        } else {
            universe.method_class()
        }
    }

    /// Whether this invocable is a primitive.
    pub fn is_primitive(&self) -> bool {
        matches!(self, Self::Primitive(_))
    }
}

impl Invoke for Invokable {
    fn invoke(&self, universe: &mut Universe, args: Vec<Value>) -> Return {
        match self {
            Self::MethodDef(method) => method.invoke(universe, args),
            Self::Primitive(func) => func(universe, args),
            Self::NotImplemented(name) => {
                Return::Exception(format!("unimplemented primitive: {}", name))
            }
        }
    }
}

impl Invoke for ast::MethodDef {
    fn invoke(&self, universe: &mut Universe, args: Vec<Value>) -> Return {
        let (self_value, params) = {
            let mut iter = args.into_iter();
            let receiver = iter.next().expect("missing receiver for invocation");
            (receiver, iter.collect::<Vec<_>>())
        };

        universe.with_frame(FrameKind::Method(self_value), |universe| {
            let current_frame = universe.current_frame().clone();
            match &self.kind {
                ast::MethodKind::Unary => {}
                ast::MethodKind::Positional { parameters } => current_frame
                    .borrow_mut()
                    .bindings
                    .extend(parameters.iter().cloned().zip(params)),
                ast::MethodKind::Operator { rhs } => {
                    current_frame
                        .borrow_mut()
                        .bindings
                        .insert(rhs.clone(), params[0].clone());
                }
            }
            match &self.body {
                ast::MethodBody::Body { locals, body } => {
                    current_frame
                        .borrow_mut()
                        .bindings
                        .extend(locals.iter().cloned().zip(std::iter::repeat(Value::Nil)));
                    loop {
                        match body.evaluate(universe) {
                            Return::NonLocal(value, frame) => {
                                if Rc::ptr_eq(&current_frame, &frame) {
                                    break Return::Local(value);
                                } else {
                                    break Return::NonLocal(value, frame);
                                }
                            }
                            Return::Local(_) => {
                                break Return::Local(current_frame.borrow().get_self())
                            }
                            Return::Exception(msg) => break Return::Exception(msg),
                            Return::Restart => continue,
                        }
                    }
                }
                ast::MethodBody::Primitive => Return::Exception(format!(
                    "unimplemented primitive: {}>>#{}",
                    current_frame
                        .borrow()
                        .get_self()
                        .class(universe)
                        .borrow()
                        .name(),
                    self.signature,
                )),
            }
        })
    }
}

impl Invoke for Block {
    fn invoke(&self, universe: &mut Universe, args: Vec<Value>) -> Return {
        universe.with_frame(FrameKind::Block(self.frame.clone()), |universe| {
            let current_frame = universe.current_frame();
            current_frame.borrow_mut().bindings.extend(
                self.block
                    .parameters
                    .iter()
                    .cloned()
                    .zip(args.into_iter().skip(1)),
            );
            current_frame.borrow_mut().bindings.extend(
                self.block
                    .locals
                    .iter()
                    .cloned()
                    .zip(std::iter::repeat(Value::Nil)),
            );
            self.block.body.evaluate(universe)
        })
    }
}
