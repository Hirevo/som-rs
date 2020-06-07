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

/// The kind of a class method.
#[derive(Clone)]
pub enum MethodKind {
    /// A user-defined method from the AST.
    Defined(ast::MethodDef),
    /// An interpreter primitive.
    Primitive(PrimitiveFn),
    /// A non-implemented primitive.
    NotImplemented(String),
}

impl MethodKind {
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
        primitive.map(MethodKind::Primitive).unwrap_or_else(|| {
            MethodKind::NotImplemented(format!("{}>>#{}", class_name, signature))
        })
        // .unwrap_or_else(|| panic!("unimplemented primitive: '{}>>#{}'", class_name, signature))
    }

    /// Whether this invocable is a primitive.
    pub fn is_primitive(&self) -> bool {
        matches!(self, Self::Primitive(_))
    }
}

/// Represents a class method.
#[derive(Clone)]
pub struct Method {
    pub kind: MethodKind,
    pub holder: SOMRef<Class>,
    pub signature: String,
}

impl Method {
    pub fn class(&self, universe: &Universe) -> SOMRef<Class> {
        if self.is_primitive() {
            universe.primitive_class()
        } else {
            universe.method_class()
        }
    }

    pub fn kind(&self) -> &MethodKind {
        &self.kind
    }

    pub fn holder(&self) -> &SOMRef<Class> {
        &self.holder
    }

    pub fn signature(&self) -> &str {
        self.signature.as_str()
    }

    /// Whether this invocable is a primitive.
    pub fn is_primitive(&self) -> bool {
        self.kind.is_primitive()
    }
}

impl Invoke for Method {
    fn invoke(&self, universe: &mut Universe, args: Vec<Value>) -> Return {
        let output = match self.kind() {
            MethodKind::Defined(method) => {
                let (self_value, params) = {
                    let mut iter = args.into_iter();
                    let receiver = match iter.next() {
                        Some(receiver) => receiver,
                        None => {
                            return Return::Exception("missing receiver for invocation".to_string())
                        }
                    };
                    (receiver, iter.collect::<Vec<_>>())
                };
                universe.with_frame(
                    FrameKind::Method {
                        holder: self.holder().clone(),
                        self_value,
                    },
                    |universe| method.invoke(universe, params),
                )
            }
            MethodKind::Primitive(func) => func(universe, args),
            MethodKind::NotImplemented(name) => {
                Return::Exception(format!("unimplemented primitive: {}", name))
            }
        };
        match output {
            // Return::Exception(msg) => Return::Exception(format!(
            //     "from {}>>#{}\n{}",
            //     self.holder().borrow().name(),
            //     self.signature(),
            //     msg,
            // )),
            output => output,
        }
    }
}

impl Invoke for ast::MethodDef {
    fn invoke(&self, universe: &mut Universe, args: Vec<Value>) -> Return {
        let current_frame = universe.current_frame().clone();
        match &self.kind {
            ast::MethodKind::Unary => {}
            ast::MethodKind::Positional { parameters } => current_frame
                .borrow_mut()
                .bindings
                .extend(parameters.iter().cloned().zip(args)),
            ast::MethodKind::Operator { rhs } => {
                let rhs_value = match args.into_iter().next() {
                    Some(value) => value,
                    None => {
                        // This should never happen in theory (the parser would have caught the missing rhs).
                        return Return::Exception(format!(
                            "no right-hand side for operator call ?"
                        ));
                    }
                };
                current_frame
                    .borrow_mut()
                    .bindings
                    .insert(rhs.clone(), rhs_value);
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
                        Return::Local(_) => break Return::Local(current_frame.borrow().get_self()),
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
    }
}

impl Invoke for Block {
    fn invoke(&self, universe: &mut Universe, args: Vec<Value>) -> Return {
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
    }
}
