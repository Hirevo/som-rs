use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use som_core::bytecode::Bytecode;

use crate::class::Class;
use crate::compiler::Literal;
use crate::frame::FrameKind;
use crate::interpreter::Interpreter;
use crate::primitives;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{SOMRef, SOMWeakRef};

#[derive(Clone)]
pub struct MethodEnv {
    pub locals: Vec<Value>,
    pub literals: Vec<Literal>,
    pub body: Vec<Bytecode>,
    pub inline_cache: RefCell<HashMap<(*const Class, usize), Rc<Method>>>,
}

/// The kind of a class method.
#[derive(Clone)]
pub enum MethodKind {
    /// A user-defined method from the AST.
    Defined(MethodEnv),
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
    pub holder: SOMWeakRef<Class>,
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

    pub fn holder(&self) -> &SOMWeakRef<Class> {
        &self.holder
    }

    pub fn signature(&self) -> &str {
        self.signature.as_str()
    }

    /// Whether this invocable is a primitive.
    pub fn is_primitive(&self) -> bool {
        self.kind.is_primitive()
    }

    pub fn invoke(
        self: Rc<Self>,
        interpreter: &mut Interpreter,
        universe: &mut Universe,
        receiver: Value,
        mut args: Vec<Value>,
    ) {
        match self.kind() {
            MethodKind::Defined(_) => {
                let holder = self.holder().upgrade().unwrap();
                let kind = FrameKind::Method {
                    method: self,
                    holder,
                    self_value: receiver.clone(),
                };

                let frame = interpreter.push_frame(kind);
                frame.borrow_mut().args.push(receiver);
                frame.borrow_mut().args.append(&mut args);
            }
            MethodKind::Primitive(func) => {
                let frame = interpreter.current_frame().unwrap();
                frame.borrow_mut().stack.push(receiver);
                frame.borrow_mut().stack.append(&mut args);
                func(interpreter, universe)
            }
            MethodKind::NotImplemented(_) => todo!(),
        }
    }
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "#{}>>#{} = ",
            self.holder.upgrade().unwrap().borrow().name(),
            self.signature
        )?;
        match &self.kind {
            MethodKind::Defined(env) => {
                writeln!(f, "(")?;
                write!(f, "    <{} locals>", env.locals.len())?;
                for bytecode in &env.body {
                    writeln!(f)?;
                    write!(f, "    {}  ", bytecode.padded_name())?;
                    match bytecode {
                        Bytecode::Halt => {}
                        Bytecode::Dup => {}
                        Bytecode::PushLocal(up_idx, idx) => {
                            write!(f, "local: {}, context: {}", idx, up_idx)?;
                        }
                        Bytecode::PushArgument(up_idx, idx) => {
                            write!(f, "argument: {}, context: {}", idx, up_idx)?;
                        }
                        Bytecode::PushField(idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        Bytecode::PushBlock(idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        Bytecode::PushConstant(idx) => {
                            write!(f, "index: {}, ", idx)?;
                            let constant = &env.literals[*idx as usize];
                            match constant {
                                Literal::Symbol(_) => write!(f, "value: (#Symbol)"),
                                Literal::String(value) => write!(f, "value: (#String) {:?}", value),
                                Literal::Double(value) => write!(f, "value: (#Double) {}", value),
                                Literal::Integer(value) => write!(f, "value: (#Integer) {}", value),
                                Literal::BigInteger(value) => {
                                    write!(f, "value: (#Integer) {}", value)
                                }
                                Literal::Array(_) => write!(f, "value: (#Array)"),
                                Literal::Block(_) => write!(f, "value: (#Block)"),
                            }?;
                        }
                        Bytecode::PushGlobal(idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        Bytecode::Pop => {}
                        Bytecode::PopLocal(up_idx, idx) => {
                            write!(f, "local: {}, context: {}", idx, up_idx)?;
                        }
                        Bytecode::PopArgument(up_idx, idx) => {
                            write!(f, "argument: {}, context: {}", idx, up_idx)?;
                        }
                        Bytecode::PopField(idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        Bytecode::Send(idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        Bytecode::SuperSend(idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        Bytecode::ReturnLocal => {}
                        Bytecode::ReturnNonLocal => {}
                    }
                }
                Ok(())
            }
            MethodKind::Primitive(_) => write!(f, "<primitive>"),
            MethodKind::NotImplemented(_) => write!(f, "<primitive>"),
        }
    }
}
