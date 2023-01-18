use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use som_core::bytecode::Bytecode;

use crate::class::Class;
use crate::compiler::Literal;
use crate::frame::FrameKind;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::{SOMRef, SOMWeakRef};

#[derive(Clone)]
pub struct MethodEnv {
    pub locals: Vec<Value>,
    pub literals: Vec<Literal>,
    pub body: Vec<Bytecode>,
    pub inline_cache_receiver: RefCell<Vec<*const Class>>,
    pub inline_cache_invocable: RefCell<Vec<Option<Rc<Method>>>>,
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
                interpreter.stack.push(receiver);
                interpreter.stack.append(&mut args);
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
