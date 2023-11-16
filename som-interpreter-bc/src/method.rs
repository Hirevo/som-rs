use std::cell::RefCell;
use std::fmt;

use anyhow::Error;
use som_core::bytecode::Bytecode;
use som_gc::{Gc, GcHeap, Trace};

use crate::class::Class;
use crate::compiler::Literal;
use crate::frame::FrameKind;
use crate::interner::Interned;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
// use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::SOMValue;
use crate::SOMRef;

#[derive(Clone)]
pub struct MethodEnv {
    pub locals: Vec<Interned>,
    pub literals: Vec<Literal>,
    pub body: Vec<Bytecode>,
    pub inline_cache: RefCell<Vec<Option<(*const RefCell<Class>, Gc<Method>)>>>,
}

/// The kind of a class method.
#[derive(Clone)]
pub enum MethodKind {
    /// A user-defined method from the AST.
    Defined(MethodEnv),
    /// An interpreter primitive.
    Primitive(&'static PrimitiveFn),
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
    pub holder: SOMRef<Class>,
    pub signature: String,
}

impl Trace for MethodKind {
    #[inline]
    fn trace(&self) {
        match self {
            MethodKind::Defined(env) => env.trace(),
            MethodKind::Primitive(_) => {}
            MethodKind::NotImplemented(_) => {}
        }
    }
}

impl Trace for MethodEnv {
    #[inline]
    fn trace(&self) {
        self.literals.trace();
        self.inline_cache.trace();
    }
}

impl Trace for Method {
    #[inline]
    fn trace(&self) {
        self.kind.trace();
        self.holder.trace();
    }
}

impl Method {
    pub fn class(&self, universe: &Universe) -> SOMRef<Class> {
        if self.is_primitive() {
            universe.primitive_class()
        } else {
            universe.method_class()
        }
    }

    pub fn signature(&self) -> &str {
        self.signature.as_str()
    }

    /// Whether this invocable is a primitive.
    pub fn is_primitive(&self) -> bool {
        self.kind.is_primitive()
    }

    pub fn invoke(
        this: Gc<Self>,
        interpreter: &mut Interpreter,
        heap: &mut GcHeap,
        universe: &mut Universe,
        receiver: SOMValue,
        mut args: Vec<SOMValue>,
    ) -> Result<(), Error> {
        match &this.kind {
            MethodKind::Defined(_) => {
                let kind = FrameKind::Method {
                    holder: this.holder.clone(),
                    method: this,
                    self_value: receiver.clone(),
                };
                let frame = interpreter.push_frame(heap, kind);
                frame.borrow_mut().args.push(receiver);
                frame.borrow_mut().args.append(&mut args);
                Ok(())
            }
            MethodKind::Primitive(func) => {
                interpreter.stack.push(receiver);
                interpreter.stack.append(&mut args);
                func(interpreter, heap, universe)
            }
            MethodKind::NotImplemented(_) => todo!(),
        }
    }
}

impl fmt::Display for Method {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "#{}>>#{} = ",
            self.holder.borrow().name(),
            self.signature,
        )?;
        match &self.kind {
            MethodKind::Defined(env) => {
                writeln!(fmt, "(")?;
                write!(fmt, "    <{} locals>", env.locals.len())?;
                for bytecode in &env.body {
                    writeln!(fmt)?;
                    write!(fmt, "    {}  ", bytecode.padded_name())?;
                    match bytecode {
                        Bytecode::Halt => {}
                        Bytecode::Dup => {}
                        Bytecode::PushLocal(up_idx, idx) => {
                            write!(fmt, "local: {}, context: {}", idx, up_idx)?;
                        }
                        Bytecode::PushArgument(up_idx, idx) => {
                            write!(fmt, "argument: {}, context: {}", idx, up_idx)?;
                        }
                        Bytecode::PushField(idx) => {
                            write!(fmt, "index: {}", idx)?;
                        }
                        Bytecode::PushBlock(idx) => {
                            write!(fmt, "index: {}", idx)?;
                        }
                        Bytecode::PushConstant0
                        | Bytecode::PushConstant1
                        | Bytecode::PushConstant2 => {}
                        Bytecode::PushConstant(idx) => {
                            write!(fmt, "index: {}, ", idx)?;
                            let constant = &env.literals[*idx as usize];
                            match constant {
                                Literal::Symbol(_) => write!(fmt, "value: (#Symbol)"),
                                Literal::String(value) => {
                                    write!(fmt, "value: (#String) {:?}", value)
                                }
                                Literal::Double(value) => write!(fmt, "value: (#Double) {}", value),
                                Literal::Integer(value) => {
                                    write!(fmt, "value: (#Integer) {}", value)
                                }
                                Literal::BigInteger(value) => {
                                    write!(fmt, "value: (#Integer) {}", value)
                                }
                                Literal::Array(_) => write!(fmt, "value: (#Array)"),
                                Literal::Block(_) => write!(fmt, "value: (#Block)"),
                            }?;
                        }
                        Bytecode::PushGlobal(idx) => {
                            write!(fmt, "index: {}", idx)?;
                        }
                        Bytecode::Push0 => {}
                        Bytecode::Push1 => {}
                        Bytecode::PushNil => {}
                        Bytecode::Pop => {}
                        Bytecode::PopLocal(up_idx, idx) => {
                            write!(fmt, "local: {}, context: {}", idx, up_idx)?;
                        }
                        Bytecode::PopArgument(up_idx, idx) => {
                            write!(fmt, "argument: {}, context: {}", idx, up_idx)?;
                        }
                        Bytecode::PopField(idx) => {
                            write!(fmt, "index: {}", idx)?;
                        }
                        Bytecode::Send1(idx)
                        | Bytecode::Send2(idx)
                        | Bytecode::Send3(idx)
                        | Bytecode::SendN(idx) => {
                            write!(fmt, "index: {}", idx)?;
                        }
                        Bytecode::SuperSend1(idx)
                        | Bytecode::SuperSend2(idx)
                        | Bytecode::SuperSend3(idx)
                        | Bytecode::SuperSendN(idx) => {
                            write!(fmt, "index: {}", idx)?;
                        }
                        Bytecode::ReturnLocal => {}
                        Bytecode::ReturnNonLocal => {}
                    }
                }
                Ok(())
            }
            MethodKind::Primitive(_) => write!(fmt, "<primitive>"),
            MethodKind::NotImplemented(_) => write!(fmt, "<primitive>"),
        }
    }
}
