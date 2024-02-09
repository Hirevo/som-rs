use std::rc::Rc;

use som_core::ast;

use crate::block::Block;
use crate::evaluate::Evaluate;
use crate::frame::Frame;
use crate::frame::FrameKind;
use crate::method::{Method, MethodKind};
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
                let holder = match self.holder().upgrade() {
                    Some(holder) => holder,
                    None => {
                        return Return::Exception(
                            "cannot invoke this method because its holder has been collected"
                                .to_string(),
                        )
                    }
                };
                let signature = universe.intern_symbol(&self.signature);
                universe.with_frame(
                    FrameKind::Method {
                        holder,
                        signature,
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
