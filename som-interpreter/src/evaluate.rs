use std::cell::RefCell;
use std::rc::Rc;

use som_core::ast;

use crate::block::Block;
use crate::invokable::{Invoke, Return};
use crate::universe::Universe;
use crate::value::Value;

macro_rules! propagate {
    ($expr:expr) => {
        match $expr {
            Return::Local(value) => value,
            ret => return ret,
        }
    };
}

/// The trait for evaluating AST nodes.
pub trait Evaluate {
    /// Evaluate the node within a given universe.
    fn evaluate(&self, universe: &mut Universe) -> Return;
}

impl Evaluate for ast::Expression {
    fn evaluate(&self, universe: &mut Universe) -> Return {
        match self {
            Self::Assignment(name, expr) => {
                let value = propagate!(expr.evaluate(universe));
                universe
                    .assign_local(name, value.clone())
                    .unwrap_or_else(|| panic!("variable '{}' not found", name));
                Return::Local(value)
            }
            Self::BinaryOp(bin_op) => bin_op.evaluate(universe),
            Self::Block(blk) => blk.evaluate(universe),
            Self::Exit(expr) => {
                let value = propagate!(expr.evaluate(universe));
                let frame = universe.current_method_frame();
                Return::NonLocal(value, frame)
            }
            Self::Literal(literal) => literal.evaluate(universe),
            Self::Reference(name) => Return::Local(
                (universe.lookup(name))
                    .or_else(|| universe.lookup_global(name))
                    .or_else(|| {
                        let sym = dbg!(universe.intern_symbol(name));
                        Value::System
                            .lookup_method(universe, "unknownGlobal:")
                            .map(|method| {
                                match method
                                    .invoke(universe, vec![Value::System, Value::Symbol(sym)])
                                {
                                    Return::Local(value) | Return::NonLocal(value, _) => value,
                                    Return::Exception(err) => panic!(
                                        "got exception from 'System>>#unknownGlobal:': {}",
                                        err
                                    ),
                                }
                            })
                    })
                    .unwrap_or_else(|| panic!("variable '{}' not found", name)),
            ),
            Self::Term(term) => term.evaluate(universe),
            Self::Message(msg) => msg.evaluate(universe),
        }
    }
}

impl Evaluate for ast::BinaryOp {
    fn evaluate(&self, universe: &mut Universe) -> Return {
        let lhs = propagate!(self.lhs.evaluate(universe));
        let rhs = propagate!(self.rhs.evaluate(universe));

        if let Some(invokable) = lhs.lookup_method(universe, &self.op) {
            invokable.invoke(universe, vec![lhs, rhs])
        } else {
            Return::Local(Value::Nil)
        }
    }
}

impl Evaluate for ast::Literal {
    fn evaluate(&self, universe: &mut Universe) -> Return {
        match self {
            Self::Array(array) => {
                let mut output = Vec::with_capacity(array.len());
                for literal in array {
                    let value = propagate!(literal.evaluate(universe));
                    output.push(value);
                }
                Return::Local(Value::Array(Rc::new(RefCell::new(output))))
            }
            Self::Integer(int) => Return::Local(Value::Integer(*int)),
            Self::Double(double) => Return::Local(Value::Double(*double)),
            Self::Symbol(sym) => Return::Local(Value::Symbol(universe.intern_symbol(sym))),
            Self::String(string) => Return::Local(Value::String(Rc::new(string.clone()))),
        }
    }
}

impl Evaluate for ast::Term {
    fn evaluate(&self, universe: &mut Universe) -> Return {
        self.body.evaluate(universe)
    }
}

impl Evaluate for ast::Block {
    fn evaluate(&self, universe: &mut Universe) -> Return {
        let frame = universe.current_frame();
        Return::Local(Value::Block(Block {
            block: self.clone(),
            frame: Rc::downgrade(&frame),
        }))
    }
}

impl Evaluate for ast::Message {
    fn evaluate(&self, universe: &mut Universe) -> Return {
        let receiver = propagate!(self.receiver.evaluate(universe));
        let args = {
            let mut output = Vec::with_capacity(self.values.len() + 1);
            output.push(receiver.clone());
            for expr in &self.values {
                let value = propagate!(expr.evaluate(universe));
                output.push(value);
            }
            output
        };

        if let Some(invokable) = receiver.lookup_method(universe, &self.signature) {
            invokable.invoke(universe, args)
        } else {
            Return::Local(Value::Nil)
        }
    }
}

impl Evaluate for ast::Body {
    fn evaluate(&self, universe: &mut Universe) -> Return {
        let mut last_value = Value::Nil;
        for expr in &self.exprs {
            // last_value = propagate!(dbg!(expr).evaluate(universe));
            last_value = propagate!(expr.evaluate(universe));
        }
        Return::Local(last_value)
    }
}
