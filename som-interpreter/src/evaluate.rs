use std::rc::Rc;

use som_core::ast::{BinaryOp, Block, Body, Expression, Literal, Message, Term};

use crate::universe::Universe;
use crate::value::Value;

pub trait Evaluate {
    fn evaluate(&self, universe: &mut Universe) -> Value;
}

impl Evaluate for Expression {
    fn evaluate(&self, universe: &mut Universe) -> Value {
        match self {
            Expression::Assignment(name, expr) => {
                let value = expr.evaluate(universe);
                let var = universe
                    .lookup(name)
                    .unwrap_or_else(|| panic!("variable '{}' not found"));
                var.replace(value.clone());
                value
            }
            Expression::BinaryOp(bin_op) => bin_op.evaluate(universe),
            Expression::Block(blk) => blk.evaluate(universe),
            Expression::Exit(expr) => {
                let value = expr.evaluate(universe);
                value
            }
            Expression::Literal(literal) => literal.evaluate(universe),
            Expression::Reference(name) => universe
                .lookup(name)
                .unwrap_or_else(|| panic!("variable '{}' not found"))
                .borrow()
                .clone(),
            Expression::Term(term) => term.evaluate(universe),
            Expression::Message(msg) => msg.evaluate(universe),
        }
    }
}

impl Evaluate for BinaryOp {
    fn evaluate(&self, universe: &mut Universe) -> Value {
        let lhs = self.lhs.evaluate(universe);
        let rhs = self.rhs.evaluate(universe);
        todo!()
    }
}

impl Evaluate for Literal {
    fn evaluate(&self, universe: &mut Universe) -> Value {
        match self {
            Literal::Array(array) => Value::Array(
                array
                    .into_iter()
                    .map(|lit| lit.evaluate(universe))
                    .collect(),
            ),
            Literal::Integer(int) => Value::Integer(*int),
            Literal::Double(double) => Value::Double(*double),
            Literal::Symbol(sym) => Value::Symbol(Rc::new(sym.clone())),
            Literal::String(string) => Value::Symbol(Rc::new(string.clone())),
        }
    }
}
impl Evaluate for Term {
    fn evaluate(&self, universe: &mut Universe) -> Value {
        todo!()
    }
}
impl Evaluate for Block {
    fn evaluate(&self, universe: &mut Universe) -> Value {
        todo!()
    }
}
impl Evaluate for Message {
    fn evaluate(&self, universe: &mut Universe) -> Value {
        todo!()
    }
}
impl Evaluate for Body {
    fn evaluate(&self, universe: &mut Universe) -> Value {
        todo!()
    }
}
