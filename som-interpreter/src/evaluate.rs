use std::cell::RefCell;
use std::rc::Rc;

use som_core::ast;

use crate::block::Block;
use crate::frame::FrameKind;
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

macro_rules! resolve_span {
    ($frame:expr, $span:expr) => {
        $frame
            .borrow()
            .get_method_holder()
            .borrow()
            .resolve_span($span)
    };
}

/// The trait for evaluating AST nodes.
pub trait Evaluate {
    /// Evaluate the node within a given universe.
    fn evaluate(&self, universe: &mut Universe) -> Return;
}

impl Evaluate for ast::Expression {
    fn evaluate(&self, universe: &mut Universe) -> Return {
        let current_frame = universe.current_frame().clone();
        match &self.kind {
            ast::ExpressionKind::Assignment(name, expr) => {
                let name = universe.span_to_symbol(*name);
                let value = propagate!(expr.evaluate(universe));
                universe
                    .assign_local(name, value.clone())
                    .map(|_| Return::Local(value))
                    .unwrap_or_else(|| {
                        Return::Exception(format!(
                            "variable '{}' not found to assign to",
                            universe.lookup_symbol(name),
                        ))
                    })
            }
            ast::ExpressionKind::Block(blk) => blk.evaluate(universe),
            ast::ExpressionKind::Exit(expr) => {
                let value = propagate!(expr.evaluate(universe));
                let frame = universe.current_method_frame();
                let has_not_escaped = universe
                    .frames
                    .iter()
                    .rev()
                    .any(|live_frame| Rc::ptr_eq(&live_frame, &frame));
                if has_not_escaped {
                    Return::NonLocal(value, frame)
                } else {
                    // Block has escaped its method frame.
                    let instance = frame.borrow().get_self();
                    let frame = universe.current_frame();
                    let block = match frame.borrow().kind() {
                        FrameKind::Block { block, .. } => block.clone(),
                        _ => {
                            // Should never happen, because `universe.current_frame()` would
                            // have been equal to `universe.current_method_frame()`.
                            return Return::Exception(format!(
                                "A method frame has escaped itself ??"
                            ));
                        }
                    };
                    universe.escaped_block(instance, block).unwrap_or_else(|| {
                        // TODO: should we call `doesNotUnderstand:` here ?
                        Return::Exception(
                            "A block has escaped and `escapedBlock:` is not defined on receiver"
                                .to_string(),
                        )
                    })
                }
            }
            ast::ExpressionKind::Literal(literal) => match literal {
                ast::Literal::Array(array) => {
                    let mut output = Vec::with_capacity(array.len());
                    for literal in array {
                        let value = propagate!(literal.evaluate(universe));
                        output.push(value);
                    }
                    Return::Local(Value::Array(Rc::new(RefCell::new(output))))
                }
                ast::Literal::Integer => {
                    resolve_span!(current_frame, self.span).parse().map_or_else(
                        |err: std::num::ParseIntError| Return::Exception(err.to_string()),
                        |value| Return::Local(Value::Integer(value)),
                    )
                }
                ast::Literal::Double => {
                    resolve_span!(current_frame, self.span).parse().map_or_else(
                        |err: std::num::ParseFloatError| Return::Exception(err.to_string()),
                        |value| Return::Local(Value::Double(value)),
                    )
                }
                ast::Literal::Symbol(span) => {
                    Return::Local(Value::Symbol(universe.span_to_symbol(*span)))
                }
                ast::Literal::String(span) => Return::Local(Value::String(Rc::new({
                    let string = resolve_span!(current_frame, *span).to_string();
                    let len = string.len();
                    let mut iter = string.chars().peekable();
                    let mut output = String::with_capacity(len);
                    loop {
                        match iter.next() {
                            None => break output,
                            Some('\\') => {
                                let ch = iter.next();
                                match ch {
                                    Some('t') => output.push('\t'),
                                    Some('b') => output.push('\x08'),
                                    Some('n') => output.push('\n'),
                                    Some('r') => output.push('\r'),
                                    Some('f') => output.push('\x12'),
                                    Some('\'') => output.push('\''),
                                    Some('\\') => output.push('\\'),
                                    Some('0') => output.push('\0'),
                                    _ => {}
                                }
                            }
                            Some(ch) => output.push(ch),
                        }
                    }
                }))),
            },
            ast::ExpressionKind::Reference => {
                let name = universe.span_to_symbol(self.span);
                (universe.lookup_local(name))
                    .or_else(|| universe.lookup_global(name))
                    .map(Return::Local)
                    .or_else(|| {
                        let frame = universe.current_frame();
                        let self_value = frame.borrow().get_self();
                        universe.unknown_global(self_value, name)
                    })
                    .unwrap_or_else(|| {
                        Return::Exception(format!(
                            "variable '{}' not found",
                            universe.lookup_symbol(name),
                        ))
                    })
            }
            ast::ExpressionKind::Term(term) => term.evaluate(universe),
            ast::ExpressionKind::Message(msg) => msg.evaluate(universe),
        }
    }
}

impl Evaluate for ast::BinaryOp {
    fn evaluate(&self, universe: &mut Universe) -> Return {
        let lhs = propagate!(self.lhs.evaluate(universe));
        let rhs = propagate!(self.rhs.evaluate(universe));

        let signature = universe.span_to_symbol(self.op);

        // println!(
        //     "invoking {}>>#{}",
        //     lhs.class(universe).borrow().name(),
        //     self.signature
        // );

        if let Some(invokable) = lhs.lookup_method(universe, signature) {
            invokable.invoke(universe, vec![lhs, rhs])
        } else {
            universe
                .does_not_understand(lhs.clone(), signature, vec![rhs])
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "could not find method '{}>>#{}'",
                        lhs.class(universe).borrow().name(),
                        universe.lookup_symbol(signature),
                    ))
                    // Return::Local(Value::Nil)
                })
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
        // TODO: avoid cloning the whole block's AST.
        Return::Local(Value::Block(Rc::new(Block {
            block: self.clone(),
            frame: frame.clone(),
        })))
    }
}

impl Evaluate for ast::Message {
    fn evaluate(&self, universe: &mut Universe) -> Return {
        let signature = universe.intern_symbol(self.signature.as_str());
        let (receiver, invokable) = match self.receiver.kind {
            // ast::ExpressionKind::Reference
            //     if resolve_span!(universe.current_frame(), self.receiver.span) == "super" =>
            // {
            //     let frame = universe.current_frame();
            //     let receiver = frame.borrow().get_self();
            //     let holder = frame.borrow().get_method_holder();
            //     let invokable = holder.borrow().lookup_method(&self.signature);
            //     (receiver, invokable)
            // }
            ast::ExpressionKind::Reference
                if resolve_span!(universe.current_frame(), self.receiver.span) == "super" =>
            {
                let frame = universe.current_frame();
                let receiver = frame.borrow().get_self();
                let holder = frame.borrow().get_method_holder();
                let super_class = match holder.borrow().super_class() {
                    Some(class) => class,
                    None => {
                        return Return::Exception(
                            "`super` used without any superclass available".to_string(),
                        )
                    }
                };
                let invokable = super_class.borrow().lookup_method(signature);
                (receiver, invokable)
            }
            _ => {
                let receiver = propagate!(self.receiver.evaluate(universe));
                let invokable = receiver.lookup_method(universe, signature);
                (receiver, invokable)
            }
        };
        let args = match &self.kind {
            ast::MessageKind::Unary => vec![receiver.clone()],
            ast::MessageKind::Binary { rhs } => {
                let value = propagate!(rhs.evaluate(universe));
                vec![receiver.clone(), value]
            }
            ast::MessageKind::Positional { values, .. } => {
                let mut output = Vec::with_capacity(values.len() + 1);
                output.push(receiver.clone());
                for expr in values {
                    let value = propagate!(expr.evaluate(universe));
                    output.push(value);
                }
                output
            }
        };

        // println!(
        //     "invoking {}>>#{} with ({:?})",
        //     receiver.class(universe).borrow().name(),
        //     self.signature,
        //     self.values,
        // );

        let value = match invokable {
            Some(invokable) => invokable.invoke(universe, args),
            None => {
                let mut args = args;
                args.remove(0);
                universe
                    .does_not_understand(receiver.clone(), signature, args)
                    .unwrap_or_else(|| {
                        Return::Exception(format!(
                            "could not find method '{}>>#{}'",
                            receiver.class(universe).borrow().name(),
                            self.signature
                        ))
                        // Return::Local(Value::Nil)
                    })
            }
        };

        value
    }
}

impl Evaluate for ast::Body {
    fn evaluate(&self, universe: &mut Universe) -> Return {
        let mut last_value = Value::Nil;
        for expr in &self.exprs {
            last_value = propagate!(expr.evaluate(universe));
        }
        Return::Local(last_value)
    }
}
