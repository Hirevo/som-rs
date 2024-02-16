use std::cell::RefCell;
use std::rc::Rc;
use std::time::Instant;

use som_core::bytecode::Bytecode;

use crate::block::Block;
use crate::class::Class;
use crate::compiler::Literal;
use crate::frame::{Frame, FrameKind};
use crate::interner::Interned;
use crate::method::{Method, MethodKind};
use crate::universe::Universe;
use crate::value::Value;
use crate::SOMRef;

const INT_0: Value = Value::Integer(0);
const INT_1: Value = Value::Integer(1);

macro_rules! send {
    ($interp:expr, $universe:expr, $frame:expr, $lit_idx:expr, $nb_params:expr, $bytecode_idx:expr) => {{
        let literal = $frame.borrow().lookup_constant($lit_idx as usize).unwrap();
        let Literal::Symbol(symbol) = literal else {
            return None;
        };
        let nb_params = match $nb_params {
            Some(v) => v,
            None => {
                let signature = $universe.lookup_symbol(symbol);
                nb_params(signature)
            }
        };
        let method = {
            let receiver = $interp.stack.iter().nth_back(nb_params)?;
            let receiver_class = receiver.class($universe);
            resolve_method($frame, &receiver_class, symbol, $bytecode_idx)
        };
        do_send($interp, $universe, method, symbol, nb_params as usize);
    }};
}

macro_rules! super_send {
    ($interp:expr, $universe:expr, $frame_expr:expr, $lit_idx:expr, $nb_params:expr, $bytecode_idx:expr) => {{
        let literal = $frame_expr
            .borrow()
            .lookup_constant($lit_idx as usize)
            .unwrap();
        let Literal::Symbol(symbol) = literal else {
            return None;
        };
        let nb_params = match $nb_params {
            Some(v) => v,
            None => {
                let signature = $universe.lookup_symbol(symbol);
                nb_params(signature)
            }
        };
        let method = {
            let holder = $frame_expr.borrow().get_method_holder();
            let super_class = holder.borrow().super_class().unwrap();
            resolve_method($frame_expr, &super_class, symbol, $bytecode_idx)
        };
        do_send($interp, $universe, method, symbol, nb_params as usize);
    }};
}

pub struct Interpreter {
    /// The interpreter's stack frames.
    pub frames: Vec<SOMRef<Frame>>,
    /// The evaluation stack.
    pub stack: Vec<Value>,
    /// The time record of the interpreter's creation.
    pub start_time: Instant,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            frames: vec![],
            stack: vec![],
            start_time: Instant::now(),
        }
    }

    pub fn push_frame(&mut self, kind: FrameKind) -> SOMRef<Frame> {
        let frame = Rc::new(RefCell::new(Frame::from_kind(kind)));
        self.frames.push(frame.clone());
        frame
    }

    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    pub fn current_frame(&self) -> Option<&SOMRef<Frame>> {
        self.frames.last()
    }

    pub fn run(&mut self, universe: &mut Universe) -> Option<Value> {
        loop {
            let frame = match self.current_frame() {
                Some(frame) => frame,
                None => return Some(self.stack.pop().unwrap_or(Value::Nil)),
            };

            let bytecode_idx = frame.borrow().bytecode_idx;
            let opt_bytecode = frame.borrow().get_current_bytecode();
            let bytecode = match opt_bytecode {
                Some(bytecode) => bytecode,
                None => {
                    self.pop_frame();
                    self.stack.push(Value::Nil);
                    continue;
                }
            };

            frame.borrow_mut().bytecode_idx += 1;

            match bytecode {
                Bytecode::Halt => {
                    return Some(Value::Nil);
                }
                Bytecode::Dup => {
                    let value = self.stack.last().cloned().unwrap();
                    self.stack.push(value);
                }
                Bytecode::PushLocal(up_idx, idx) => {
                    let mut from = frame.clone();
                    for _ in 0..up_idx {
                        let temp = match from.borrow().kind() {
                            FrameKind::Block { block } => block.frame.clone().unwrap(),
                            FrameKind::Method { .. } => {
                                panic!("requested local from non-existing frame")
                            }
                        };
                        from = temp;
                    }
                    let value = from.borrow().lookup_local(idx as usize).unwrap();
                    self.stack.push(value);
                }
                Bytecode::PushArgument(up_idx, idx) => {
                    let mut from = frame.clone();
                    for _ in 0..up_idx {
                        let temp = match from.borrow().kind() {
                            FrameKind::Block { block } => block.frame.clone().unwrap(),
                            FrameKind::Method { .. } => {
                                panic!("requested local from non-existing frame")
                            }
                        };
                        from = temp;
                    }
                    let value = from.borrow().lookup_argument(idx as usize).unwrap();
                    self.stack.push(value);
                }
                Bytecode::PushField(idx) => {
                    let holder = frame.borrow().get_method_holder();
                    let value = if holder.borrow().is_static {
                        holder.borrow_mut().lookup_local(idx as usize).unwrap()
                    } else {
                        let self_value = frame.borrow().get_self();
                        self_value.lookup_local(idx as usize).unwrap()
                    };
                    self.stack.push(value);
                }
                Bytecode::PushBlock(idx) => {
                    let literal = frame.borrow().lookup_constant(idx as usize).unwrap();
                    let mut block = match literal {
                        Literal::Block(blk) => Block::clone(&blk),
                        _ => return None,
                    };
                    block.frame.replace(Rc::clone(&frame));
                    self.stack.push(Value::Block(Rc::new(block)));
                }
                Bytecode::PushConstant(idx) => {
                    let literal = frame.borrow().lookup_constant(idx as usize).unwrap();
                    let value = convert_literal(frame, literal).unwrap();
                    self.stack.push(value);
                }
                Bytecode::PushConstant0 => {
                    let literal = frame.borrow().lookup_constant(0).unwrap();
                    let value = convert_literal(frame, literal).unwrap();
                    self.stack.push(value);
                }
                Bytecode::PushConstant1 => {
                    let literal = frame.borrow().lookup_constant(1).unwrap();
                    let value = convert_literal(frame, literal).unwrap();
                    self.stack.push(value);
                }
                Bytecode::PushConstant2 => {
                    let literal = frame.borrow().lookup_constant(2).unwrap();
                    let value = convert_literal(frame, literal).unwrap();
                    self.stack.push(value);
                }
                Bytecode::PushGlobal(idx) => {
                    let literal = frame.borrow().lookup_constant(idx as usize).unwrap();
                    let symbol = match literal {
                        Literal::Symbol(sym) => sym,
                        _ => return None,
                    };
                    if let Some(value) = universe.lookup_global(symbol) {
                        self.stack.push(value);
                    } else {
                        let self_value = frame.borrow().get_self();
                        universe.unknown_global(self, self_value, symbol).unwrap();
                    }
                }
                Bytecode::Push0 => {
                    self.stack.push(INT_0);
                }
                Bytecode::Push1 => {
                    self.stack.push(INT_1);
                }
                Bytecode::PushNil => {
                    self.stack.push(Value::Nil);
                }
                Bytecode::Pop => {
                    self.stack.pop();
                }
                Bytecode::PopLocal(up_idx, idx) => {
                    let value = self.stack.pop().unwrap();
                    let mut from = self.current_frame().unwrap().clone();
                    for _ in 0..up_idx {
                        let temp = match from.borrow().kind() {
                            FrameKind::Block { block } => block.frame.clone().unwrap(),
                            FrameKind::Method { .. } => {
                                panic!("requested local from non-existing frame")
                            }
                        };
                        from = temp;
                    }
                    from.borrow_mut().assign_local(idx as usize, value).unwrap();
                }
                Bytecode::PopArgument(up_idx, idx) => {
                    let value = self.stack.pop().unwrap();
                    let mut from = self.current_frame().unwrap().clone();
                    for _ in 0..up_idx {
                        let temp = match from.borrow().kind() {
                            FrameKind::Block { block } => block.frame.clone().unwrap(),
                            FrameKind::Method { .. } => {
                                panic!("requested local from non-existing frame")
                            }
                        };
                        from = temp;
                    }
                    from.borrow_mut()
                        .args
                        .get_mut(idx as usize)
                        .map(|loc| *loc = value)
                        .unwrap();
                }
                Bytecode::PopField(idx) => {
                    let value = self.stack.pop().unwrap();
                    let frame = self.current_frame().unwrap();
                    let holder = frame.borrow().get_method_holder();
                    if holder.borrow().is_static {
                        holder
                            .borrow_mut()
                            .assign_local(idx as usize, value)
                            .unwrap();
                    } else {
                        let mut self_value = frame.borrow().get_self();
                        self_value.assign_local(idx as usize, value).unwrap();
                    }
                }
                Bytecode::Send1(idx) => {
                    send! {self, universe, frame, idx, Some(0), bytecode_idx} // Send1 => receiver + 0 args, so we pass Some(0)
                }
                Bytecode::Send2(idx) => {
                    send! {self, universe, frame, idx, Some(1), bytecode_idx}
                }
                Bytecode::Send3(idx) => {
                    send! {self, universe, frame, idx, Some(2), bytecode_idx}
                }
                Bytecode::SendN(idx) => {
                    send! {self, universe, frame, idx, None, bytecode_idx}
                }
                Bytecode::SuperSend1(idx) => {
                    super_send! {self, universe, frame, idx, Some(0), bytecode_idx}
                }
                Bytecode::SuperSend2(idx) => {
                    super_send! {self, universe, frame, idx, Some(1), bytecode_idx}
                }
                Bytecode::SuperSend3(idx) => {
                    super_send! {self, universe, frame, idx, Some(2), bytecode_idx}
                }
                Bytecode::SuperSendN(idx) => {
                    super_send! {self, universe, frame, idx, None, bytecode_idx}
                }
                Bytecode::ReturnLocal => {
                    let value = self.stack.pop().unwrap();
                    self.pop_frame();
                    self.stack.push(value);
                }
                Bytecode::ReturnNonLocal => {
                    let value = self.stack.pop().unwrap();
                    let frame = self.current_frame().unwrap();
                    let method_frame = Frame::method_frame(&frame);
                    let escaped_frames = self
                        .frames
                        .iter()
                        .rev()
                        .position(|live_frame| Rc::ptr_eq(&live_frame, &method_frame));

                    if let Some(count) = escaped_frames {
                        (0..count).for_each(|_| self.pop_frame());
                        self.pop_frame();
                        self.stack.push(value);
                    } else {
                        // Block has escaped its method frame.
                        let instance = frame.borrow().get_self();
                        let block = match frame.borrow().kind() {
                            FrameKind::Block { block, .. } => block.clone(),
                            _ => {
                                // Should never happen, because `universe.current_frame()` would
                                // have been equal to `universe.current_method_frame()`.
                                panic!("A method frame has escaped itself ??");
                            }
                        };
                        // TODO: should we call `doesNotUnderstand:` here ?
                        universe.escaped_block(self, instance, block).expect(
                            "A block has escaped and `escapedBlock:` is not defined on receiver",
                        );
                    }
                }
            }
        }

        fn do_send(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            method: Option<Rc<Method>>,
            symbol: Interned,
            nb_params: usize,
        ) {
            let Some(method) = method else {
                let mut args = Vec::with_capacity(nb_params + 1);

                for _ in 0..nb_params {
                    let arg = interpreter.stack.pop().unwrap();
                    args.push(arg);
                }
                let self_value = interpreter.stack.pop().unwrap();

                args.reverse();

                universe.does_not_understand(interpreter, self_value, symbol, args)
                    .expect(
                        "A message cannot be handled and `doesNotUnderstand:arguments:` is not defined on receiver"
                    );

                return;
            };

            match method.kind() {
                MethodKind::Defined(_) => {
                    let mut args = Vec::with_capacity(nb_params + 1);

                    for _ in 0..nb_params {
                        let arg = interpreter.stack.pop().unwrap();
                        args.push(arg);
                    }
                    let self_value = interpreter.stack.pop().unwrap();
                    args.push(self_value.clone());

                    args.reverse();

                    let holder = method.holder.upgrade().unwrap();
                    let frame = interpreter.push_frame(FrameKind::Method {
                        self_value,
                        method,
                        holder,
                    });
                    frame.borrow_mut().args = args;
                }
                MethodKind::Primitive(func) => {
                    func(interpreter, universe);
                }
                MethodKind::NotImplemented(err) => {
                    let self_value = interpreter.stack.iter().nth_back(nb_params).unwrap();
                    println!(
                        "{}>>#{}",
                        self_value.class(&universe).borrow().name(),
                        method.signature(),
                    );
                    panic!("Primitive `#{}` not implemented", err)
                }
            }
        }

        fn resolve_method(
            frame: &SOMRef<Frame>,
            class: &SOMRef<Class>,
            signature: Interned,
            bytecode_idx: usize,
        ) -> Option<Rc<Method>> {
            match frame.borrow().kind() {
                FrameKind::Block { block } => {
                    let mut inline_cache = block.blk_info.inline_cache.borrow_mut();

                    // SAFETY: this access is actually safe because the bytecode compiler
                    // makes sure the cache has as many entries as there are bytecode instructions,
                    // therefore we can avoid doing any redundant bounds checks here.
                    let maybe_found = unsafe { inline_cache.get_unchecked_mut(bytecode_idx) };

                    match maybe_found {
                        Some((receiver, method)) if *receiver == class.as_ptr() => {
                            Some(Rc::clone(method))
                        }
                        place @ None => {
                            let found = class.borrow().lookup_method(signature);
                            *place = found
                                .clone()
                                .map(|method| (class.as_ptr() as *const _, method));
                            found
                        }
                        _ => class.borrow().lookup_method(signature),
                    }
                }
                FrameKind::Method { method, .. } => {
                    if let MethodKind::Defined(env) = method.kind() {
                        let mut inline_cache = env.inline_cache.borrow_mut();

                        // SAFETY: this access is actually safe because the bytecode compiler
                        // makes sure the cache has as many entries as there are bytecode instructions,
                        // therefore we can avoid doing any redundant bounds checks here.
                        let maybe_found = unsafe { inline_cache.get_unchecked_mut(bytecode_idx) };

                        match maybe_found {
                            Some((receiver, method)) if *receiver == class.as_ptr() => {
                                Some(Rc::clone(method))
                            }
                            place @ None => {
                                let found = class.borrow().lookup_method(signature);
                                *place = found
                                    .clone()
                                    .map(|method| (class.as_ptr() as *const _, method));
                                found
                            }
                            _ => class.borrow().lookup_method(signature),
                        }
                    } else {
                        class.borrow().lookup_method(signature)
                    }
                }
            }
        }

        fn convert_literal(frame: &SOMRef<Frame>, literal: Literal) -> Option<Value> {
            let value = match literal {
                Literal::Symbol(sym) => Value::Symbol(sym),
                Literal::String(val) => Value::String(val),
                Literal::Double(val) => Value::Double(val),
                Literal::Integer(val) => Value::Integer(val),
                Literal::BigInteger(val) => Value::BigInteger(val),
                Literal::Array(val) => {
                    let arr = val
                        .into_iter()
                        .map(|idx| {
                            frame
                                .borrow()
                                .lookup_constant(idx as usize)
                                .and_then(|lit| convert_literal(frame, lit))
                        })
                        .collect::<Option<Vec<_>>>()
                        .unwrap();
                    Value::Array(Rc::new(RefCell::new(arr)))
                }
                Literal::Block(val) => Value::Block(val),
            };
            Some(value)
        }

        fn nb_params(signature: &str) -> usize {
            match signature.chars().nth(0) {
                Some(ch) if !ch.is_alphabetic() => 1,
                _ => signature.chars().filter(|ch| *ch == ':').count(),
            }
        }
    }
}
