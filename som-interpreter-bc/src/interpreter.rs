use std::cell::RefCell;
use std::rc::Rc;
use std::time::Instant;

use som_core::bytecode::Bytecode;

use crate::block::Block;
use crate::compiler::Literal;
use crate::frame::{Frame, FrameKind};
use crate::method::MethodKind;
use crate::universe::Universe;
use crate::value::Value;
use crate::SOMRef;

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
                    let value = convert_literal(&frame, literal).unwrap();
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
                Bytecode::Send(idx) => {
                    let literal = frame.borrow().lookup_constant(idx as usize).unwrap();
                    let symbol = match literal {
                        Literal::Symbol(sym) => sym,
                        _ => {
                            return None;
                        }
                    };
                    let signature = universe.lookup_symbol(symbol);
                    let nb_params = nb_params(signature);
                    let method = {
                        let receiver = self.stack.iter().nth_back(nb_params)?;
                        let receiver_class = receiver.class(universe);
                        match frame.borrow().kind() {
                            FrameKind::Block { block } => {
                                let mut inline_cache_receiver =
                                    block.inline_cache_receiver.borrow_mut();
                                let mut inline_cache_invocable =
                                    block.inline_cache_invocable.borrow_mut();
                                // SAFETY: this access is actually safe because the bytecode compiler
                                // makes sure the cache has as many entries as there are bytecode instructions,
                                // therefore we can avoid doing any redundant bounds checks here.
                                let maybe_found_receiver = unsafe {
                                    inline_cache_receiver.get_unchecked_mut(bytecode_idx)
                                };
                                let maybe_found_invocable = unsafe {
                                    inline_cache_invocable.get_unchecked_mut(bytecode_idx)
                                };

                                match (maybe_found_receiver, maybe_found_invocable) {
                                    (receiver, Some(method))
                                        if *receiver == receiver_class.as_ptr() =>
                                    {
                                        Some(Rc::clone(method))
                                    }
                                    (receiver, method) => {
                                        let found = receiver_class.borrow().lookup_method(symbol);
                                        *receiver = receiver_class.as_ptr();
                                        *method = found.clone();
                                        found
                                    }
                                }
                            }
                            FrameKind::Method { method, .. } => {
                                if let MethodKind::Defined(env) = method.kind() {
                                    let mut inline_cache_receiver =
                                        env.inline_cache_receiver.borrow_mut();
                                    let mut inline_cache_invocable =
                                        env.inline_cache_invocable.borrow_mut();

                                    // SAFETY: this access is actually safe because the bytecode compiler
                                    // makes sure the cache has as many entries as there are bytecode instructions,
                                    // therefore we can avoid doing any redundant bounds checks here.
                                    let maybe_found_receiver = unsafe {
                                        inline_cache_receiver.get_unchecked_mut(bytecode_idx)
                                    };
                                    let maybe_found_invocable = unsafe {
                                        inline_cache_invocable.get_unchecked_mut(bytecode_idx)
                                    };

                                    match (maybe_found_receiver, maybe_found_invocable) {
                                        (receiver, Some(method))
                                            if *receiver == receiver_class.as_ptr() =>
                                        {
                                            Some(Rc::clone(method))
                                        }
                                        (receiver, method) => {
                                            let found =
                                                receiver_class.borrow().lookup_method(symbol);
                                            *receiver = receiver_class.as_ptr();
                                            *method = found.clone();
                                            found
                                        }
                                    }
                                } else {
                                    receiver_class.borrow().lookup_method(symbol)
                                }
                            }
                        }
                    };

                    if let Some(method) = method {
                        match method.kind() {
                            MethodKind::Defined(_) => {
                                let mut args = Vec::with_capacity(nb_params + 1);

                                for _ in 0..nb_params {
                                    let arg = self.stack.pop().unwrap();
                                    args.push(arg);
                                }
                                let self_value = self.stack.pop().unwrap();
                                args.push(self_value.clone());

                                args.reverse();

                                let holder = method.holder.upgrade().unwrap();
                                let frame = self.push_frame(FrameKind::Method {
                                    self_value,
                                    method,
                                    holder,
                                });
                                frame.borrow_mut().args = args;
                            }
                            MethodKind::Primitive(func) => {
                                func(self, universe);
                            }
                            MethodKind::NotImplemented(err) => {
                                let self_value = self.stack.iter().nth_back(nb_params).unwrap();
                                println!(
                                    "{}>>#{}",
                                    self_value.class(&universe).borrow().name(),
                                    method.signature()
                                );
                                panic!("Primitive `#{}` not implemented", err)
                            }
                        }
                    } else {
                        let mut args = Vec::with_capacity(nb_params + 1);

                        for _ in 0..nb_params {
                            let arg = self.stack.pop().unwrap();
                            args.push(arg);
                        }
                        let self_value = self.stack.pop().unwrap();

                        args.reverse();

                        universe.does_not_understand(self, self_value, symbol, args)
                            .expect(
                                "A message cannot be handled and `doesNotUnderstand:arguments:` is not defined on receiver"
                            );
                    }
                }
                Bytecode::SuperSend(idx) => {
                    let literal = frame.borrow().lookup_constant(idx as usize).unwrap();
                    let symbol = match literal {
                        Literal::Symbol(sym) => sym,
                        _ => {
                            return None;
                        }
                    };
                    let signature = universe.lookup_symbol(symbol);
                    let nb_params = nb_params(signature);
                    let holder = frame.borrow().get_method_holder();
                    let method = {
                        let super_class = holder.borrow().super_class()?;
                        match frame.borrow().kind() {
                            FrameKind::Block { block } => {
                                let mut inline_cache_receiver =
                                    block.inline_cache_receiver.borrow_mut();
                                let mut inline_cache_invocable =
                                    block.inline_cache_invocable.borrow_mut();

                                // SAFETY: this access is actually safe because the bytecode compiler
                                // makes sure the cache has as many entries as there are bytecode instructions,
                                // therefore we can avoid doing any redundant bounds checks here.
                                let maybe_found_receiver = unsafe {
                                    inline_cache_receiver.get_unchecked_mut(bytecode_idx)
                                };
                                let maybe_found_invocable = unsafe {
                                    inline_cache_invocable.get_unchecked_mut(bytecode_idx)
                                };

                                match (maybe_found_receiver, maybe_found_invocable) {
                                    (receiver, Some(method))
                                        if *receiver == super_class.as_ptr() =>
                                    {
                                        Some(Rc::clone(method))
                                    }
                                    (receiver, method) => {
                                        let found = super_class.borrow().lookup_method(symbol);
                                        *receiver = super_class.as_ptr();
                                        *method = found.clone();
                                        found
                                    }
                                }
                            }
                            FrameKind::Method { method, .. } => {
                                if let MethodKind::Defined(env) = method.kind() {
                                    let mut inline_cache_receiver =
                                        env.inline_cache_receiver.borrow_mut();
                                    let mut inline_cache_invocable =
                                        env.inline_cache_invocable.borrow_mut();

                                    // SAFETY: this access is actually safe because the bytecode compiler
                                    // makes sure the cache has as many entries as there are bytecode instructions,
                                    // therefore we can avoid doing any redundant bounds checks here.
                                    let maybe_found_receiver = unsafe {
                                        inline_cache_receiver.get_unchecked_mut(bytecode_idx)
                                    };
                                    let maybe_found_invocable = unsafe {
                                        inline_cache_invocable.get_unchecked_mut(bytecode_idx)
                                    };

                                    match (maybe_found_receiver, maybe_found_invocable) {
                                        (receiver, Some(method))
                                            if *receiver == super_class.as_ptr() =>
                                        {
                                            Some(Rc::clone(method))
                                        }
                                        (receiver, method) => {
                                            let found = super_class.borrow().lookup_method(symbol);
                                            *receiver = super_class.as_ptr();
                                            *method = found.clone();
                                            found
                                        }
                                    }
                                } else {
                                    super_class.borrow().lookup_method(symbol)
                                }
                            }
                        }
                    };

                    if let Some(method) = method {
                        match method.kind() {
                            MethodKind::Defined(_) => {
                                let mut args = Vec::with_capacity(nb_params + 1);

                                for _ in 0..nb_params {
                                    let arg = self.stack.pop().unwrap();
                                    args.push(arg);
                                }
                                let self_value = self.stack.pop().unwrap();
                                args.push(self_value.clone());

                                args.reverse();

                                let holder = method.holder.upgrade().unwrap();
                                let frame = self.push_frame(FrameKind::Method {
                                    self_value,
                                    method,
                                    holder,
                                });
                                frame.borrow_mut().args = args;
                            }
                            MethodKind::Primitive(func) => {
                                func(self, universe);
                            }
                            MethodKind::NotImplemented(err) => {
                                panic!("Primitive `#{}` not implemented", err)
                            }
                        }
                    } else {
                        let mut args = Vec::with_capacity(nb_params + 1);

                        for _ in 0..nb_params {
                            let arg = self.stack.pop().unwrap();
                            args.push(arg);
                        }
                        let self_value = self.stack.pop().unwrap();

                        args.reverse();

                        universe.does_not_understand(self, self_value, symbol, args)
                            .expect(
                                "A message cannot be handled and `doesNotUnderstand:arguments:` is not defined on receiver"
                            );
                    }
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
