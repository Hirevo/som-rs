use std::cell::RefCell;
use std::time::Instant;

use anyhow::{bail, Context, Error};
use som_core::bytecode::Bytecode;
use som_gc::{Gc, GcHeap, Trace};

use crate::block::Block;
use crate::class::Class;
use crate::compiler::Literal;
use crate::frame::{Frame, FrameKind};
use crate::interner::Interned;
use crate::method::{Method, MethodKind};
use crate::universe::Universe;
use crate::value::SOMValue;
use crate::SOMRef;

macro_rules! send {
    ($interp:expr, $universe:expr, $heap:expr, $frame:expr, $lit_idx:expr, $nb_params:expr, $bytecode_idx:expr) => {{
        let &Literal::Symbol(symbol) = $frame
            .borrow()
            .lookup_constant($lit_idx as usize)
            .context("SEND without a signature constant")?
        else {
            bail!("SEND with a non-symbol signature constant");
        };
        let nb_params = match $nb_params {
            Some(v) => v,
            None => {
                let signature = $universe.lookup_symbol(symbol);
                nb_params(signature)
            }
        };
        let method = {
            let receiver = $interp
                .stack
                .iter()
                .nth_back(nb_params)
                .context("missing SEND arguments")?;
            let receiver_class = receiver.class($universe);
            resolve_method($frame, &receiver_class, symbol, $bytecode_idx)
        };
        do_send(
            $interp,
            $universe,
            $heap,
            method,
            symbol,
            nb_params as usize,
        )
        .with_context(|| anyhow::anyhow!("error calling `{}`", $universe.lookup_symbol(symbol)))?;
        $heap.maybe_collect_garbage(|| {
            $interp.trace();
            $universe.trace();
        });
    }};
}

macro_rules! super_send {
    ($interp:expr, $universe:expr, $heap:expr, $frame:expr, $lit_idx:expr, $nb_params:expr, $bytecode_idx:expr) => {{
        let &Literal::Symbol(symbol) = $frame
            .borrow()
            .lookup_constant($lit_idx as usize)
            .context("SUPER_SEND without a signature constant")?
        else {
            bail!("SUPER_SEND with a non-symbol signature constant");
        };

        let nb_params = match $nb_params {
            Some(v) => v,
            None => {
                let signature = $universe.lookup_symbol(symbol);
                nb_params(signature)
            }
        };
        let method = {
            let holder = $frame.borrow().get_method_holder();
            let super_class = holder
                .borrow()
                .super_class()
                .context("SUPER_SEND without a super class")?;
            resolve_method($frame, &super_class, symbol, $bytecode_idx)
        };
        do_send(
            $interp,
            $universe,
            $heap,
            method,
            symbol,
            nb_params as usize,
        )
        .with_context(|| anyhow::anyhow!("error calling `{}`", $universe.lookup_symbol(symbol)))?;
        $heap.maybe_collect_garbage(|| {
            $interp.trace();
            $universe.trace();
        });
    }};
}

pub struct Interpreter {
    /// The interpreter's stack frames.
    pub frames: Vec<SOMRef<Frame>>,
    /// The evaluation stack.
    pub stack: Vec<SOMValue>,
    /// The time record of the interpreter's creation.
    pub start_time: Instant,
}

impl Trace for Interpreter {
    #[inline]
    fn trace(&self) {
        self.frames.trace();
        self.stack.trace();
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            frames: vec![],
            stack: vec![],
            start_time: Instant::now(),
        }
    }

    pub fn push_frame(&mut self, heap: &mut GcHeap, kind: FrameKind) -> SOMRef<Frame> {
        let frame = heap.allocate(RefCell::new(Frame::from_kind(kind)));
        self.frames.push(frame.clone());
        frame
    }

    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    pub fn current_frame(&self) -> Option<&SOMRef<Frame>> {
        self.frames.last()
    }

    pub fn run(&mut self, heap: &mut GcHeap, universe: &mut Universe) -> Result<SOMValue, Error> {
        loop {
            let Some(frame) = self.frames.last() else {
                heap.maybe_collect_garbage(|| {
                    self.trace();
                    universe.trace();
                });

                return Ok(self.stack.pop().unwrap_or(SOMValue::NIL));
            };

            let (bytecode, bytecode_idx) = {
                let mut cur_frame = frame.borrow_mut();
                let Some(bytecode) = cur_frame.get_current_bytecode() else {
                    drop(cur_frame);
                    self.pop_frame();
                    self.stack.push(SOMValue::NIL);
                    continue;
                };
                let bytecode_idx = cur_frame.bytecode_idx;
                cur_frame.bytecode_idx += 1;
                (bytecode, bytecode_idx)
            };

            match bytecode {
                Bytecode::Halt => {
                    heap.maybe_collect_garbage(|| {
                        self.trace();
                        universe.trace();
                    });

                    return Ok(SOMValue::NIL);
                }
                Bytecode::Dup => {
                    let value = self
                        .stack
                        .last()
                        .cloned()
                        .context("DUP with missing value")?;
                    self.stack.push(value);
                }
                Bytecode::PushLocal(up_idx, idx) => {
                    let mut from = frame.clone();
                    for _ in 0..up_idx {
                        let temp = match from.borrow().kind() {
                            FrameKind::Block { block } => {
                                block.frame.clone().context("missing block frame")?
                            }
                            FrameKind::Method { .. } => {
                                bail!("requested local from non-existing frame");
                            }
                        };
                        from = temp;
                    }
                    let value = from
                        .borrow()
                        .lookup_local(idx as usize)
                        .context("PUSH_LOCAL with missing local")?;
                    self.stack.push(value);
                }
                Bytecode::PushArgument(up_idx, idx) => {
                    let mut from = frame.clone();
                    for _ in 0..up_idx {
                        let temp = match from.borrow().kind() {
                            FrameKind::Block { block } => {
                                block.frame.clone().context("missing block frame")?
                            }
                            FrameKind::Method { .. } => {
                                bail!("requested local from non-existing frame");
                            }
                        };
                        from = temp;
                    }
                    let value = from
                        .borrow()
                        .lookup_argument(idx as usize)
                        .context("PUSH_ARGUMENT with missing argument")?;
                    self.stack.push(value);
                }
                Bytecode::PushField(idx) => {
                    let holder = frame.borrow().get_method_holder();
                    let value = if holder.borrow().is_static {
                        holder
                            .borrow_mut()
                            .lookup_local(idx as usize)
                            .context("PUSH_FIELD with missing field")?
                    } else {
                        let self_value = frame.borrow().get_self();
                        self_value
                            .lookup_local(idx as usize)
                            .context("PUSH_FIELD with missing field")?
                    };
                    self.stack.push(value);
                }
                Bytecode::PushBlock(idx) => {
                    let value = {
                        let frame_ref = frame.borrow();
                        let literal = frame_ref
                            .lookup_constant(idx as usize)
                            .context("PUSH_BLOCK with missing constant")?;
                        let Literal::Block(block) = literal else {
                            bail!("PUSH_BLOCK with non-block literal constant");
                        };
                        let mut block = Block::clone(&block);
                        block.frame.replace(Gc::clone(&frame));
                        SOMValue::new_block(&heap.allocate(block))
                    };
                    self.stack.push(value);
                }
                Bytecode::PushConstant(idx) => {
                    let value = {
                        let frame_ref = frame.borrow();
                        let literal = frame_ref
                            .lookup_constant(idx as usize)
                            .context("PUSH_CONSTANT with missing constant")?;
                        convert_literal(heap, &frame, literal)?
                    };
                    self.stack.push(value);
                }
                Bytecode::PushConstant0 => {
                    let value = {
                        let frame_ref = frame.borrow();
                        let literal = frame_ref.lookup_constant(0).unwrap();
                        convert_literal(heap, &frame, literal).unwrap()
                    };
                    self.stack.push(value);
                }
                Bytecode::PushConstant1 => {
                    let value = {
                        let frame_ref = frame.borrow();
                        let literal = frame_ref.lookup_constant(1).unwrap();
                        convert_literal(heap, &frame, literal).unwrap()
                    };
                    self.stack.push(value);
                }
                Bytecode::PushConstant2 => {
                    let value = {
                        let frame_ref = frame.borrow();
                        let literal = frame_ref.lookup_constant(2).unwrap();
                        convert_literal(heap, &frame, literal).unwrap()
                    };
                    self.stack.push(value);
                }
                Bytecode::PushGlobal(idx) => {
                    let frame_ref = frame.borrow();
                    let &Literal::Symbol(symbol) = frame_ref
                        .lookup_constant(idx as usize)
                        .context("PUSH_GLOBAL with missing constant")?
                    else {
                        bail!("PUSH_GLOBAL attempted with a non-symbol constant");
                    };
                    let Some(value) = universe.lookup_global(symbol) else {
                        let self_value = frame_ref.get_self();
                        drop(frame_ref);
                        universe
                            .unknown_global(self, heap, self_value, symbol)
                            .context("missing `#unknownGlobal:` method")?;
                        continue;
                    };
                    self.stack.push(value);
                }
                Bytecode::Push0 => {
                    self.stack.push(SOMValue::INTEGER_ZERO);
                }
                Bytecode::Push1 => {
                    self.stack.push(SOMValue::INTEGER_ONE);
                }
                Bytecode::PushNil => {
                    self.stack.push(SOMValue::NIL);
                }
                Bytecode::Pop => {
                    self.stack.pop().context("POP with missing value")?;
                }
                Bytecode::PopLocal(up_idx, idx) => {
                    let value = self.stack.pop().context("POP_LOCAL with missing value")?;
                    let mut from = frame.clone();
                    for _ in 0..up_idx {
                        let temp = match from.borrow().kind() {
                            FrameKind::Block { block } => {
                                block.frame.clone().context("missing block frame")?
                            }
                            FrameKind::Method { .. } => {
                                bail!("requested local from non-existing frame");
                            }
                        };
                        from = temp;
                    }
                    from.borrow_mut()
                        .assign_local(idx as usize, value)
                        .context("POP_LOCAL with missing local")?;
                }
                Bytecode::PopArgument(up_idx, idx) => {
                    let value = self
                        .stack
                        .pop()
                        .context("POP_ARGUMENT with missing value")?;
                    let mut from = frame.clone();
                    for _ in 0..up_idx {
                        let temp = match from.borrow().kind() {
                            FrameKind::Block { block } => {
                                block.frame.clone().context("missing block frame")?
                            }
                            FrameKind::Method { .. } => {
                                bail!("requested local from non-existing frame");
                            }
                        };
                        from = temp;
                    }
                    from.borrow_mut()
                        .args
                        .get_mut(idx as usize)
                        .map(|loc| *loc = value)
                        .context("POP_ARGUMENT with missing argument")?;
                }
                Bytecode::PopField(idx) => {
                    let value = self.stack.pop().context("POP_FIELD with missing value")?;
                    let holder = frame.borrow().get_method_holder();
                    if holder.borrow().is_static {
                        holder
                            .borrow_mut()
                            .assign_local(idx as usize, value)
                            .context("POP_FIELD with missing field")?;
                    } else {
                        let mut self_value = frame.borrow().get_self();
                        self_value
                            .assign_local(idx as usize, value)
                            .context("POP_FIELD with missing field")?;
                    }
                }
                Bytecode::Send1(idx) => {
                    // Send1 => receiver + 0 args, so we pass Some(0)
                    send!(self, universe, heap, frame, idx, Some(0), bytecode_idx)
                }
                Bytecode::Send2(idx) => {
                    send!(self, universe, heap, frame, idx, Some(1), bytecode_idx)
                }
                Bytecode::Send3(idx) => {
                    send!(self, universe, heap, frame, idx, Some(2), bytecode_idx)
                }
                Bytecode::SendN(idx) => {
                    send!(self, universe, heap, frame, idx, None, bytecode_idx)
                }
                Bytecode::SuperSend1(idx) => {
                    super_send!(self, universe, heap, frame, idx, Some(0), bytecode_idx)
                }
                Bytecode::SuperSend2(idx) => {
                    super_send!(self, universe, heap, frame, idx, Some(1), bytecode_idx)
                }
                Bytecode::SuperSend3(idx) => {
                    super_send!(self, universe, heap, frame, idx, Some(2), bytecode_idx)
                }
                Bytecode::SuperSendN(idx) => {
                    super_send!(self, universe, heap, frame, idx, None, bytecode_idx)
                }
                Bytecode::ReturnLocal => {
                    let value = self
                        .stack
                        .pop()
                        .context("RETURN_LOCAL with missing value")?;
                    self.pop_frame();
                    self.stack.push(value);
                }
                Bytecode::ReturnNonLocal => {
                    let value = self
                        .stack
                        .pop()
                        .context("RETURN_NON_LOCAL with missing value")?;
                    let method_frame = Frame::method_frame(frame);
                    let escaped_frames = self
                        .frames
                        .iter()
                        .rev()
                        .position(|live_frame| Gc::ptr_eq(&live_frame, &method_frame));

                    if let Some(count) = escaped_frames {
                        (0..count).for_each(|_| self.pop_frame());
                        self.pop_frame();
                        self.stack.push(value);
                    } else {
                        // Block has escaped its method frame.
                        let frame_ref = frame.borrow();
                        let instance = frame_ref.get_self();
                        let FrameKind::Block { block, .. } = frame_ref.kind() else {
                            // Should never happen, because `universe.current_frame()` would
                            // have been equal to `universe.current_method_frame()`.
                            bail!("A method frame has escaped itself ??");
                        };
                        let block = Gc::clone(&block);
                        drop(frame_ref);
                        // TODO: should we call `doesNotUnderstand:` here ?
                        universe
                            .escaped_block(self, heap, instance, block.clone())
                            .context("A block has escaped and `escapedBlock:` is not defined on receiver")?;
                    }
                }
            }
        }

        fn do_send(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            heap: &mut GcHeap,
            method: Option<Gc<Method>>,
            symbol: Interned,
            nb_params: usize,
        ) -> Result<(), Error> {
            let Some(method) = method else {
                let mut args = Vec::with_capacity(nb_params + 1);

                for _ in 0..nb_params {
                    let arg = interpreter
                        .stack
                        .pop()
                        .context("message send with missing argument")?;
                    args.push(arg);
                }
                let self_value = interpreter
                    .stack
                    .pop()
                    .context("message send with missing receiver")?;

                args.reverse();

                return universe.does_not_understand(interpreter, heap, self_value, symbol, args)
                    .context(
                        "A message cannot be handled and `doesNotUnderstand:arguments:` is not defined on receiver"
                    );
            };

            match &method.kind {
                MethodKind::Defined(_) => {
                    let mut args = Vec::with_capacity(nb_params + 1);

                    for _ in 0..nb_params {
                        let arg = interpreter
                            .stack
                            .pop()
                            .context("message send with missing argument")?;
                        args.push(arg);
                    }
                    let self_value = interpreter
                        .stack
                        .pop()
                        .context("message send with missing receiver")?;
                    args.push(self_value.clone());

                    args.reverse();

                    let frame = interpreter.push_frame(
                        heap,
                        FrameKind::Method {
                            holder: method.holder.clone(),
                            self_value,
                            method,
                        },
                    );
                    frame.borrow_mut().args = args;

                    Ok(())
                }
                MethodKind::Primitive(func) => func(interpreter, heap, universe),
                MethodKind::NotImplemented(err) => {
                    let self_value = interpreter.stack.iter().nth_back(nb_params).unwrap();
                    println!(
                        "{}>>#{}",
                        self_value.class(&universe).borrow().name(),
                        method.signature(),
                    );
                    bail!("Primitive `#{}` not implemented", err)
                }
            }
        }

        fn resolve_method(
            frame: &SOMRef<Frame>,
            class: &SOMRef<Class>,
            signature: Interned,
            bytecode_idx: usize,
        ) -> Option<Gc<Method>> {
            match frame.borrow().kind() {
                FrameKind::Block { block } => {
                    let mut inline_cache = block.blk_info.inline_cache.borrow_mut();

                    // SAFETY: this access is actually safe because the bytecode compiler
                    // makes sure the cache has as many entries as there are bytecode instructions,
                    // therefore we can avoid doing any redundant bounds checks here.
                    let maybe_found = unsafe { inline_cache.get_unchecked_mut(bytecode_idx) };

                    match maybe_found {
                        Some((receiver, method)) if *receiver == Gc::as_ptr(class) => {
                            Some(Gc::clone(method))
                        }
                        place @ None => {
                            let found = class.borrow().lookup_method(signature);
                            *place = found.clone().map(|method| (Gc::as_ptr(class), method));
                            found
                        }
                        _ => class.borrow().lookup_method(signature),
                    }
                }
                FrameKind::Method { method, .. } => {
                    if let MethodKind::Defined(env) = &method.kind {
                        let mut inline_cache = env.inline_cache.borrow_mut();

                        // SAFETY: this access is actually safe because the bytecode compiler
                        // makes sure the cache has as many entries as there are bytecode instructions,
                        // therefore we can avoid doing any redundant bounds checks here.
                        let maybe_found = unsafe { inline_cache.get_unchecked_mut(bytecode_idx) };

                        match maybe_found {
                            Some((receiver, method)) if *receiver == Gc::as_ptr(class) => {
                                Some(Gc::clone(method))
                            }
                            place @ None => {
                                let found = class.borrow().lookup_method(signature);
                                *place = found.clone().map(|method| (Gc::as_ptr(class), method));
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

        fn convert_literal(
            heap: &mut GcHeap,
            frame: &SOMRef<Frame>,
            literal: &Literal,
        ) -> Result<SOMValue, Error> {
            let value = match literal {
                Literal::Symbol(sym) => SOMValue::new_symbol(*sym),
                Literal::String(val) => SOMValue::new_string(val),
                Literal::Double(val) => SOMValue::new_double(*val),
                Literal::Integer(val) => SOMValue::new_integer(*val),
                Literal::BigInteger(val) => SOMValue::new_big_integer(val),
                Literal::Array(val) => {
                    let arr = val
                        .into_iter()
                        .map(|idx| {
                            frame
                                .borrow()
                                .lookup_constant(*idx as usize)
                                .context("missing constant in array literal")
                                .and_then(|lit| convert_literal(heap, frame, lit))
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    SOMValue::new_array(&heap.allocate(RefCell::new(arr)))
                }
                Literal::Block(val) => SOMValue::new_block(&val),
            };
            Ok(value)
        }

        fn nb_params(signature: &str) -> usize {
            match signature.chars().nth(0) {
                Some(ch) if !ch.is_alphabetic() => 1,
                _ => signature.chars().filter(|ch| *ch == ':').count(),
            }
        }
    }
}
