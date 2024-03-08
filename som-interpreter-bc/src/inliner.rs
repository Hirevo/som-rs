use crate::block::{Block, BlockInfo};
use crate::compiler::MethodCodegen;
use crate::compiler::{InnerGenCtxt, Literal};
use crate::inliner::JumpType::{JumpOnFalse, JumpOnTrue};
use crate::inliner::OrAndChoice::{And, Or};
use rand::Rng;
use som_core::ast;
use som_core::bytecode::Bytecode;
use std::rc::Rc;

pub enum JumpType {
    JumpOnFalse,
    JumpOnTrue,
}

pub enum OrAndChoice {
    Or,
    And,
}

// TODO some of those should return Result types and throw errors instead, most likely.
pub trait PrimMessageInliner {
    fn inline_if_possible(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message)
                          -> Option<()>;
    fn adapt_block_after_outer_inlined(&self, ctxt: &mut dyn InnerGenCtxt, block_body: &Block, adjust_scope_by: usize) -> Block;
    fn inline_compiled_block(&self, ctxt: &mut dyn InnerGenCtxt, block: &BlockInfo) -> Option<()>;
    fn inline_last_push_block_bc(&self, ctxt: &mut dyn InnerGenCtxt) -> Option<()>;
    fn inline_if_true_or_if_false(
        &self,
        ctxt: &mut dyn InnerGenCtxt,
        message: &ast::Message,
        jump_type: JumpType,
    ) -> Option<()>;
    fn inline_if_true_if_false(
        &self,
        ctxt: &mut dyn InnerGenCtxt,
        message: &ast::Message,
        jump_type: JumpType,
    ) -> Option<()>;
    fn inline_while(
        &self,
        ctxt: &mut dyn InnerGenCtxt,
        message: &ast::Message,
        jump_type: JumpType,
    ) -> Option<()>;
    fn inline_or_and(
        &self,
        ctxt: &mut dyn InnerGenCtxt,
        message: &ast::Message,
        or_and_choice: OrAndChoice,
    ) -> Option<()>;
}

impl PrimMessageInliner for ast::Expression {
    fn inline_if_possible(
        &self,
        ctxt: &mut dyn InnerGenCtxt,
        message: &ast::Message,
    ) -> Option<()> {
        let has_inlined = match message.signature.as_str() {
            "ifTrue:" => self.inline_if_true_or_if_false(ctxt, message, JumpOnFalse),
            "ifFalse:" => self.inline_if_true_or_if_false(ctxt, message, JumpOnTrue),
            "ifTrue:ifFalse:" => self.inline_if_true_if_false(ctxt, message, JumpOnFalse),
            "ifFalse:ifTrue:" => self.inline_if_true_if_false(ctxt, message, JumpOnTrue),
            "whileTrue:" => self.inline_while(ctxt, message, JumpOnFalse),
            "whileFalse:" => self.inline_while(ctxt, message, JumpOnTrue),
            "or:" => self.inline_or_and(ctxt, message, Or),
            "and:" => self.inline_or_and(ctxt, message, And),
            // TODO: to:do, maybe others i'm forgetting
            _ => None,
        };
        // if has_inlined.is_some() { // todo maybe? probably unneeded
        //     ctxt.remove_dup_popx_pop_sequences();
        // }
        has_inlined
    }

    fn inline_compiled_block(&self, ctxt: &mut dyn InnerGenCtxt, block: &BlockInfo) -> Option<()> {
        let nbr_locals_pre_inlining = ctxt.get_nbr_locals();

        let mut rand_thread = rand::thread_rng();
        let og_scope = rand_thread.gen(); // does this matter? should it be the exact same as the original compiled block? i'm thinking it's fine like this?
        for block_local_intern_id in &block.locals {
            let symbol_str = ctxt.lookup_symbol(*block_local_intern_id);
            ctxt.push_local(String::from(symbol_str), og_scope);
        }

        // last is always ReturnLocal, so it gets ignored
        if let Some((_, body)) = block.body.split_last() {
            for block_bc in body {
                match block_bc {
                    Bytecode::PushLocal(up_idx, idx) => match up_idx {
                        0 => ctxt.push_instr(Bytecode::PushLocal(
                            *up_idx,
                            nbr_locals_pre_inlining as u8 + *idx,
                        )),
                        1.. => ctxt.push_instr(Bytecode::PushLocal(*up_idx - 1, *idx)),
                    },
                    Bytecode::PopLocal(up_idx, idx) => match up_idx {
                        0 => ctxt.push_instr(Bytecode::PopLocal(
                            *up_idx,
                            nbr_locals_pre_inlining as u8 + *idx,
                        )),
                        1.. => ctxt.push_instr(Bytecode::PopLocal(*up_idx - 1, *idx)),
                    },
                    Bytecode::PushArgument(up_idx, idx) => {
                        ctxt.push_instr(Bytecode::PushArgument(*up_idx - 1, *idx))
                    } // not 100% sure i need to adjust the up_idx there and for pop
                    Bytecode::PopArgument(up_idx, idx) => {
                        ctxt.push_instr(Bytecode::PopArgument(*up_idx - 1, *idx))
                    }
                    Bytecode::Send1(lit_idx)
                    | Bytecode::Send2(lit_idx)
                    | Bytecode::Send3(lit_idx)
                    | Bytecode::SendN(lit_idx) => {
                        match block.literals.get(*lit_idx as usize)? {
                            Literal::Symbol(interned) => {
                                // TODO does this push duplicate literals? I think it doesn't?
                                let idx = ctxt.push_literal(Literal::Symbol(*interned));

                                match block_bc {
                                    Bytecode::Send1(_) => {
                                        ctxt.push_instr(Bytecode::Send1(idx as u8))
                                    }
                                    Bytecode::Send2(_) => {
                                        ctxt.push_instr(Bytecode::Send2(idx as u8))
                                    }
                                    Bytecode::Send3(_) => {
                                        ctxt.push_instr(Bytecode::Send3(idx as u8))
                                    }
                                    Bytecode::SendN(_) => {
                                        ctxt.push_instr(Bytecode::SendN(idx as u8))
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            _ => panic!("Unexpected block literal type, not yet implemented"),
                        }
                    }
                    Bytecode::PushBlock(block_idx) => {
                        match block.literals.get(*block_idx as usize)? {
                            Literal::Block(inner_block) => {
                                // let new_block = inner_block.as_ref().clone();
                                let new_block = self.adapt_block_after_outer_inlined(ctxt, &inner_block, 1);
                                let idx = ctxt.push_literal(Literal::Block(Rc::from(new_block)));
                                ctxt.push_instr(Bytecode::PushBlock(idx as u8));
                            }
                            _ => panic!("PushBlock not actually pushing a block somehow"),
                        };
                    }
                    Bytecode::PushGlobal(global_idx) => {
                        match block.literals.get(*global_idx as usize)? {
                            lit => {
                                let lit_idx = ctxt.push_literal(lit.clone());
                                ctxt.push_instr(Bytecode::PushGlobal(lit_idx as u8));
                            }
                        };
                    }
                    Bytecode::PushConstant(_)
                    | Bytecode::PushConstant0
                    | Bytecode::PushConstant1
                    | Bytecode::PushConstant2 => {
                        let constant_idx = match block_bc {
                            Bytecode::PushConstant(idx) => *idx,
                            Bytecode::PushConstant0 => 0,
                            Bytecode::PushConstant1 => 1,
                            Bytecode::PushConstant2 => 2,
                            _ => unreachable!(),
                        };

                        match block.literals.get(constant_idx as usize)? {
                            lit => {
                                let lit_idx = ctxt.push_literal(lit.clone());
                                match lit_idx {
                                    // maybe create a function just for translating "constant_id (usize) <-> Bytecode" that to avoid duplication
                                    0 => ctxt.push_instr(Bytecode::PushConstant0),
                                    1 => ctxt.push_instr(Bytecode::PushConstant1),
                                    2 => ctxt.push_instr(Bytecode::PushConstant2),
                                    _ => ctxt.push_instr(Bytecode::PushConstant(lit_idx as u8)),
                                }
                            }
                        };
                    }
                    Bytecode::ReturnNonLocal => {
                        // TODO: this is incomplete, but it doesn't seem to affect performance?
                        // Incomplete because Send{1|2|..} get turned to ReturnNonLocal when in 99% of cases, they should be "ReturnLocal"s
                        // and maybe some other cases that I forget
                        // But I don't observe any speedup from turning those nonlocal rets to local rets and deactivating the ONE broken benchmark, so... who cares, I guess?
                        // TODO: also if ReturnNonLocal ever gets a scope as argument (as it should) this code should be super simplifiable ("ReturnNonLocal(scope - 1), or ReturnLocal if scope is 1")
                        match ctxt.get_instructions().last()? {
                            Bytecode::Push0 | Bytecode::Push1 | Bytecode::PushNil | Bytecode::PushGlobal(_) => ctxt.push_instr(Bytecode::ReturnNonLocal),
                            Bytecode::PushLocal(up_idx, _) | Bytecode::PopLocal(up_idx, _) |
                            Bytecode::PushArgument(up_idx, _) | Bytecode::PopArgument(up_idx, _) => {
                                match up_idx {
                                    0 => ctxt.push_instr(Bytecode::ReturnLocal),
                                    _ => ctxt.push_instr(Bytecode::ReturnNonLocal)
                                }
                            },
                            Bytecode::PushField(_) | Bytecode::PopField(_) => {
                                match ctxt.current_scope() {
                                    0 => ctxt.push_instr(Bytecode::ReturnLocal),
                                    _ => ctxt.push_instr(Bytecode::ReturnNonLocal)
                                }
                            },
                            _ => {
                                ctxt.push_instr(Bytecode::ReturnNonLocal)
                            }
                        }
                    }
                    Bytecode::ReturnLocal => {}
                    // todo: hmm... do we? if so, add these to the _ case i guess.
                    // Bytecode::Jump(idx) => ctxt.push_instr(Bytecode::Jump(idx + idx_start_inlining)),
                    Bytecode::Jump(idx) => ctxt.push_instr(Bytecode::Jump(*idx)),
                    Bytecode::JumpBackward(idx) => ctxt.push_instr(Bytecode::JumpBackward(*idx)),
                    Bytecode::JumpOnTruePop(idx) => ctxt.push_instr(Bytecode::JumpOnTruePop(*idx)),
                    Bytecode::JumpOnFalsePop(idx) => {
                        ctxt.push_instr(Bytecode::JumpOnFalsePop(*idx))
                    }
                    Bytecode::JumpOnTrueTopNil(idx) => {
                        ctxt.push_instr(Bytecode::JumpOnTrueTopNil(*idx))
                    }
                    Bytecode::JumpOnFalseTopNil(idx) => {
                        ctxt.push_instr(Bytecode::JumpOnFalseTopNil(*idx))
                    }
                    Bytecode::Halt
                    | Bytecode::Dup
                    | Bytecode::Push0
                    | Bytecode::Push1
                    | Bytecode::PushNil
                    | Bytecode::Pop
                    | Bytecode::PushField(_)
                    | Bytecode::PopField(_)
                    | Bytecode::SuperSend1(_)
                    | Bytecode::SuperSend2(_)
                    | Bytecode::SuperSend3(_)
                    | Bytecode::SuperSendN(_) => {
                        ctxt.push_instr(*block_bc) // explicitly listing them out to account for the fact that new BC could be introduced and mess things up if we handled it with a _ case
                    }
                }
            }
        }

        Some(())
    }

    fn inline_last_push_block_bc(&self, ctxt: &mut dyn InnerGenCtxt) -> Option<()> {
        let block_idx = match ctxt.get_instructions().last()? {
            Bytecode::PushBlock(val) => *val,
            _ => panic!("function expects last bytecode to be a PUSH_BLOCK."),
        };
        ctxt.pop_instr(); // removing the PUSH_BLOCK

        let cond_block_ref = match ctxt.get_literal(block_idx as usize)? {
            Literal::Block(val) => val.clone(),
            _ => return None,
        };
        ctxt.remove_literal(block_idx as usize);

        match self.inline_compiled_block(ctxt, cond_block_ref.as_ref().blk_info.as_ref()) {
            None => panic!("Inlining a compiled block failed!"),
            _ => Some(()),
        }
    }

    fn adapt_block_after_outer_inlined(&self, ctxt: &mut dyn InnerGenCtxt, orig_block: &Block, adjust_scope_by: usize) -> Block {
        let mut block_literals_to_patch = vec![];
        let new_body = orig_block.blk_info.body.iter().map(|b|
            match b {
                Bytecode::PushLocal(up_idx, _) | Bytecode::PopLocal(up_idx, _) |
                Bytecode::PushArgument(up_idx, _) | Bytecode::PopArgument(up_idx, _) => {
                    let new_up_idx = match *up_idx {
                        0 => 0, // local var/arg, not affected by inlining, stays the same
                        d if d > adjust_scope_by as u8 => *up_idx - 1,
                        _ => *up_idx
                    };

                    match b {
                        Bytecode::PushLocal(_, idx) => Bytecode::PushLocal(new_up_idx, *idx),
                        Bytecode::PopLocal(_, idx) => Bytecode::PopLocal(new_up_idx, *idx),
                        Bytecode::PushArgument(_, idx) => Bytecode::PushArgument(new_up_idx, *idx),
                        Bytecode::PopArgument(_, idx) => Bytecode::PopArgument(new_up_idx, *idx),
                        _ => unreachable!()
                    }
                },
                Bytecode::PushBlock(block_idx) => {
                    let inner_lit = orig_block.blk_info.literals.get(*block_idx as usize)
                        .unwrap_or_else(|| panic!("PushBlock is associated with no literal whatsoever?"));
                    let inner_block = match inner_lit {
                        Literal::Block(inner_blk) => inner_blk,
                        _ => panic!("PushBlock is not actually pushing a block somehow")
                    };

                    let new_block = self.adapt_block_after_outer_inlined(ctxt, inner_block.clone().as_ref(), adjust_scope_by);

                    block_literals_to_patch.push((block_idx, Rc::from(new_block)));

                    Bytecode::PushBlock(*block_idx)
                },
                // Bytecode::ReturnNonLocal => Bytecode::ReturnNonLocal,
                _ => b.clone()
            }
        ).collect();

        // can't just clone the inner_block then modify the body/literals because the body is behind an Rc (not Rc<RefCell<>>), so immutable
        // though if we ever want to do some runtime bytecode rewriting, it'll have to be an Rc<RefCell<>> and this code will be refactorable (not so many individual calls to .clone())
        Block {
            frame: orig_block.frame.clone(),
            blk_info: Rc::new(BlockInfo {
                locals: orig_block.blk_info.locals.clone(),
                literals: orig_block.blk_info.literals.iter().enumerate()
                    .map(|(idx, l)| {
                        let a = block_literals_to_patch
                            .iter()
                            .find_map(|(block_idx, blk)| {(**block_idx == idx as u8).then(|| blk)});

                        if a.is_some() {
                            Literal::Block(Rc::clone(a.unwrap()))
                        } else {
                            l.clone()
                        }
                    })
                    .collect(),
                body: new_body,
                nb_params: orig_block.blk_info.nb_params,
                inline_cache: orig_block.blk_info.inline_cache.clone(),
            }),
        }
    }

    fn inline_if_true_or_if_false(
        &self,
        ctxt: &mut dyn InnerGenCtxt,
        message: &ast::Message,
        jump_type: JumpType,
    ) -> Option<()> {
        if message.values.len() != 1 || !matches!(message.values.get(0)?, ast::Expression::Block(_))
        {
            return None;
        }

        let jump_idx = ctxt.get_cur_instr_idx();
        match jump_type {
            JumpOnFalse => ctxt.push_instr(Bytecode::JumpOnFalseTopNil(0)),
            JumpOnTrue => ctxt.push_instr(Bytecode::JumpOnTrueTopNil(0)),
        }

        // we need to compile the block before inlining it, and we haven't encountered/compiled it yet
        message.values.get(0)?.codegen(ctxt)?;

        self.inline_last_push_block_bc(ctxt);

        // dbg!(ctxt.get_instructions());

        // todo i think Recurse took a big hit when i started inlining any expression instead of just blocks. needs investigating
        // wrt previous todo comment: likely super outdated. but until proven, i'm keeping it as a reminder.

        //         self.inline_expr(ctxt, message.values.get(0)?);
        ctxt.backpatch_jump_to_current(jump_idx);

        Some(())
    }

    fn inline_if_true_if_false(
        &self,
        ctxt: &mut dyn InnerGenCtxt,
        message: &ast::Message,
        jump_type: JumpType,
    ) -> Option<()> {
        if message.values.len() != 2
            || !matches!(message.values.get(0)?, ast::Expression::Block(_))
            || !matches!(message.values.get(1)?, ast::Expression::Block(_))
        {
            return None;
        }

        let start_jump_idx = ctxt.get_cur_instr_idx();
        match jump_type {
            JumpOnFalse => ctxt.push_instr(Bytecode::JumpOnFalsePop(0)),
            JumpOnTrue => ctxt.push_instr(Bytecode::JumpOnTruePop(0)),
        }

        message.values.get(0)?.codegen(ctxt)?;

        self.inline_last_push_block_bc(ctxt);
        // self.inline_compiled_block(ctxt, cond_block_ref.as_ref().blk_info.as_ref());

        let middle_jump_idx = ctxt.get_cur_instr_idx();
        ctxt.push_instr(Bytecode::Jump(0));

        ctxt.backpatch_jump_to_current(start_jump_idx);

        message.values.get(1)?.codegen(ctxt)?;

        // self.inline_expr(ctxt, message.values.get(1)?);
        // self.inline_compiled_block(ctxt, cond_block2_ref.as_ref().blk_info.as_ref());
        self.inline_last_push_block_bc(ctxt);

        ctxt.backpatch_jump_to_current(middle_jump_idx);

        Some(())
    }

    fn inline_while(
        &self,
        ctxt: &mut dyn InnerGenCtxt,
        message: &ast::Message,
        jump_type: JumpType,
    ) -> Option<()> {
        if message.values.len() != 1
            || !matches!(message.values.get(0)?, ast::Expression::Block(_))
            || !matches!(ctxt.get_instructions().last(), Some(Bytecode::PushBlock(_)))
        {
            return None;
        }

        let idx_before_condition = ctxt.get_cur_instr_idx();

        self.inline_last_push_block_bc(ctxt);

        let cond_jump_idx = ctxt.get_cur_instr_idx();
        match jump_type {
            JumpOnFalse => ctxt.push_instr(Bytecode::JumpOnFalsePop(0)),
            JumpOnTrue => ctxt.push_instr(Bytecode::JumpOnTruePop(0)),
        }

        message.values.get(0)?.codegen(ctxt)?;
        self.inline_last_push_block_bc(ctxt);

        // we push a POP, unless the body of the loop is empty.
        match message.values.get(0).unwrap() {
            ast::Expression::Block(block) => {
                if block.body.exprs.len() != 0 {
                    ctxt.push_instr(Bytecode::Pop);
                }
            }
            _ => {}
        };

        ctxt.push_instr(Bytecode::JumpBackward(
            ctxt.get_cur_instr_idx() - idx_before_condition + 1,
        ));
        ctxt.backpatch_jump_to_current(cond_jump_idx);

        // that's a PushNil with the specialized bytecode, which is prettier.
        let name = ctxt.intern_symbol("nil");
        let idx = ctxt.push_literal(Literal::Symbol(name));
        ctxt.push_instr(Bytecode::PushGlobal(idx as u8));

        Some(())
    }

    fn inline_or_and(
        &self,
        ctxt: &mut dyn InnerGenCtxt,
        message: &ast::Message,
        or_and_choice: OrAndChoice,
    ) -> Option<()> {
        if message.values.len() != 1 || !matches!(message.values.get(0)?, ast::Expression::Block(_))
        {
            return None;
        }

        let skip_cond_jump_idx = ctxt.get_cur_instr_idx();

        match or_and_choice {
            Or => ctxt.push_instr(Bytecode::JumpOnTruePop(0)),
            And => ctxt.push_instr(Bytecode::JumpOnFalsePop(0)),
        }

        message.values.get(0)?.codegen(ctxt)?;
        self.inline_last_push_block_bc(ctxt);

        let skip_return_true_idx = ctxt.get_cur_instr_idx();
        ctxt.push_instr(Bytecode::Jump(0));

        ctxt.backpatch_jump_to_current(skip_cond_jump_idx);

        let name = match or_and_choice {
            Or => ctxt.intern_symbol("true"),
            And => ctxt.intern_symbol("false"),
        };
        let idx = ctxt.push_literal(Literal::Symbol(name));
        ctxt.push_instr(Bytecode::PushGlobal(idx as u8));

        ctxt.backpatch_jump_to_current(skip_return_true_idx);

        Some(())
    }
}
