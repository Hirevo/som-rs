use std::rc::Rc;
use rand::Rng;
use som_core::ast;
use som_core::bytecode::Bytecode;
use crate::block::{BlockInfo};
use crate::compiler::{compile_block, InnerGenCtxt, Literal};
use crate::compiler::MethodCodegen;
use crate::inliner::JumpType::{JumpOnFalse, JumpOnTrue};

pub enum JumpType {
    JumpOnFalse,
    JumpOnTrue
}

// TODO some of those should return Result types and throw errors instead, most likely.
pub trait PrimMessageInliner {
    fn inline_if_possible(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()>;
    fn inline_compiled_block(&self, ctxt: &mut dyn InnerGenCtxt, block: &BlockInfo) -> Option<()>;
    fn inline_last_push_block_bc(&self, ctxt: &mut dyn InnerGenCtxt) -> Option<()>;
    fn inline_if_true_or_if_false(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message, jump_type: JumpType) -> Option<()>;
    fn inline_if_true_if_false(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message, jump_type: JumpType) -> Option<()>;
    fn inline_while(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message, jump_type: JumpType) -> Option<()>;
}

impl PrimMessageInliner for ast::Expression {
    fn inline_if_possible(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()> {
        match message.signature.as_str() {
            "ifTrue:" => self.inline_if_true_or_if_false(ctxt, message, JumpOnFalse),
            "ifFalse:" => self.inline_if_true_or_if_false(ctxt, message, JumpOnTrue),
            "ifTrue:ifFalse:" => self.inline_if_true_if_false(ctxt, message, JumpOnFalse),
            "ifFalse:ifTrue:" => self.inline_if_true_if_false(ctxt, message, JumpOnTrue),
            "whileTrue:" => self.inline_while(ctxt, message, JumpOnFalse),
            "whileFalse:" => self.inline_while(ctxt, message, JumpOnTrue),
            // TODO: [or, and]
            _ => None
        }
    }

    fn inline_compiled_block(&self, ctxt: &mut dyn InnerGenCtxt, block: &BlockInfo) -> Option<()> {
        let nbr_locals_pre_inlining = ctxt.get_nbr_locals();

        let mut rand_thread = rand::thread_rng();
        let og_scope = rand_thread.gen(); // does this matter? should it be the exact same as the original compiled block? i'm thinking it's fine like this?
        for block_local_intern_id in &block.locals {
            let symbol_str = ctxt.lookup_symbol(*block_local_intern_id);
            // ctxt.push_local(String::from(symbol_str), ctxt.current_scope() + 1);
            ctxt.push_local(String::from(symbol_str), og_scope);
        }

        // dbg!(&block.body);

        // let idx_start_inlining = ctxt.get_cur_instr_idx();

        // last is always ReturnLocal, so it gets ignored
        if let Some((_, body)) = block.body.split_last() {
            for block_bc in body {
                match block_bc {
                    Bytecode::PushLocal(up_idx, idx) => {
                        match up_idx {
                            0 => ctxt.push_instr(Bytecode::PushLocal(*up_idx, nbr_locals_pre_inlining as u8 + *idx)),
                            1.. => ctxt.push_instr(Bytecode::PushLocal(*up_idx - 1, *idx))
                        }
                    },
                    Bytecode::PopLocal(up_idx, idx) => {
                        match up_idx {
                            0 => ctxt.push_instr(Bytecode::PopLocal(*up_idx, nbr_locals_pre_inlining as u8 + *idx)),
                            1.. => ctxt.push_instr(Bytecode::PopLocal(*up_idx - 1, *idx))
                        }
                    },
                    Bytecode::PushArgument(up_idx, idx) => ctxt.push_instr(Bytecode::PushArgument(*up_idx - 1, *idx)), // not 100% sure i need to adjust the up_idx there and for pop
                    Bytecode::PopArgument(up_idx, idx) => ctxt.push_instr(Bytecode::PopArgument(*up_idx - 1, *idx)),
                    Bytecode::Send(lit_idx) => {
                        match block.literals.get(*lit_idx as usize)? {
                            Literal::Symbol(interned) => {
                                // does this push duplicate literals? I think it doesn't?
                                let idx = ctxt.push_literal(Literal::Symbol(*interned));
                                ctxt.push_instr(Bytecode::Send(idx as u8));
                            },
                            _ => panic!("Unexpected block literal type, not yet implemented")
                        }
                    },
                    Bytecode::PushBlock(block_idx) => {
                        match block.literals.get(*block_idx as usize)? {
                            Literal::Block(inner_block) => {
                                // dbg!(&inner_block.ast_body);
                                // dbg!(&inner_block.blk_info.body);
                                let new_block = compile_block(ctxt.as_gen_ctxt(), &inner_block.ast_body)?;
                                let idx = ctxt.push_literal(Literal::Block(Rc::from(new_block)));
                                // dbg!(idx);
                                ctxt.push_instr(Bytecode::PushBlock(idx as u8));
                            },
                            _ => panic!("PushBlock not actually pushing a block somehow")
                        };
                    },
                    Bytecode::PushGlobal(global_idx) => {
                        match block.literals.get(*global_idx as usize)? {
                            lit => {
                                let lit_idx = ctxt.push_literal(lit.clone());
                                ctxt.push_instr(Bytecode::PushGlobal(lit_idx as u8));
                            }
                        };
                    },
                    Bytecode::PushConstant(constant_idx) => {
                        match block.literals.get(*constant_idx as usize)? {
                            lit => {
                                let lit_idx = ctxt.push_literal(lit.clone());
                                ctxt.push_instr(Bytecode::PushConstant(lit_idx as u8));
                            }
                        };
                    },
                    Bytecode::ReturnNonLocal => {
                        // TODO; if the new context level is 0 (check prev bytecode emitted?), gotta emit a RETURNLOCAL instead!
                        // as far as i understand... this still works? and is just slower? TODO fix though obviously
                        // dbg!("wow");
                        // dbg!(&ctxt.get_instructions().last());
                        // match ctxt.get_instructions().last().unwrap() {
                        //     Bytecode::PushGlobal(_) => ctxt.push_instr(Bytecode::ReturnLocal),
                        //     _ => ctxt.push_instr(Bytecode::ReturnNonLocal)
                        // }
                        ctxt.push_instr(Bytecode::ReturnNonLocal)
                    },
                    Bytecode::ReturnLocal => {}, //panic!("Is that a thing? If so, just ignore it."),
                    // todo: hmm... do we? if so, add these to the _ case i guess.
                    // Bytecode::Jump(idx) => ctxt.push_instr(Bytecode::Jump(idx + idx_start_inlining)),
                    Bytecode::Jump(idx) => ctxt.push_instr(Bytecode::Jump(*idx)),
                    Bytecode::JumpBackward(idx) => ctxt.push_instr(Bytecode::JumpBackward(*idx)),
                    Bytecode::JumpOnTruePop(idx) => ctxt.push_instr(Bytecode::JumpOnTruePop(*idx)),
                    Bytecode::JumpOnFalsePop(idx) => ctxt.push_instr(Bytecode::JumpOnFalsePop(*idx)),
                    Bytecode::JumpOnTrueTopNil(idx) => ctxt.push_instr(Bytecode::JumpOnTrueTopNil(*idx)),
                    Bytecode::JumpOnFalseTopNil(idx) => ctxt.push_instr(Bytecode::JumpOnFalseTopNil(*idx)),
                    _ => ctxt.push_instr(*block_bc) // I *think* the rest are all fine..
                }
            }
        }

        Some(())
    }

    fn inline_last_push_block_bc(&self, ctxt: &mut dyn InnerGenCtxt) -> Option<()> {
        let block_idx = match ctxt.get_instructions().last()? {
            Bytecode::PushBlock(val) => *val,
            _ => panic!("function expects last bytecode to be a PUSH_BLOCK.")
        };
        ctxt.pop_instr(); // removing the PUSH_BLOCK

        let cond_block_ref = match ctxt.get_literal(block_idx as usize)? {
            Literal::Block(val) => val.clone(),
            _ => return None
        };
        ctxt.remove_literal(block_idx as usize);

        match self.inline_compiled_block(ctxt, cond_block_ref.as_ref().blk_info.as_ref()) {
            None => panic!("Inlining a compiled block failed!"),
            _ => Some(())
        }
    }

    fn inline_if_true_or_if_false(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message, jump_type: JumpType) -> Option<()> {
        if message.values.len() != 1 || !matches!(message.values.get(0)?, ast::Expression::Block(_)) {
            return None;
        }

        let jump_idx = ctxt.get_cur_instr_idx();
        match jump_type {
            JumpOnFalse => ctxt.push_instr(Bytecode::JumpOnFalseTopNil(0)),
            JumpOnTrue => ctxt.push_instr(Bytecode::JumpOnTrueTopNil(0))
        }

        // we need to compile the block before inlining it, and we haven't encountered/compiled it yet
        message.values.get(0)?.codegen(ctxt)?;

        self.inline_last_push_block_bc(ctxt);

        // dbg!(ctxt.get_instructions());

        // todo i think Recurse took a big hit when i started inlining any expression instead of just blocks. needs investigating
        // wrt previous todo comment: likely super outdated. but until proven, i'm keeping it as a reminder.

//         self.inline_expr(ctxt, message.values.get(0)?);
        ctxt.backpatch_jump_to_current(jump_idx);

        return Some(());
    }

    fn inline_if_true_if_false(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message, jump_type: JumpType) -> Option<()> {
        if message.values.len() != 2
            || !matches!(message.values.get(0)?, ast::Expression::Block(_))
            || !matches!(message.values.get(1)?, ast::Expression::Block(_)) {
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

        return Some(());
    }

    fn inline_while(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message, jump_type: JumpType) -> Option<()> {
        if message.values.len() != 1 || !matches!(message.values.get(0)?, ast::Expression::Block(_)) || !matches!(ctxt.get_instructions().last() , Some(Bytecode::PushBlock(_))) {
            return None;
        }

        let idx_before_condition = ctxt.get_cur_instr_idx();

        self.inline_last_push_block_bc(ctxt);

        let cond_jump_idx = ctxt.get_cur_instr_idx();
        match jump_type {
            JumpOnFalse => ctxt.push_instr(Bytecode::JumpOnFalsePop(0)),
            JumpOnTrue => ctxt.push_instr(Bytecode::JumpOnTruePop(0))
        }

        message.values.get(0)?.codegen(ctxt)?;
        self.inline_last_push_block_bc(ctxt);

        // we push a POP, unless the body of the loop is empty.
        match message.values.get(0).unwrap() {
            ast::Expression::Block(block)  => {
                if block.body.exprs.len() != 0 {
                    ctxt.push_instr(Bytecode::Pop);
                }
            },
            _ => {}
        };

        ctxt.push_instr(Bytecode::JumpBackward(ctxt.get_cur_instr_idx() - idx_before_condition + 1));
        ctxt.backpatch_jump_to_current(cond_jump_idx);

        // that's a PushNil with the specialized bytecode, which is prettier.
        let name = ctxt.intern_symbol("nil");
        let idx = ctxt.push_literal(Literal::Symbol(name));
        ctxt.push_instr(Bytecode::PushGlobal(idx as u8));

        return Some(());
    }
}