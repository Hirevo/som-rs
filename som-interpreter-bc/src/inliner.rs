use std::rc::Rc;
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
    // fn inline_expr(&self, ctxt: &mut dyn InnerGenCtxt, block: &ast::Expression) -> Option<()>;

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
            // "ifFalse:ifTrue:" => self.inline_if_true_if_false(ctxt, message, JumpOnTrue),
            // "whileTrue:" => self.inline_while(ctxt, message, JumpOnFalse),
            // "whileFalse:" => self.inline_while(ctxt, message, JumpOnTrue),
            // TODO: [or, and]
            _ => None
        }
    }

    fn inline_compiled_block(&self, ctxt: &mut dyn InnerGenCtxt, block: &BlockInfo) -> Option<()> {
        for _block_local_intern_id in &block.locals {
            panic!("we don't handle block locals yet!");
            // ctxt.push_local(ctxt.lookup_symbol(block_local_intern_id));
        }

        let idx_start_inlining = ctxt.get_cur_instr_idx();

        // last is always ReturnLocal, so it gets ignored
        if let Some((_, body)) = block.body.split_last() {
            for block_bc in body {
                match block_bc {
                    Bytecode::PushLocal(up_idx, idx) => {
                        match up_idx { // todo: is there more logic to put there?
                            0 => ctxt.push_instr(Bytecode::PushLocal(*up_idx, *idx)),
                            1.. => ctxt.push_instr(Bytecode::PushLocal(*up_idx - 1, *idx))
                        }
                    },
                    Bytecode::PopLocal(up_idx, idx) => {
                        match up_idx {
                            0 => ctxt.push_instr(Bytecode::PopLocal(*up_idx, *idx)),
                            1.. => ctxt.push_instr(Bytecode::PopLocal(*up_idx - 1, *idx))
                        }
                    },
                    Bytecode::PushArgument(up_idx, idx) => ctxt.push_instr(Bytecode::PushArgument(*up_idx - 1, *idx)), // not 100% sure i need to adjust the up_idx there and for pop
                    Bytecode::PopArgument(up_idx, idx) => ctxt.push_instr(Bytecode::PopArgument(*up_idx - 1, *idx)),
                    Bytecode::Send1(lit_idx) | Bytecode::Send2(lit_idx) |
                    Bytecode::Send3(lit_idx) | Bytecode::SendN(lit_idx) => {
                        match block.literals.get(*lit_idx as usize)? {
                            Literal::Symbol(interned) => {
                                // does this push duplicate literals? I think it doesn't?
                                let idx = ctxt.push_literal(Literal::Symbol(*interned));
                                match block_bc {
                                    Bytecode::Send1(_) => ctxt.push_instr(Bytecode::Send1(idx as u8)),
                                    Bytecode::Send2(_) => ctxt.push_instr(Bytecode::Send2(idx as u8)),
                                    Bytecode::Send3(_) => ctxt.push_instr(Bytecode::Send3(idx as u8)),
                                    Bytecode::SendN(_) => ctxt.push_instr(Bytecode::SendN(idx as u8)),
                                    _ => panic!("Unreachable branch")
                                }
                            },
                            _ => panic!("Unexpected block literal type, not yet implemented")
                        }
                    },
                    Bytecode::PushBlock(block_idx) => {
                        match block.literals.get(*block_idx as usize)? {
                            Literal::Block(inner_block) => {
                                let new_block = compile_block(ctxt.as_gen_ctxt(), &inner_block.ast_body)?;
                                let idx = ctxt.push_literal(Literal::Block(Rc::from(new_block)));
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
                                match lit_idx {
                                    0 => ctxt.push_instr(Bytecode::PushConstant0),
                                    1 => ctxt.push_instr(Bytecode::PushConstant1),
                                    2 => ctxt.push_instr(Bytecode::PushConstant2),
                                    _ => ctxt.push_instr(Bytecode::PushConstant(lit_idx as u8))
                                }
                            }
                        };
                    },
                    Bytecode::PushConstant0 | Bytecode::PushConstant1 | Bytecode::PushConstant2 => {
                        let constant_idx: usize = match block_bc {
                            Bytecode::PushConstant0 => 0,
                            Bytecode::PushConstant1 => 1,
                            Bytecode::PushConstant2 => 2,
                            _ => panic!("Unreachable")
                        };

                        match block.literals.get(constant_idx)? {
                            lit => {
                                let lit_idx = ctxt.push_literal(lit.clone());
                                match lit_idx {
                                    0 => ctxt.push_instr(Bytecode::PushConstant0),
                                    1 => ctxt.push_instr(Bytecode::PushConstant1),
                                    2 => ctxt.push_instr(Bytecode::PushConstant2),
                                    _ => ctxt.push_instr(Bytecode::PushConstant(lit_idx as u8))
                                }
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
                    Bytecode::ReturnLocal => {},//panic!("Is that a thing? If so, just ignore it."),
                    // For jumps, we just need to adjust their offsets based on when we started inlining the block. probably.
                    Bytecode::Jump(idx) => ctxt.push_instr(Bytecode::Jump(idx + idx_start_inlining)),
                    Bytecode::JumpBackward(idx) => ctxt.push_instr(Bytecode::JumpBackward(idx + idx_start_inlining)),
                    Bytecode::JumpOnTruePop(idx) => ctxt.push_instr(Bytecode::JumpOnTruePop(idx + idx_start_inlining)),
                    Bytecode::JumpOnFalsePop(idx) => ctxt.push_instr(Bytecode::JumpOnFalsePop(idx + idx_start_inlining)),
                    Bytecode::JumpOnTrueTopNil(idx) => ctxt.push_instr(Bytecode::JumpOnTrueTopNil(idx + idx_start_inlining)),
                    Bytecode::JumpOnFalseTopNil(idx) => ctxt.push_instr(Bytecode::JumpOnFalseTopNil(idx + idx_start_inlining)),
                    _ => ctxt.push_instr(*block_bc) // I *think* the rest are all fine..
                }
            }
        }

        Some(())
    }

    fn inline_last_push_block_bc(&self, ctxt: &mut dyn InnerGenCtxt) -> Option<()> {
        let block1_idx = match ctxt.get_instructions().last()? {
            Bytecode::PushBlock(val) => *val,
            _ => panic!("function expects last bytecode to be a block.")
        };
        ctxt.pop_instr(); // removing the PUSH_BLOCK

        let cond_block_ref = match ctxt.get_literal(block1_idx as usize)? {
            Literal::Block(val) => val.clone(),
            _ => return None
        };
        // shouldn't break anything, probably
        // ctxt.remove_literal(block_idx as usize);

        self.inline_compiled_block(ctxt, cond_block_ref.as_ref().blk_info.as_ref())
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

    fn inline_while(&self, _ctxt: &mut dyn InnerGenCtxt, _message: &ast::Message, _jump_type: JumpType) -> Option<()> {
        todo!("make it use the new inlining function");
        // if message.values.len() != 1  {
        //     return None;
        // }
        //
        // if message.values.len() != 1 || !matches!(message.values.get(0)?, ast::Expression::Block(_)) {
        //     return None;
        // }
        //
        // ctxt.pop_instr(); // we remove the PUSH_BLOCK
        //
        // let idx_before_condition = ctxt.get_cur_instr_idx();
        //
        // self.inline_expr(ctxt, message.receiver.as_ref());
        //
        // let cond_jump_idx = ctxt.get_cur_instr_idx();
        // match jump_type {
        //     JumpOnFalse => ctxt.push_instr(Bytecode::JumpOnFalsePop(0)),
        //     JumpOnTrue => ctxt.push_instr(Bytecode::JumpOnTruePop(0))
        // }
        //
        // self.inline_expr(ctxt, message.values.get(0).unwrap());
        //
        // // we push a POP, unless the body of the loop is empty.
        // match message.values.get(0).unwrap() {
        //     ast::Expression::Block(block)  => {
        //         if block.body.exprs.len() != 0 {
        //             ctxt.push_instr(Bytecode::Pop);
        //         }
        //     },
        //     _ => {}
        // };
        //
        // ctxt.push_instr(Bytecode::JumpBackward(ctxt.get_cur_instr_idx() - idx_before_condition));
        // ctxt.backpatch_jump_to_current(cond_jump_idx);
        // ctxt.push_instr(Bytecode::PushNil);
        //
        // return Some(());
    }
}