use std::rc::Rc;
use som_core::ast;
use som_core::bytecode::Bytecode;
use crate::block::Block;
use crate::compiler::{compile_block, InnerGenCtxt, Literal};
use crate::compiler::MethodCodegen;

// TODO some of those should return Result types and throw errors instead, most likely.
pub trait PrimMessageInliner {
    fn inline_if_possible(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()>;
    fn inline_block_expr(&self, ctxt: &mut dyn InnerGenCtxt, block: &ast::Expression) -> Option<()>;
    fn inline_compiled_block(&self, ctxt: &mut dyn InnerGenCtxt, block: &Block) -> Option<()>;
    fn patch_inner_block_during_inlining(&self, ctxt: &mut dyn InnerGenCtxt, block: &Block) -> Block;

    fn inline_if_true_or_if_false(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()>;
    fn inline_if_true_if_false(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()>;
    fn inline_while(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()>;
}

impl PrimMessageInliner for ast::Expression {
    fn inline_if_possible(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()> {
        match message.signature.as_str() {
            "ifTrue:" | "ifFalse:" => self.inline_if_true_or_if_false(ctxt, message),
            "ifTrue:ifFalse:" | "ifFalse:ifTrue:" => self.inline_if_true_if_false(ctxt, message),
            "whileTrue:" | "whileFalse:" => self.inline_while(ctxt, message),
            // TODO: [or, and]
            _ => None
        }
    }

    fn inline_block_expr(&self, ctxt: &mut dyn InnerGenCtxt, block_expr: &ast::Expression) -> Option<()> {
        match block_expr {
            ast::Expression::Block(block) => {
                for block_local in &block.locals {
                    ctxt.push_local(String::from(block_local)); // breaks shadowing
                }

                // TODO i suspect we can reuse the other inline function (inlines a compiled block) when it's done, since turning a block expr into a block is trivial.
                // TODO also, need remove those POPs somehow.
                if let Some((last, rest)) = block.body.exprs.split_last() {
                    for expr in rest {
                        expr.codegen(ctxt);
                        ctxt.push_instr(Bytecode::Pop);
                    }
                    last.codegen(ctxt)?;
                }
                Some(())
            },
            _ => panic!("Expression was not a block")
        }
    }

    fn inline_compiled_block(&self, ctxt: &mut dyn InnerGenCtxt, block: &Block) -> Option<()> {
        for block_local in &block.locals {
            dbg!(block_local);
            todo!("actually pushing locals would be nice!")
            // ctxt.push_local(String::from(block_local));
        }

        // last is always ReturnLocal, so it gets ignored
        if let Some((_, body)) = block.body.split_last() {
            for block_bc in body {
                match block_bc {
                    Bytecode::PushLocal(up_idx, idx) => ctxt.push_instr(Bytecode::PushLocal(*up_idx - 1, *idx)),
                    Bytecode::PopLocal(up_idx, idx) => ctxt.push_instr(Bytecode::PopLocal(*up_idx - 1, *idx)),
                    Bytecode::PushArgument(up_idx, idx) => ctxt.push_instr(Bytecode::PushArgument(*up_idx - 1, *idx)),
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
                                let new_block = self.patch_inner_block_during_inlining(ctxt, inner_block.as_ref());
                                let idx = ctxt.push_literal(Literal::Block(Rc::new(new_block)));
                                ctxt.push_instr(Bytecode::PushBlock(idx as u8));
                            },
                            _ => panic!("PushBlock not actually pushing a block somehow")
                        };
                    },
                    _ => ctxt.push_instr(*block_bc)
                }
            }
        }

        Some(())
    }

    fn patch_inner_block_during_inlining(&self, ctxt: &mut dyn InnerGenCtxt, block: &Block) -> Block {
        compile_block(ctxt.as_gen_ctxt(), &block.ast_body).unwrap() // ...is it really that simple?
    }

    fn inline_if_true_or_if_false(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()> {
        // TODO we can inline more than blocks if we rely on the existing codegen methods. However, that's a pain for some reason.
        if message.values.len() != 1 || !matches!(message.values.get(0)?, ast::Expression::Block(_)) {
            return None;
        }

        let is_if_true = message.signature == "ifTrue:";

        let jump_idx = ctxt.get_instr_idx();

        match is_if_true {
            true => ctxt.push_instr(Bytecode::JumpOnFalseTopNil(0)),
            false => ctxt.push_instr(Bytecode::JumpOnTrueTopNil(0))
        }

        self.inline_block_expr(ctxt, message.values.get(0)?);

        let jump_by = ctxt.get_instr_idx() - jump_idx;
        match is_if_true {
            true => ctxt.backpatch(jump_idx, Bytecode::JumpOnFalseTopNil(jump_by)),
            false => ctxt.backpatch(jump_idx, Bytecode::JumpOnTrueTopNil(jump_by)),
        }

        return Some(());
    }

    fn inline_if_true_if_false(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()> {
        if message.values.len() != 2
            || !matches!(message.values.get(0)?, ast::Expression::Block(_))
            || !matches!(message.values.get(1)?, ast::Expression::Block(_)) {
            return None;
        }

        let is_if_true_if_false = message.signature == "ifTrue:ifFalse:";

        let start_jump_idx = ctxt.get_instr_idx();
        match is_if_true_if_false {
            true => ctxt.push_instr(Bytecode::JumpOnFalsePop(0)),
            false => ctxt.push_instr(Bytecode::JumpOnTruePop(0)),
        }

        self.inline_block_expr(ctxt, message.values.get(0)?);

        let middle_jump_idx = ctxt.get_instr_idx();
        ctxt.push_instr(Bytecode::Jump(0));

        let jump_by = ctxt.get_instr_idx() - start_jump_idx;
        match is_if_true_if_false {
            true => ctxt.backpatch(start_jump_idx, Bytecode::JumpOnFalsePop(jump_by)),
            false => ctxt.backpatch(start_jump_idx, Bytecode::JumpOnTruePop(jump_by)),
        }

        self.inline_block_expr(ctxt, message.values.get(1)?);

        let jump_by = ctxt.get_instr_idx() - middle_jump_idx;
        ctxt.backpatch(middle_jump_idx, Bytecode::Jump(jump_by));

        return Some(());
    }

    fn inline_while(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()> {
        let block_idx = match ctxt.get_instructions().last()? {
            Bytecode::PushBlock(val) => val,
            _ => return None
        };

        // todo pop the literal
        let block_ref = match ctxt.get_literal(*block_idx as usize)? {
            Literal::Block(val) => val.clone(),
            _ => return None
        };

        if message.values.len() != 1
            || !matches!(message.values.get(0)?, ast::Expression::Block(_)) {
            return None;
        }

        ctxt.pop_instr(); // we remove the PUSH_BLOCK

        let is_while_true = message.signature == "whileTrue:";

        let cond_idx = ctxt.get_instr_idx();

        self.inline_compiled_block(ctxt, block_ref.as_ref());

        let loop_start_idx = ctxt.get_instr_idx();

        match is_while_true {
            true => ctxt.push_instr(Bytecode::JumpOnFalsePop(0)),
            false => ctxt.push_instr(Bytecode::JumpOnTruePop(0))
        }

        self.inline_block_expr(ctxt, message.values.get(0).unwrap());

        ctxt.push_instr(Bytecode::Pop);

        let jump_to_cond_val = ctxt.get_instr_idx() - cond_idx;
        ctxt.push_instr(Bytecode::JumpBackward(jump_to_cond_val));

        let loop_jump_by = ctxt.get_instr_idx() - loop_start_idx;

        match is_while_true {
            true => ctxt.backpatch(loop_start_idx, Bytecode::JumpOnFalsePop(loop_jump_by)),
            false => ctxt.backpatch(loop_start_idx, Bytecode::JumpOnTruePop(loop_jump_by))
        }

        ctxt.push_instr(Bytecode::PushNil);

        // println!("BYTECODES:");
        // for instr in ctxt.get_instructions() {
        //     println!("{}", instr);
        // }
        // println!();

        return Some(());
    }
}