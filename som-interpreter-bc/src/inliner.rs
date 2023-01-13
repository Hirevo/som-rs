use som_core::ast;
use som_core::bytecode::Bytecode;
use crate::compiler::{InnerGenCtxt};
use crate::compiler::MethodCodegen;
use crate::inliner::JumpType::{JumpOnFalse, JumpOnTrue};

pub enum JumpType {
    JumpOnFalse,
    JumpOnTrue
}

// TODO some of those should return Result types and throw errors instead, most likely.
pub trait PrimMessageInliner {
    fn inline_if_possible(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()>;
    fn inline_expr(&self, ctxt: &mut dyn InnerGenCtxt, block: &ast::Expression) -> Option<()>;

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

    fn inline_expr(&self, ctxt: &mut dyn InnerGenCtxt, block_expr: &ast::Expression) -> Option<()> {
        match block_expr {
            ast::Expression::Block(block) => {
                for block_local in &block.locals {
                    ctxt.push_local(String::from(block_local)); // breaks shadowing
                }

                // TODO need to remove those POPs somehow.
                if let Some((last, rest)) = block.body.exprs.split_last() {
                    for expr in rest {
                        expr.codegen(ctxt);
                        ctxt.push_instr(Bytecode::Pop);
                    }
                    last.codegen(ctxt)?;
                }
                Some(())
            },
            expr => expr.codegen(ctxt)
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

        // todo i think Recurse took a big hit when i started inlining any expression instead of just blocks. needs investigating
        self.inline_expr(ctxt, message.values.get(0)?);
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

        self.inline_expr(ctxt, message.values.get(0)?);

        let middle_jump_idx = ctxt.get_cur_instr_idx();
        ctxt.push_instr(Bytecode::Jump(0));

        ctxt.backpatch_jump_to_current(start_jump_idx);
        self.inline_expr(ctxt, message.values.get(1)?);
        ctxt.backpatch_jump_to_current(middle_jump_idx);

        return Some(());
    }

    fn inline_while(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message, jump_type: JumpType) -> Option<()> {
        if message.values.len() != 1  {
            return None;
        }

        if message.values.len() != 1 || !matches!(message.values.get(0)?, ast::Expression::Block(_)) {
            return None;
        }

        ctxt.pop_instr(); // we remove the PUSH_BLOCK

        let idx_before_condition = ctxt.get_cur_instr_idx();

        self.inline_expr(ctxt, message.receiver.as_ref());

        let cond_jump_idx = ctxt.get_cur_instr_idx();
        match jump_type {
            JumpOnFalse => ctxt.push_instr(Bytecode::JumpOnFalsePop(0)),
            JumpOnTrue => ctxt.push_instr(Bytecode::JumpOnTruePop(0))
        }

        self.inline_expr(ctxt, message.values.get(0).unwrap());

        // we push a POP, unless the body of the loop is empty.
        match message.values.get(0).unwrap() {
            ast::Expression::Block(block)  => {
                if block.body.exprs.len() != 0 {
                    ctxt.push_instr(Bytecode::Pop);
                }
            },
            _ => {}
        };

        ctxt.push_instr(Bytecode::JumpBackward(ctxt.get_cur_instr_idx() - idx_before_condition));
        ctxt.backpatch_jump_to_current(cond_jump_idx);
        ctxt.push_instr(Bytecode::PushNil);

        return Some(());
    }
}