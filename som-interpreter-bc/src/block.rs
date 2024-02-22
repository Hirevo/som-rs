use som_core::ast;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use som_core::bytecode::Bytecode;

use crate::class::Class;
use crate::compiler::Literal;
use crate::frame::Frame;
use crate::interner::Interned;
use crate::method::Method;
use crate::universe::Universe;
use crate::SOMRef;

#[derive(Clone)]
pub struct BlockInfo {
    pub locals: Vec<Interned>,
    pub literals: Vec<Literal>,
    pub body: Vec<Bytecode>,
    pub nb_params: usize,
    pub inline_cache: RefCell<Vec<Option<(*const Class, Rc<Method>)>>>,
}

/// Represents an executable block.
#[derive(Clone)]
pub struct Block {
    /// Reference to the captured stack frame.
    pub frame: Option<SOMRef<Frame>>,
    pub blk_info: Rc<BlockInfo>,
    // OLarose: not a fan... but it's needed when inlining to be able to recreate a working version of the block from the original AST
    // (see PushBlock in inliner)
    pub ast_body: ast::Block,
}

impl Block {
    /// Get the block's class.
    pub fn class(&self, universe: &Universe) -> SOMRef<Class> {
        match self.nb_parameters() {
            0 => universe.block1_class(),
            1 => universe.block2_class(),
            2 => universe.block3_class(),
            _ => panic!("no support for blocks with more than 2 parameters"),
        }
    }

    /// Retrieve the number of parameters this block accepts.
    pub fn nb_parameters(&self) -> usize {
        self.blk_info.nb_params
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(&format!("Block{}", self.nb_parameters() + 1))
            .finish()
    }
}
