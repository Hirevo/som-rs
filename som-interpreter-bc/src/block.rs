use std::cell::RefCell;
use std::fmt;

use som_core::bytecode::Bytecode;
use som_gc::{Gc, Trace};

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
    pub inline_cache: RefCell<Vec<Option<(*const Class, Gc<Method>)>>>,
}

/// Represents an executable block.
#[derive(Clone)]
pub struct Block {
    /// Reference to the captured stack frame.
    pub frame: Option<SOMRef<Frame>>,
    pub blk_info: Gc<BlockInfo>,
}

impl Trace for BlockInfo {
    #[inline]
    fn trace(&self) {
        self.locals.trace();
        self.literals.trace();
        self.inline_cache.trace();
    }
}

impl Trace for Block {
    #[inline]
    fn trace(&self) {
        self.frame.trace();
        self.blk_info.trace();
    }
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
