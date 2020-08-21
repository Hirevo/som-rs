use std::fmt;
use std::rc::Rc;

use som_core::bytecode::Bytecode;

use crate::class::Class;
use crate::compiler::Literal;
use crate::frame::Frame;
use crate::universe::Universe;
use crate::value::Value;
use crate::SOMRef;

#[derive(Clone)]
pub struct BlockInfo {
    pub locals: Vec<Value>,
    pub literals: Vec<Literal>,
    pub body: Vec<Bytecode>,
    pub nb_params: usize,
}

/// Represents an executable block.
#[derive(Clone)]
pub struct Block {
    /// Reference to the captured stack frame.
    pub frame: Option<SOMRef<Frame>>,
    pub blk_info: Rc<BlockInfo>,
}

impl Block {
    /// Get the block's class.
    pub fn class(&self, universe: &Universe) -> SOMRef<Class> {
        match self.nb_parameters() {
            0 => universe.block1_class(),
            1 => universe.block2_class(),
            2 => universe.block3_class(),
            _ => panic!("no support for blocks with more than 2 paramters"),
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
