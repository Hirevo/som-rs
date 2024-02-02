use std::fmt;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Bytecode {
    Halt,
    Dup,
    PushLocal(u8, u8),
    PushArgument(u8, u8),
    PushField(u8),
    PushBlock(u8),
    PushConstant(u8),
    PushGlobal(u8),
    Pop,
    PopLocal(u8, u8),
    PopArgument(u8, u8),
    PopField(u8),
    Send(u8),
    SuperSend(u8),
    ReturnLocal,
    ReturnNonLocal,
    Jump(usize),
    JumpBackward(usize),
    JumpOnTrueTopNil(usize),
    JumpOnFalseTopNil(usize),
    JumpOnTruePop(usize),
    JumpOnFalsePop(usize)
}

impl Bytecode {
    /// Get the instruction's name.
    #[rustfmt::skip]
    pub fn name(self) -> &'static str {
        // NAMES[self as usize]
        match self {
            Self::Halt                     => "HALT",
            Self::Dup                      => "DUP",
            Self::PushLocal(_, _)          => "PUSH_LOCAL",
            Self::PushArgument(_, _)       => "PUSH_ARGUMENT",
            Self::PushField(_)             => "PUSH_FIELD",
            Self::PushBlock(_)             => "PUSH_BLOCK",
            Self::PushConstant(_)          => "PUSH_CONSTANT",
            Self::PushGlobal(_)            => "PUSH_GLOBAL",
            Self::Pop                      => "POP",
            Self::PopLocal(_, _)           => "POP_LOCAL",
            Self::PopArgument(_, _)        => "POP_ARGUMENT",
            Self::PopField(_)              => "POP_FIELD",
            Self::Send(_)                 => "SEND",
            Self::SuperSend(_)            => "SUPER_SEND",
            Self::ReturnLocal              => "RETURN_LOCAL",
            Self::ReturnNonLocal           => "RETURN_NON_LOCAL",
            Self::Jump(_)                  => "JUMP",
            Bytecode::JumpBackward(_)      => "JUMP_BACKWARD",
            Bytecode::JumpOnTrueTopNil(_)  => "JUMP_ON_TRUE_TOP_NIL",
            Bytecode::JumpOnFalseTopNil(_) => "JUMP_ON_FALSE_TOP_NIL",
            Bytecode::JumpOnTruePop(_)     => "JUMP_ON_TRUE_POP",
            Bytecode::JumpOnFalsePop(_)    => "JUMP_ON_FALSE_POP"
        }
    }

    /// Get the instruction's name padded so that every padded names are of the same length.
    #[rustfmt::skip]
    pub fn padded_name(self) -> &'static str {
        // PADDED_NAMES[self as usize]
        match self {
            Self::Halt                 => "HALT                   ",
            Self::Dup                  => "DUP                    ",
            Self::PushLocal(_, _)      => "PUSH_LOCAL             ",
            Self::PushArgument(_, _)   => "PUSH_ARGUMENT          ",
            Self::PushField(_)         => "PUSH_FIELD             ",
            Self::PushBlock(_)         => "PUSH_BLOCK             ",
            Self::PushConstant(_)      => "PUSH_CONSTANT          ",
            Self::PushGlobal(_)        => "PUSH_GLOBAL            ",
            Self::Pop                  => "POP                    ",
            Self::PopLocal(_, _)       => "POP_LOCAL              ",
            Self::PopArgument(_, _)    => "POP_ARGUMENT           ",
            Self::PopField(_)          => "POP_FIELD              ",
            Self::Send(_)              => "SEND                   ",
            Self::SuperSend(_)         => "SUPER_SEND             ",
            Self::ReturnLocal          => "RETURN_LOCAL           ",
            Self::ReturnNonLocal       => "RETURN_NON_LOCAL       ",
            Self::Jump(_)              => "JUMP                   ",
            Self::JumpBackward(_)      => "JUMP_BACKWARD          ",
            Self::JumpOnTrueTopNil(_)  => "JUMP_ON_TRUE_TOP_NIL   ",
            Self::JumpOnFalseTopNil(_) => "JUMP_ON_FALSE_TOP_NIL  ",
            Self::JumpOnTruePop(_)     => "JUMP_ON_TRUE_POP       ",
            Self::JumpOnFalsePop(_)    => "JUMP_ON_FALSE_POP      "
        }
    }
}

impl fmt::Display for Bytecode {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Halt                      => write!(f, "HALT"),
            Self::Dup                       => write!(f, "DUP"),
            Self::PushLocal(up_idx, idx)    => write!(f, "PUSH_LOCAL {}, {}", up_idx, idx),
            Self::PushArgument(up_idx, idx) => write!(f, "PUSH_ARGUMENT {}, {}", up_idx, idx),
            Self::PushField(idx)            => write!(f, "PUSH_FIELD {}", idx),
            Self::PushBlock(idx)            => write!(f, "PUSH_BLOCK {}", idx),
            Self::PushConstant(idx)         => write!(f, "PUSH_CONSTANT {}", idx),
            Self::PushGlobal(idx)         => write!(f, "PUSH_GLOBAL {}", idx),
            Self::Pop                           => write!(f, "POP"),
            Self::PopLocal(up_idx, idx)     => write!(f, "POP_LOCAL {}, {}", up_idx, idx),
            Self::PopArgument(up_idx, idx)  => write!(f, "POP_ARGUMENT {}, {}", up_idx, idx),
            Self::PopField(idx)             => write!(f, "POP_FIELD {}", idx),
            Self::Send(idx)                 => write!(f, "SEND {}", idx),
            Self::SuperSend(idx)            => write!(f, "SUPER_SEND {}", idx),
            Self::ReturnLocal               => write!(f, "RETURN_LOCAL", ),
            Self::ReturnNonLocal            => write!(f, "RETURN_NON_LOCAL", ),
            Self::Jump(idx)             => write!(f, "JUMP {}", idx),
            Self::JumpBackward(idx)             => write!(f, "JUMP_BACKWARD {}", idx),
            Self::JumpOnFalseTopNil(idx) => write!(f, "JUMP_ON_FALSE_TOP_NIL {}", idx),
            Self::JumpOnFalsePop(idx) => write!(f, "JUMP_ON_FALSE_POP {}", idx),
            Self::JumpOnTrueTopNil(idx) => write!(f, "JUMP_ON_TRUE_TOP_NIL {}", idx),
            Self::JumpOnTruePop(idx) => write!(f, "JUMP_ON_TRUE_POP {}", idx),
        }
    }
}
