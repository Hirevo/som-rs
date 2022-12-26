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
    PushConstant0,
    PushConstant1,
    PushConstant2,
    PushGlobal(u8),
    Push0,
    Push1,
    PushNil,
    Pop,
    PopLocal(u8, u8),
    PopArgument(u8, u8),
    PopField(u8),
    Send1(u8),
    Send2(u8),
    Send3(u8),
    SendN(u8),
    SuperSend1(u8),
    SuperSend2(u8),
    SuperSend3(u8),
    SuperSendN(u8),
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
            Self::Halt               => "HALT",
            Self::Dup                => "DUP",
            Self::PushLocal(_, _)    => "PUSH_LOCAL",
            Self::PushArgument(_, _) => "PUSH_ARGUMENT",
            Self::PushField(_)       => "PUSH_FIELD",
            Self::PushBlock(_)       => "PUSH_BLOCK",
            Self::PushConstant(_)    => "PUSH_CONSTANT",
            Self::PushConstant0      => "PUSH_CONSTANT 0",
            Self::PushConstant1      => "PUSH_CONSTANT 1",
            Self::PushConstant2      => "PUSH_CONSTANT 2",
            Self::PushGlobal(_)      => "PUSH_GLOBAL",
            Self::Push0              => "PUSH_0",
            Self::Push1              => "PUSH_1",
            Self::PushNil            => "PUSH_NIL",
            Self::Pop                => "POP",
            Self::PopLocal(_, _)     => "POP_LOCAL",
            Self::PopArgument(_, _)  => "POP_ARGUMENT",
            Self::PopField(_)        => "POP_FIELD",
            Self::Send1(_)           => "SEND 1",
            Self::Send2(_)           => "SEND 2",
            Self::Send3(_)           => "SEND 3",
            Self::SendN(_)           => "SEND N",
            Self::SuperSend1(_)      => "SUPER_SEND 1",
            Self::SuperSend2(_)      => "SUPER_SEND 2",
            Self::SuperSend3(_)      => "SUPER_SEND 3",
            Self::SuperSendN(_)      => "SUPER_SEND N",
            Self::ReturnLocal        => "RETURN_LOCAL",
            Self::ReturnNonLocal     => "RETURN_NON_LOCAL",
            Self::Jump(_)            => "JUMP",
            _                        => "NO NAME, TODO" // laziness
        }
    }

    /// Get the instruction's name padded so that every padded names are of the same length.
    #[rustfmt::skip]
    pub fn padded_name(self) -> &'static str {
        // PADDED_NAMES[self as usize]
        match self {
            Self::Halt               => "HALT            ",
            Self::Dup                => "DUP             ",
            Self::PushLocal(_, _)    => "PUSH_LOCAL      ",
            Self::PushArgument(_, _) => "PUSH_ARGUMENT   ",
            Self::PushField(_)       => "PUSH_FIELD      ",
            Self::PushBlock(_)       => "PUSH_BLOCK      ",
            Self::PushConstant(_)    => "PUSH_CONSTANT   ",
            Self::PushConstant0      => "PUSH_CONSTANT 0 ",
            Self::PushConstant1      => "PUSH_CONSTANT 1 ",
            Self::PushConstant2      => "PUSH_CONSTANT 2 ",
            Self::PushGlobal(_)      => "PUSH_GLOBAL     ",
            Self::Push0              => "PUSH_0          ",
            Self::Push1              => "PUSH_1          ",
            Self::PushNil            => "PUSH_NIL        ",
            Self::Pop                => "POP             ",
            Self::PopLocal(_, _)     => "POP_LOCAL       ",
            Self::PopArgument(_, _)  => "POP_ARGUMENT    ",
            Self::PopField(_)        => "POP_FIELD       ",
            Self::Send1(_)           => "SEND 1          ",
            Self::Send2(_)           => "SEND 2          ",
            Self::Send3(_)           => "SEND 3          ",
            Self::SendN(_)           => "SEND N          ",
            Self::SuperSend1(_)      => "SUPER_SEND 1    ",
            Self::SuperSend2(_)      => "SUPER_SEND 2    ",
            Self::SuperSend3(_)      => "SUPER_SEND 3    ",
            Self::SuperSendN(_)      => "SUPER_SEND N    ",
            Self::ReturnLocal        => "RETURN_LOCAL    ",
            Self::ReturnNonLocal     => "RETURN_NON_LOCAL",
            Self::Jump(_)            => "JUMP            ",
            _                        => "NO NAME, TODO"
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
            Self::PushConstant0                 => write!(f, "PUSH_CONSTANT_0"),
            Self::PushConstant1                 => write!(f, "PUSH_CONSTANT_1"),
            Self::PushConstant2                 => write!(f, "PUSH_CONSTANT_2"),
            Self::PushGlobal(idx)         => write!(f, "PUSH_GLOBAL {}", idx),
            Self::Push0                         => write!(f, "PUSH_0"),
            Self::Push1                         => write!(f, "PUSH_1"),
            Self::PushNil                       => write!(f, "PUSH_NIL"),
            Self::Pop                           => write!(f, "POP"),
            Self::PopLocal(up_idx, idx)     => write!(f, "POP_LOCAL {}, {}", up_idx, idx),
            Self::PopArgument(up_idx, idx)  => write!(f, "POP_ARGUMENT {}, {}", up_idx, idx),
            Self::PopField(idx)             => write!(f, "POP_FIELD {}", idx),
            Self::Send1(idx)                 => write!(f, "SEND_1 {}", idx),
            Self::Send2(idx)                 => write!(f, "SEND_2 {}", idx),
            Self::Send3(idx)                 => write!(f, "SEND_3 {}", idx),
            Self::SendN(idx)                 => write!(f, "SEND_N {}", idx),
            Self::SuperSend1(idx)            => write!(f, "SUPER_SEND_1 {}", idx),
            Self::SuperSend2(idx)            => write!(f, "SUPER_SEND_2 {}", idx),
            Self::SuperSend3(idx)            => write!(f, "SUPER_SEND_3 {}", idx),
            Self::SuperSendN(idx)            => write!(f, "SUPER_SEND_N {}", idx),
            Self::ReturnLocal               => write!(f, "RETURN_LOCAL", ),
            Self::ReturnNonLocal            => write!(f, "RETURN_NON_LOCAL", ),
            Self::Jump(idx)             => write!(f, "JUMP {}", idx),
            Self::JumpBackward(idx)             => write!(f, "JUMP_BACKWARD {}", idx),
            Self::JumpOnFalseTopNil(idx) => write!(f, "JUMP_ON_FALSE_TOP_NIL {}", idx),
            Self::JumpOnFalsePop(idx) => write!(f, "JUMP_ON_FALSE_POP {}", idx),
            _ => write!(f, "No display for this bytecode, TODO")
        }
    }
}
