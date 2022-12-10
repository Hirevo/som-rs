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
            Self::PushGlobal(_)      => "PUSH_GLOBAL",
            Self::Pop                => "POP",
            Self::PopLocal(_, _)     => "POP_LOCAL",
            Self::PopArgument(_, _)  => "POP_ARGUMENT",
            Self::PopField(_)        => "POP_FIELD",
            Self::Send1(_)            => "SEND 1",
            Self::Send2(_)            => "SEND 2",
            Self::Send3(_)            => "SEND 3",
            Self::SendN(_)            => "SEND N",
            Self::SuperSend1(_)       => "SUPER_SEND 1",
            Self::SuperSend2(_)       => "SUPER_SEND 2",
            Self::SuperSend3(_)       => "SUPER_SEND 3",
            Self::SuperSendN(_)       => "SUPER_SEND N",
            Self::ReturnLocal        => "RETURN_LOCAL",
            Self::ReturnNonLocal     => "RETURN_NON_LOCAL",
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
            Self::PushGlobal(_)      => "PUSH_GLOBAL     ",
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
        }
    }
}

pub static NAMES: [&str; 22] = [
    "HALT",
    "DUP",
    "PUSH_LOCAL",
    "PUSH_ARGUMENT",
    "PUSH_FIELD",
    "PUSH_BLOCK",
    "PUSH_CONSTANT",
    "PUSH_GLOBAL",
    "POP",
    "POP_LOCAL",
    "POP_ARGUMENT",
    "POP_FIELD",
    "SEND_1",
    "SEND_2",
    "SEND_3",
    "SEND_N",
    "SUPER_SEND_1",
    "SUPER_SEND_2",
    "SUPER_SEND_3",
    "SUPER_SEND_N",
    "RETURN_LOCAL",
    "RETURN_NON_LOCAL",
];

pub static PADDED_NAMES: [&str; 22] = [
    "HALT            ",
    "DUP             ",
    "PUSH_LOCAL      ",
    "PUSH_ARGUMENT   ",
    "PUSH_FIELD      ",
    "PUSH_BLOCK      ",
    "PUSH_CONSTANT   ",
    "PUSH_GLOBAL     ",
    "POP             ",
    "POP_LOCAL       ",
    "POP_ARGUMENT    ",
    "POP_FIELD       ",
    "SEND_1          ",
    "SEND_2          ",
    "SEND_3          ",
    "SEND_N          ",
    "SUPER_SEND 1    ",
    "SUPER_SEND 2    ",
    "SUPER_SEND 3    ",
    "SUPER_SEND N    ",
    "RETURN_LOCAL    ",
    "RETURN_NON_LOCAL",
];

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
            Self::PushGlobal(idx)           => write!(f, "PUSH_GLOBAL {}", idx),
            Self::Pop                       => write!(f, "POP"),
            Self::PopLocal(up_idx, idx)     => write!(f, "POP_LOCAL {}, {}", up_idx, idx),
            Self::PopArgument(up_idx, idx)  => write!(f, "POP_ARGUMENT {}, {}", up_idx, idx),
            Self::PopField(idx)             => write!(f, "POP_FIELD {}", idx),
            Self::Send1(idx)                 => write!(f, "SEND 1 {}", idx),
            Self::Send2(idx)                 => write!(f, "SEND 2 {}", idx),
            Self::Send3(idx)                 => write!(f, "SEND 3 {}", idx),
            Self::SendN(idx)                 => write!(f, "SEND N {}", idx),
            Self::SuperSend1(idx)            => write!(f, "SUPER_SEND 1 {}", idx),
            Self::SuperSend2(idx)            => write!(f, "SUPER_SEND 2 {}", idx),
            Self::SuperSend3(idx)            => write!(f, "SUPER_SEND 3 {}", idx),
            Self::SuperSendN(idx)            => write!(f, "SUPER_SEND N {}", idx),
            Self::ReturnLocal               => write!(f, "RETURN_LOCAL", ),
            Self::ReturnNonLocal            => write!(f, "RETURN_NON_LOCAL", ),
        }
    }
}
