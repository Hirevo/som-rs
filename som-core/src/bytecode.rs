use std::fmt;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[rustfmt::skip]
pub enum Bytecode {
    Halt           = 0,
    Dup            = 1,
    PushLocal      = 2,
    PushArgument   = 3,
    PushField      = 4,
    PushBlock      = 5,
    PushConstant   = 6,
    PushGlobal     = 7,
    Pop            = 8,
    PopLocal       = 9,
    PopArgument    = 10,
    PopField       = 11,
    Send           = 12,
    SuperSend      = 13,
    ReturnLocal    = 14,
    ReturnNonLocal = 15,
}

impl Bytecode {
    /// Get the instruction's name.
    #[rustfmt::skip]
    pub fn name(self) -> &'static str {
        // NAMES[self as usize]
        match self {
            Self::Halt           => "HALT",
            Self::Dup            => "DUP",
            Self::PushLocal      => "PUSH_LOCAL",
            Self::PushArgument   => "PUSH_ARGUMENT",
            Self::PushField      => "PUSH_FIELD",
            Self::PushBlock      => "PUSH_BLOCK",
            Self::PushConstant   => "PUSH_CONSTANT",
            Self::PushGlobal     => "PUSH_GLOBAL",
            Self::Pop            => "POP",
            Self::PopLocal       => "POP_LOCAL",
            Self::PopArgument    => "POP_ARGUMENT",
            Self::PopField       => "POP_FIELD",
            Self::Send           => "SEND",
            Self::SuperSend      => "SUPER_SEND",
            Self::ReturnLocal    => "RETURN_LOCAL",
            Self::ReturnNonLocal => "RETURN_NON_LOCAL",
        }
    }

    /// Get the instruction's name padded so that every padded names are of the same length.
    #[rustfmt::skip]
    pub fn padded_name(self) -> &'static str {
        // PADDED_NAMES[self as usize]
        match self {
            Self::Halt           => "HALT            ",
            Self::Dup            => "DUP             ",
            Self::PushLocal      => "PUSH_LOCAL      ",
            Self::PushArgument   => "PUSH_ARGUMENT   ",
            Self::PushField      => "PUSH_FIELD      ",
            Self::PushBlock      => "PUSH_BLOCK      ",
            Self::PushConstant   => "PUSH_CONSTANT   ",
            Self::PushGlobal     => "PUSH_GLOBAL     ",
            Self::Pop            => "POP             ",
            Self::PopLocal       => "POP_LOCAL       ",
            Self::PopArgument    => "POP_ARGUMENT    ",
            Self::PopField       => "POP_FIELD       ",
            Self::Send           => "SEND            ",
            Self::SuperSend      => "SUPER_SEND      ",
            Self::ReturnLocal    => "RETURN_LOCAL    ",
            Self::ReturnNonLocal => "RETURN_NON_LOCAL",
        }
    }

    /// Get the number of bytes to read to process the instruction.
    #[rustfmt::skip]
    pub fn bytecode_len(self) -> usize {
        match self {
            Self::Halt           => 1,
            Self::Dup            => 1,
            Self::PushLocal      => 3,
            Self::PushArgument   => 3,
            Self::PushField      => 2,
            Self::PushBlock      => 2,
            Self::PushConstant   => 2,
            Self::PushGlobal     => 2,
            Self::Pop            => 1,
            Self::PopLocal       => 3,
            Self::PopArgument    => 3,
            Self::PopField       => 2,
            Self::Send           => 2,
            Self::SuperSend      => 2,
            Self::ReturnLocal    => 1,
            Self::ReturnNonLocal => 1,
        }
    }

    /// Attempt to convert a raw byte to an instruction.
    #[rustfmt::skip]
    pub fn from_byte(byte: u8) -> Option<Self> {
        match byte {
             0 => Some(Self::Halt),
             1 => Some(Self::Dup),
             2 => Some(Self::PushLocal),
             3 => Some(Self::PushArgument),
             4 => Some(Self::PushField),
             5 => Some(Self::PushBlock),
             6 => Some(Self::PushConstant),
             7 => Some(Self::PushGlobal),
             8 => Some(Self::Pop),
             9 => Some(Self::PopLocal),
            10 => Some(Self::PopArgument),
            11 => Some(Self::PopField),
            12 => Some(Self::Send),
            13 => Some(Self::SuperSend),
            14 => Some(Self::ReturnLocal),
            15 => Some(Self::ReturnNonLocal),
             _ => None,
        }
    }
}

pub static NAMES: [&str; 16] = [
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
    "SEND",
    "SUPER_SEND",
    "RETURN_LOCAL",
    "RETURN_NON_LOCAL",
];

pub static PADDED_NAMES: [&str; 16] = [
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
    "SEND            ",
    "SUPER_SEND      ",
    "RETURN_LOCAL    ",
    "RETURN_NON_LOCAL",
];

impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}
