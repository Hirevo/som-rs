use std::fmt;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Bytecode {
    Halt = 0,
    Dup = 1,
    PushLocal = 2,
    PushArgument = 3,
    PushField = 4,
    PushBlock = 5,
    PushConstant = 6,
    PushGlobal = 7,
    Pop = 8,
    PopLocal = 9,
    PopArgument = 10,
    PopField = 11,
    Send = 12,
    SuperSend = 13,
    ReturnLocal = 14,
    ReturnNonLocal = 15,
}

impl Bytecode {
    /// Get the instruction's name.
    pub fn name(self) -> &'static str {
        NAMES[self as usize]
    }

    /// Get the instruction's name padded so that every padded names are of the same length.
    pub fn padded_name(self) -> &'static str {
        PADDED_NAMES[self as usize]
    }

    /// Get the number of bytes to read to process the instruction.
    pub fn bytecode_len(self) -> usize {
        match self {
            Bytecode::Halt => 1,
            Bytecode::Dup => 1,
            Bytecode::PushLocal => 3,
            Bytecode::PushArgument => 3,
            Bytecode::PushField => 2,
            Bytecode::PushBlock => 2,
            Bytecode::PushConstant => 2,
            Bytecode::PushGlobal => 2,
            Bytecode::Pop => 1,
            Bytecode::PopLocal => 3,
            Bytecode::PopArgument => 3,
            Bytecode::PopField => 2,
            Bytecode::Send => 2,
            Bytecode::SuperSend => 2,
            Bytecode::ReturnLocal => 1,
            Bytecode::ReturnNonLocal => 1,
        }
    }

    /// Attempt to convert a raw byte to an instruction.
    pub fn from_byte(byte: u8) -> Option<Bytecode> {
        match byte {
            0 => Some(Bytecode::Halt),
            1 => Some(Bytecode::Dup),
            2 => Some(Bytecode::PushLocal),
            3 => Some(Bytecode::PushArgument),
            4 => Some(Bytecode::PushField),
            5 => Some(Bytecode::PushBlock),
            6 => Some(Bytecode::PushConstant),
            7 => Some(Bytecode::PushGlobal),
            8 => Some(Bytecode::Pop),
            9 => Some(Bytecode::PopLocal),
            10 => Some(Bytecode::PopArgument),
            11 => Some(Bytecode::PopField),
            12 => Some(Bytecode::Send),
            13 => Some(Bytecode::SuperSend),
            14 => Some(Bytecode::ReturnLocal),
            15 => Some(Bytecode::ReturnNonLocal),
            _ => None,
        }
    }
}

static NAMES: [&str; 16] = [
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

static PADDED_NAMES: [&str; 16] = [
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
