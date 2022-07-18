const CARRY_MASK: u8 = 0b0000_0001;
const ZERO_MASK: u8 = 0b0000_0010;
const INTERRUPT_MASK: u8 = 0b0000_0100;
const DECIMAL_MODE_MASK: u8 = 0b0000_1000;
const BREAK_CMD_MASK: u8 = 0b0011_0000;
const OVERFLOW_MASK: u8 = 0b0100_0000;
const NEGATIVE_MASK: u8 = 0b1000_0000;

#[derive(Debug, Clone, Copy)]
pub struct CpuFlags {
    pub carry: bool,
    pub zero: bool,
    pub interrupt: bool,
    pub decimal_mode: bool,
    pub break_cmd: bool,
    pub overflow: bool,
    pub negative: bool,
}


impl From<u8> for CpuFlags {
    fn from(flags: u8) -> Self {
        CpuFlags {
            carry: (flags & CARRY_MASK) > 0,
            zero: (flags & ZERO_MASK) > 0,
            interrupt: (flags & INTERRUPT_MASK) > 0,
            decimal_mode: (flags & DECIMAL_MODE_MASK) > 0,
            break_cmd: (flags & BREAK_CMD_MASK) > 0,
            overflow: (flags & OVERFLOW_MASK) > 0,
            negative: (flags & NEGATIVE_MASK) > 0,
        }
    }
}

impl From<CpuFlags>  for u8 {
    fn from(flags: CpuFlags) -> Self {
        let mut value = 0;

        if flags.carry {
            value += CARRY_MASK;
        }

        if flags.zero {
            value += ZERO_MASK;
        }
        
        if flags.interrupt {
            value += INTERRUPT_MASK;
        }

        if flags.decimal_mode {
            value += DECIMAL_MODE_MASK;
        }

        if flags.break_cmd {
            value += BREAK_CMD_MASK;
        }

        if flags.overflow {
            value += OVERFLOW_MASK;
        }

        if flags.negative {
            value += NEGATIVE_MASK;
        }

        value
    }
}

// impl CpuFlags {
//     pub fn from_bits(flags: u8) -> Self {
//     }
// }
