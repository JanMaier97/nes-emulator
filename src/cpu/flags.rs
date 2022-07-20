use super::bus::Value;

const CARRY_MASK: Value = Value::from_const(0b0000_0001);
const ZERO_MASK: Value = Value::from_const(0b0000_0010);
const INTERRUPT_MASK: Value = Value::from_const(0b0000_0100);
const DECIMAL_MODE_MASK: Value = Value::from_const(0b0000_1000);
const BREAK_CMD_MASK: Value = Value::from_const(0b0001_0000);
const OVERFLOW_MASK: Value = Value::from_const(0b0100_0000);
pub const NEGATIVE_MASK: Value = Value::from_const(0b1000_0000);
const BASE_FLAG_VALUE: Value = Value::from_const(0b0010_0000);

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

impl From<CpuFlags> for u8 {
    fn from(flags: CpuFlags) -> Self {
        Value::from(flags).into()
    }
}
impl From<u8> for CpuFlags {
    fn from(flags: u8) -> Self {
        Value::from(flags).into()
    }
}

impl From<Value> for CpuFlags {
    fn from(flags: Value) -> Self {
        CpuFlags {
            carry: (flags & CARRY_MASK) != 0.into(),
            zero: (flags & ZERO_MASK) != 0.into(),
            interrupt: (flags & INTERRUPT_MASK) != 0.into(),
            decimal_mode: (flags & DECIMAL_MODE_MASK) != 0.into(),
            break_cmd: (flags & BREAK_CMD_MASK) != 0.into(),
            overflow: (flags & OVERFLOW_MASK) != 0.into(),
            negative: (flags & NEGATIVE_MASK) != 0.into(),
        }
    }
}

impl From<CpuFlags> for Value {
    fn from(flags: CpuFlags) -> Self {
        let mut value = BASE_FLAG_VALUE;

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn carry_flag_convertion() {
        let flag = CpuFlags::from(0b0000_0001);

        assert!(flag.carry);
        assert!(!flag.zero);
        assert!(!flag.interrupt);
        assert!(!flag.decimal_mode);
        assert!(!flag.overflow);
        assert!(!flag.negative);
    }

    #[test]
    fn zero_flag_convertion() {
        let flag = CpuFlags::from(0b0000_0010);

        assert!(flag.zero);
        assert!(!flag.carry);
        assert!(!flag.interrupt);
        assert!(!flag.decimal_mode);
        assert!(!flag.overflow);
        assert!(!flag.negative);
    }
    #[test]
    fn interrupt_flag_convertion() {
        let flag = CpuFlags::from(0b0000_0100);

        assert!(flag.interrupt);
        assert!(!flag.carry);
        assert!(!flag.zero);
        assert!(!flag.decimal_mode);
        assert!(!flag.overflow);
        assert!(!flag.negative);
    }
    #[test]
    fn decimal_flag_convertion() {
        let flag = CpuFlags::from(0b0000_1000);

        assert!(flag.decimal_mode);
        assert!(!flag.carry);
        assert!(!flag.zero);
        assert!(!flag.interrupt);
        assert!(!flag.overflow);
        assert!(!flag.negative);
    }
    #[test]
    fn overflow_flag_convertion() {
        let flag = CpuFlags::from(0b0100_0000);

        assert!(flag.overflow);
        assert!(!flag.zero);
        assert!(!flag.carry);
        assert!(!flag.interrupt);
        assert!(!flag.decimal_mode);
        assert!(!flag.negative);
    }

    #[test]
    fn negative_flag_convertion() {
        let flag = CpuFlags::from(0b1000_0000);

        assert!(flag.negative);
        assert!(!flag.zero);
        assert!(!flag.carry);
        assert!(!flag.interrupt);
        assert!(!flag.decimal_mode);
        assert!(!flag.overflow);
    }

    #[test]
    fn break_flag_convertion() {
        let flag = CpuFlags::from(0b0011_0000);

        assert!(flag.break_cmd);
        assert!(!flag.negative);
        assert!(!flag.zero);
        assert!(!flag.carry);
        assert!(!flag.interrupt);
        assert!(!flag.decimal_mode);
        assert!(!flag.overflow);
    }

    #[test]
    fn break_flag_convertion_2() {
        let flag = CpuFlags::from(0b0001_0000);

        assert!(flag.break_cmd);
        assert!(!flag.negative);
        assert!(!flag.zero);
        assert!(!flag.carry);
        assert!(!flag.interrupt);
        assert!(!flag.decimal_mode);
        assert!(!flag.overflow);
    }

    #[test]
    fn flags_to_u8() {
        let values = [
            0b0010_0000,
            0b0010_0001,
            0b0010_0010,
            0b0010_0100,
            0b0010_1000,
            0b0011_0000,
            0b0110_0000,
            0b1010_0000,
            0b0010_0011,
            0b0010_0101,
            0b0010_1001,
            0b0011_0001,
            0b0110_0001,
            0b1010_0001,
            0b0010_0110,
            0b0010_1010,
            0b0011_0010,
            0b0110_0010,
            0b1010_0010,
            0b0010_1100,
            0b0011_0100,
            0b0110_0100,
            0b1010_0100,
            0b0011_1000,
            0b0110_1000,
            0b1010_1000,
            0x24,
            0x2C,
        ];

        for (i, value) in values.iter().enumerate() {
            let flag = CpuFlags::from(*value);
            let result = flag.into();
            assert_eq!(
                *value, result,
                "Could not convert element {}: {:08b} -> {:08b}",
                i, value, result
            );
        }
    }
}
