use core::fmt;
use lazy_static::lazy_static;
use std::collections::HashMap;

use super::bus::values::Value;

lazy_static! {
    pub static ref CPU_OPS_CODES: HashMap<u8, OpCode> = [
        (
            0x69,
            OpCode::new(OpGroup::ADC, 2, 2, AddressingMode::Immediate)
        ),
        (
            0x65,
            OpCode::new(OpGroup::ADC, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x75,
            OpCode::new(OpGroup::ADC, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0x6D,
            OpCode::new(OpGroup::ADC, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x7D,
            OpCode::new(OpGroup::ADC, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0x79,
            OpCode::new(OpGroup::ADC, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0x61,
            OpCode::new(OpGroup::ADC, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0x71,
            OpCode::new(OpGroup::ADC, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0x29,
            OpCode::new(OpGroup::AND, 2, 2, AddressingMode::Immediate)
        ),
        (
            0x25,
            OpCode::new(OpGroup::AND, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x35,
            OpCode::new(OpGroup::AND, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0x2D,
            OpCode::new(OpGroup::AND, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x3D,
            OpCode::new(OpGroup::AND, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0x39,
            OpCode::new(OpGroup::AND, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0x21,
            OpCode::new(OpGroup::AND, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0x31,
            OpCode::new(OpGroup::AND, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0x0A,
            OpCode::new(OpGroup::ASL, 1, 2, AddressingMode::Accumulator)
        ),
        (
            0x06,
            OpCode::new(OpGroup::ASL, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0x16,
            OpCode::new(OpGroup::ASL, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0x0E,
            OpCode::new(OpGroup::ASL, 3, 6, AddressingMode::Absolute)
        ),
        (
            0x1E,
            OpCode::new(OpGroup::ASL, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0x90,
            OpCode::new(OpGroup::BCC, 2, 2, AddressingMode::Relative)
        ),
        (
            0xB0,
            OpCode::new(OpGroup::BCS, 2, 2, AddressingMode::Relative)
        ),
        (
            0xF0,
            OpCode::new(OpGroup::BEQ, 2, 2, AddressingMode::Relative)
        ),
        (
            0x24,
            OpCode::new(OpGroup::BIT, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x2C,
            OpCode::new(OpGroup::BIT, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x30,
            OpCode::new(OpGroup::BMI, 2, 2, AddressingMode::Relative)
        ),
        (
            0xD0,
            OpCode::new(OpGroup::BNE, 2, 2, AddressingMode::Relative)
        ),
        (
            0x10,
            OpCode::new(OpGroup::BPL, 2, 2, AddressingMode::Relative)
        ),
        (
            0x00,
            OpCode::new(OpGroup::BRK, 1, 7, AddressingMode::Implicit)
        ),
        (
            0x50,
            OpCode::new(OpGroup::BVC, 2, 2, AddressingMode::Relative)
        ),
        (
            0x70,
            OpCode::new(OpGroup::BVS, 2, 2, AddressingMode::Relative)
        ),
        (
            0x18,
            OpCode::new(OpGroup::CLC, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xD8,
            OpCode::new(OpGroup::CLD, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x58,
            OpCode::new(OpGroup::CLI, 1, 2, AddressingMode::Immediate)
        ),
        (
            0xB8,
            OpCode::new(OpGroup::CLV, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xc9,
            OpCode::new(OpGroup::CMP, 2, 2, AddressingMode::Immediate)
        ),
        (
            0xc5,
            OpCode::new(OpGroup::CMP, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xd5,
            OpCode::new(OpGroup::CMP, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0xcd,
            OpCode::new(OpGroup::CMP, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xdd,
            OpCode::new(OpGroup::CMP, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0xd9,
            OpCode::new(OpGroup::CMP, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0xc1,
            OpCode::new(OpGroup::CMP, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0xd1,
            OpCode::new(OpGroup::CMP, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0xe0,
            OpCode::new(OpGroup::CPX, 2, 2, AddressingMode::Immediate)
        ),
        (
            0xe4,
            OpCode::new(OpGroup::CPX, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xec,
            OpCode::new(OpGroup::CPX, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xc0,
            OpCode::new(OpGroup::CPY, 2, 2, AddressingMode::Immediate)
        ),
        (
            0xc4,
            OpCode::new(OpGroup::CPY, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xcc,
            OpCode::new(OpGroup::CPY, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xc6,
            OpCode::new(OpGroup::DEC, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0xd6,
            OpCode::new(OpGroup::DEC, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0xce,
            OpCode::new(OpGroup::DEC, 3, 6, AddressingMode::Absolute)
        ),
        (
            0xde,
            OpCode::new(OpGroup::DEC, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0xca,
            OpCode::new(OpGroup::DEX, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x88,
            OpCode::new(OpGroup::DEY, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x49,
            OpCode::new(OpGroup::EOR, 2, 2, AddressingMode::Immediate)
        ),
        (
            0x45,
            OpCode::new(OpGroup::EOR, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x55,
            OpCode::new(OpGroup::EOR, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0x4D,
            OpCode::new(OpGroup::EOR, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x5d,
            OpCode::new(OpGroup::EOR, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0x59,
            OpCode::new(OpGroup::EOR, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0x41,
            OpCode::new(OpGroup::EOR, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0x51,
            OpCode::new(OpGroup::EOR, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0xe6,
            OpCode::new(OpGroup::INC, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0xf6,
            OpCode::new(OpGroup::INC, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0xee,
            OpCode::new(OpGroup::INC, 3, 6, AddressingMode::Absolute)
        ),
        (
            0xfe,
            OpCode::new(OpGroup::INC, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0xe8,
            OpCode::new(OpGroup::INX, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xC8,
            OpCode::new(OpGroup::INY, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x4c,
            OpCode::new(OpGroup::JMP, 3, 3, AddressingMode::Absolute)
        ),
        (
            0x6c,
            OpCode::new(OpGroup::JMP, 3, 5, AddressingMode::Indirect)
        ),
        (
            0x20,
            OpCode::new(OpGroup::JSR, 3, 6, AddressingMode::Absolute)
        ),
        (
            0xA9,
            OpCode::new(OpGroup::LDA, 2, 2, AddressingMode::Immediate)
        ),
        (
            0xA5,
            OpCode::new(OpGroup::LDA, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xB5,
            OpCode::new(OpGroup::LDA, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0xAD,
            OpCode::new(OpGroup::LDA, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xBD,
            OpCode::new(OpGroup::LDA, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0xB9,
            OpCode::new(OpGroup::LDA, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0xa1,
            OpCode::new(OpGroup::LDA, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0xB1,
            OpCode::new(OpGroup::LDA, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0xA2,
            OpCode::new(OpGroup::LDX, 2, 2, AddressingMode::Immediate)
        ),
        (
            0xA6,
            OpCode::new(OpGroup::LDX, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xB6,
            OpCode::new(OpGroup::LDX, 2, 4, AddressingMode::ZeroPageY)
        ),
        (
            0xAE,
            OpCode::new(OpGroup::LDX, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xBE,
            OpCode::new(OpGroup::LDX, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0xA0,
            OpCode::new(OpGroup::LDY, 2, 2, AddressingMode::Immediate)
        ),
        (
            0xA4,
            OpCode::new(OpGroup::LDY, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xB4,
            OpCode::new(OpGroup::LDY, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0xAC,
            OpCode::new(OpGroup::LDY, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xBC,
            OpCode::new(OpGroup::LDY, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0x4a,
            OpCode::new(OpGroup::LSR, 1, 2, AddressingMode::Accumulator)
        ),
        (
            0x46,
            OpCode::new(OpGroup::LSR, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0x56,
            OpCode::new(OpGroup::LSR, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0x4e,
            OpCode::new(OpGroup::LSR, 3, 6, AddressingMode::Absolute)
        ),
        (
            0x5e,
            OpCode::new(OpGroup::LSR, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0xea,
            OpCode::new(OpGroup::NOP, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x09,
            OpCode::new(OpGroup::ORA, 2, 2, AddressingMode::Immediate)
        ),
        (
            0x05,
            OpCode::new(OpGroup::ORA, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x15,
            OpCode::new(OpGroup::ORA, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0x0D,
            OpCode::new(OpGroup::ORA, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x1d,
            OpCode::new(OpGroup::ORA, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0x19,
            OpCode::new(OpGroup::ORA, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0x01,
            OpCode::new(OpGroup::ORA, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0x11,
            OpCode::new(OpGroup::ORA, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0x08,
            OpCode::new(OpGroup::PHP, 1, 3, AddressingMode::Implicit)
        ),
        (
            0x68,
            OpCode::new(OpGroup::PLA, 1, 4, AddressingMode::Implicit)
        ),
        (
            0x28,
            OpCode::new(OpGroup::PLP, 1, 4, AddressingMode::Implicit)
        ),
        (
            0x2a,
            OpCode::new(OpGroup::ROL, 1, 2, AddressingMode::Accumulator)
        ),
        (
            0x26,
            OpCode::new(OpGroup::ROL, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0x36,
            OpCode::new(OpGroup::ROL, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0x2e,
            OpCode::new(OpGroup::ROL, 3, 6, AddressingMode::Absolute)
        ),
        (
            0x3e,
            OpCode::new(OpGroup::ROL, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0x6a,
            OpCode::new(OpGroup::ROR, 1, 2, AddressingMode::Accumulator)
        ),
        (
            0x66,
            OpCode::new(OpGroup::ROR, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0x76,
            OpCode::new(OpGroup::ROR, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0x6e,
            OpCode::new(OpGroup::ROR, 3, 6, AddressingMode::Absolute)
        ),
        (
            0x7e,
            OpCode::new(OpGroup::ROR, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0x40,
            OpCode::new(OpGroup::RTI, 1, 6, AddressingMode::Implicit)
        ),
        (
            0x60,
            OpCode::new(OpGroup::RTS, 1, 6, AddressingMode::Implicit)
        ),
        (
            0xe9,
            OpCode::new(OpGroup::SBC, 2, 6, AddressingMode::Immediate)
        ),
        (
            0xe5,
            OpCode::new(OpGroup::SBC, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xf5,
            OpCode::new(OpGroup::SBC, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0xed,
            OpCode::new(OpGroup::SBC, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xfd,
            OpCode::new(OpGroup::SBC, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0xf9,
            OpCode::new(OpGroup::SBC, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0xe1,
            OpCode::new(OpGroup::SBC, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0xf1,
            OpCode::new(OpGroup::SBC, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0x38,
            OpCode::new(OpGroup::SEC, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xf8,
            OpCode::new(OpGroup::SED, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x78,
            OpCode::new(OpGroup::SEI, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x85,
            OpCode::new(OpGroup::STA, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x95,
            OpCode::new(OpGroup::STA, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0x8d,
            OpCode::new(OpGroup::STA, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x9d,
            OpCode::new(OpGroup::STA, 3, 5, AddressingMode::AbsoluteX)
        ),
        (
            0x99,
            OpCode::new(OpGroup::STA, 3, 5, AddressingMode::AbsoluteY)
        ),
        (
            0x81,
            OpCode::new(OpGroup::STA, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0x91,
            OpCode::new(OpGroup::STA, 2, 6, AddressingMode::IndirectY)
        ),
        (
            0x86,
            OpCode::new(OpGroup::STX, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x96,
            OpCode::new(OpGroup::STX, 2, 4, AddressingMode::ZeroPageY)
        ),
        (
            0x8E,
            OpCode::new(OpGroup::STX, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x84,
            OpCode::new(OpGroup::STY, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x94,
            OpCode::new(OpGroup::STY, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0x8c,
            OpCode::new(OpGroup::STY, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xAA,
            OpCode::new(OpGroup::TAX, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xA8,
            OpCode::new(OpGroup::TAY, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xba,
            OpCode::new(OpGroup::TSX, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x8a,
            OpCode::new(OpGroup::TXA, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x9a,
            OpCode::new(OpGroup::TXS, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x98,
            OpCode::new(OpGroup::TYA, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x48,
            OpCode::new(OpGroup::PHA, 1, 2, AddressingMode::Implicit)
        ),
    ]
    .iter()
    .cloned()
    .collect();
}

#[derive(Debug, Clone, Copy)]
pub enum OpGroup {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
}

impl fmt::Display for OpGroup {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OpCode {
    pub group: OpGroup,
    pub bytes: u16,
    pub cycles: u8,
    pub mode: AddressingMode,
}

impl OpCode {
    const fn new(group: OpGroup, bytes: u16, cycles: u8, mode: AddressingMode) -> OpCode {
        OpCode {
            group,
            bytes,
            cycles,
            mode,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AddressingMode {
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Accumulator,
    Immediate,
    Implicit,
    IndirectX,
    IndirectY,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Indirect,
}

pub fn get_opcode(raw_opcode: Value) -> OpCode {
    let opcode = CPU_OPS_CODES.get(&raw_opcode.into());

    match opcode {
        Some(op) => *op,
        None => panic!("Opcode {:02X} is not defined yet", raw_opcode)
    }
}
