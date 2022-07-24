use core::fmt;
use lazy_static::lazy_static;
use std::collections::HashMap;

use super::bus::values::Value;

lazy_static! {
    pub static ref CPU_OPS_CODES: HashMap<u8, OpCode> = [
        (
            0x69,
            OpCode::offical(OpGroup::ADC, 2, 2, AddressingMode::Immediate)
        ),
        (
            0x65,
            OpCode::offical(OpGroup::ADC, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x75,
            OpCode::offical(OpGroup::ADC, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0x6D,
            OpCode::offical(OpGroup::ADC, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x7D,
            OpCode::offical(OpGroup::ADC, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0x79,
            OpCode::offical(OpGroup::ADC, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0x61,
            OpCode::offical(OpGroup::ADC, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0x71,
            OpCode::offical(OpGroup::ADC, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0x29,
            OpCode::offical(OpGroup::AND, 2, 2, AddressingMode::Immediate)
        ),
        (
            0x25,
            OpCode::offical(OpGroup::AND, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x35,
            OpCode::offical(OpGroup::AND, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0x2D,
            OpCode::offical(OpGroup::AND, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x3D,
            OpCode::offical(OpGroup::AND, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0x39,
            OpCode::offical(OpGroup::AND, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0x21,
            OpCode::offical(OpGroup::AND, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0x31,
            OpCode::offical(OpGroup::AND, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0x0A,
            OpCode::offical(OpGroup::ASL, 1, 2, AddressingMode::Accumulator)
        ),
        (
            0x06,
            OpCode::offical(OpGroup::ASL, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0x16,
            OpCode::offical(OpGroup::ASL, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0x0E,
            OpCode::offical(OpGroup::ASL, 3, 6, AddressingMode::Absolute)
        ),
        (
            0x1E,
            OpCode::offical(OpGroup::ASL, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0x90,
            OpCode::offical(OpGroup::BCC, 2, 2, AddressingMode::Relative)
        ),
        (
            0xB0,
            OpCode::offical(OpGroup::BCS, 2, 2, AddressingMode::Relative)
        ),
        (
            0xF0,
            OpCode::offical(OpGroup::BEQ, 2, 2, AddressingMode::Relative)
        ),
        (
            0x24,
            OpCode::offical(OpGroup::BIT, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x2C,
            OpCode::offical(OpGroup::BIT, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x30,
            OpCode::offical(OpGroup::BMI, 2, 2, AddressingMode::Relative)
        ),
        (
            0xD0,
            OpCode::offical(OpGroup::BNE, 2, 2, AddressingMode::Relative)
        ),
        (
            0x10,
            OpCode::offical(OpGroup::BPL, 2, 2, AddressingMode::Relative)
        ),
        (
            0x00,
            OpCode::offical(OpGroup::BRK, 1, 7, AddressingMode::Implicit)
        ),
        (
            0x50,
            OpCode::offical(OpGroup::BVC, 2, 2, AddressingMode::Relative)
        ),
        (
            0x70,
            OpCode::offical(OpGroup::BVS, 2, 2, AddressingMode::Relative)
        ),
        (
            0x18,
            OpCode::offical(OpGroup::CLC, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xD8,
            OpCode::offical(OpGroup::CLD, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x58,
            OpCode::offical(OpGroup::CLI, 1, 2, AddressingMode::Immediate)
        ),
        (
            0xB8,
            OpCode::offical(OpGroup::CLV, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xc9,
            OpCode::offical(OpGroup::CMP, 2, 2, AddressingMode::Immediate)
        ),
        (
            0xc5,
            OpCode::offical(OpGroup::CMP, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xd5,
            OpCode::offical(OpGroup::CMP, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0xcd,
            OpCode::offical(OpGroup::CMP, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xdd,
            OpCode::offical(OpGroup::CMP, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0xd9,
            OpCode::offical(OpGroup::CMP, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0xc1,
            OpCode::offical(OpGroup::CMP, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0xd1,
            OpCode::offical(OpGroup::CMP, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0xe0,
            OpCode::offical(OpGroup::CPX, 2, 2, AddressingMode::Immediate)
        ),
        (
            0xe4,
            OpCode::offical(OpGroup::CPX, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xec,
            OpCode::offical(OpGroup::CPX, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xc0,
            OpCode::offical(OpGroup::CPY, 2, 2, AddressingMode::Immediate)
        ),
        (
            0xc4,
            OpCode::offical(OpGroup::CPY, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xcc,
            OpCode::offical(OpGroup::CPY, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xc6,
            OpCode::offical(OpGroup::DEC, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0xd6,
            OpCode::offical(OpGroup::DEC, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0xce,
            OpCode::offical(OpGroup::DEC, 3, 6, AddressingMode::Absolute)
        ),
        (
            0xde,
            OpCode::offical(OpGroup::DEC, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0xca,
            OpCode::offical(OpGroup::DEX, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x88,
            OpCode::offical(OpGroup::DEY, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x49,
            OpCode::offical(OpGroup::EOR, 2, 2, AddressingMode::Immediate)
        ),
        (
            0x45,
            OpCode::offical(OpGroup::EOR, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x55,
            OpCode::offical(OpGroup::EOR, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0x4D,
            OpCode::offical(OpGroup::EOR, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x5d,
            OpCode::offical(OpGroup::EOR, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0x59,
            OpCode::offical(OpGroup::EOR, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0x41,
            OpCode::offical(OpGroup::EOR, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0x51,
            OpCode::offical(OpGroup::EOR, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0xe6,
            OpCode::offical(OpGroup::INC, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0xf6,
            OpCode::offical(OpGroup::INC, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0xee,
            OpCode::offical(OpGroup::INC, 3, 6, AddressingMode::Absolute)
        ),
        (
            0xfe,
            OpCode::offical(OpGroup::INC, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0xe8,
            OpCode::offical(OpGroup::INX, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xC8,
            OpCode::offical(OpGroup::INY, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x4c,
            OpCode::offical(OpGroup::JMP, 3, 3, AddressingMode::Absolute)
        ),
        (
            0x6c,
            OpCode::offical(OpGroup::JMP, 3, 5, AddressingMode::Indirect)
        ),
        (
            0x20,
            OpCode::offical(OpGroup::JSR, 3, 6, AddressingMode::Absolute)
        ),
        (
            0xA9,
            OpCode::offical(OpGroup::LDA, 2, 2, AddressingMode::Immediate)
        ),
        (
            0xA5,
            OpCode::offical(OpGroup::LDA, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xB5,
            OpCode::offical(OpGroup::LDA, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0xAD,
            OpCode::offical(OpGroup::LDA, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xBD,
            OpCode::offical(OpGroup::LDA, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0xB9,
            OpCode::offical(OpGroup::LDA, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0xa1,
            OpCode::offical(OpGroup::LDA, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0xB1,
            OpCode::offical(OpGroup::LDA, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0xA2,
            OpCode::offical(OpGroup::LDX, 2, 2, AddressingMode::Immediate)
        ),
        (
            0xA6,
            OpCode::offical(OpGroup::LDX, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xB6,
            OpCode::offical(OpGroup::LDX, 2, 4, AddressingMode::ZeroPageY)
        ),
        (
            0xAE,
            OpCode::offical(OpGroup::LDX, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xBE,
            OpCode::offical(OpGroup::LDX, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0xA0,
            OpCode::offical(OpGroup::LDY, 2, 2, AddressingMode::Immediate)
        ),
        (
            0xA4,
            OpCode::offical(OpGroup::LDY, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xB4,
            OpCode::offical(OpGroup::LDY, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0xAC,
            OpCode::offical(OpGroup::LDY, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xBC,
            OpCode::offical(OpGroup::LDY, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0x4a,
            OpCode::offical(OpGroup::LSR, 1, 2, AddressingMode::Accumulator)
        ),
        (
            0x46,
            OpCode::offical(OpGroup::LSR, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0x56,
            OpCode::offical(OpGroup::LSR, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0x4e,
            OpCode::offical(OpGroup::LSR, 3, 6, AddressingMode::Absolute)
        ),
        (
            0x5e,
            OpCode::offical(OpGroup::LSR, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0xea,
            OpCode::offical(OpGroup::NOP, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x09,
            OpCode::offical(OpGroup::ORA, 2, 2, AddressingMode::Immediate)
        ),
        (
            0x05,
            OpCode::offical(OpGroup::ORA, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x15,
            OpCode::offical(OpGroup::ORA, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0x0D,
            OpCode::offical(OpGroup::ORA, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x1d,
            OpCode::offical(OpGroup::ORA, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0x19,
            OpCode::offical(OpGroup::ORA, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0x01,
            OpCode::offical(OpGroup::ORA, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0x11,
            OpCode::offical(OpGroup::ORA, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0x08,
            OpCode::offical(OpGroup::PHP, 1, 3, AddressingMode::Implicit)
        ),
        (
            0x68,
            OpCode::offical(OpGroup::PLA, 1, 4, AddressingMode::Implicit)
        ),
        (
            0x28,
            OpCode::offical(OpGroup::PLP, 1, 4, AddressingMode::Implicit)
        ),
        (
            0x2a,
            OpCode::offical(OpGroup::ROL, 1, 2, AddressingMode::Accumulator)
        ),
        (
            0x26,
            OpCode::offical(OpGroup::ROL, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0x36,
            OpCode::offical(OpGroup::ROL, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0x2e,
            OpCode::offical(OpGroup::ROL, 3, 6, AddressingMode::Absolute)
        ),
        (
            0x3e,
            OpCode::offical(OpGroup::ROL, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0x6a,
            OpCode::offical(OpGroup::ROR, 1, 2, AddressingMode::Accumulator)
        ),
        (
            0x66,
            OpCode::offical(OpGroup::ROR, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0x76,
            OpCode::offical(OpGroup::ROR, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0x6e,
            OpCode::offical(OpGroup::ROR, 3, 6, AddressingMode::Absolute)
        ),
        (
            0x7e,
            OpCode::offical(OpGroup::ROR, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0x40,
            OpCode::offical(OpGroup::RTI, 1, 6, AddressingMode::Implicit)
        ),
        (
            0x60,
            OpCode::offical(OpGroup::RTS, 1, 6, AddressingMode::Implicit)
        ),
        (
            0xe9,
            OpCode::offical(OpGroup::SBC, 2, 6, AddressingMode::Immediate)
        ),
        (
            0xe5,
            OpCode::offical(OpGroup::SBC, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xf5,
            OpCode::offical(OpGroup::SBC, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0xed,
            OpCode::offical(OpGroup::SBC, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xfd,
            OpCode::offical(OpGroup::SBC, 3, 4, AddressingMode::AbsoluteX)
        ),
        (
            0xf9,
            OpCode::offical(OpGroup::SBC, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0xe1,
            OpCode::offical(OpGroup::SBC, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0xf1,
            OpCode::offical(OpGroup::SBC, 2, 5, AddressingMode::IndirectY)
        ),
        (
            0x38,
            OpCode::offical(OpGroup::SEC, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xf8,
            OpCode::offical(OpGroup::SED, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x78,
            OpCode::offical(OpGroup::SEI, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x85,
            OpCode::offical(OpGroup::STA, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x95,
            OpCode::offical(OpGroup::STA, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0x8d,
            OpCode::offical(OpGroup::STA, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x9d,
            OpCode::offical(OpGroup::STA, 3, 5, AddressingMode::AbsoluteX)
        ),
        (
            0x99,
            OpCode::offical(OpGroup::STA, 3, 5, AddressingMode::AbsoluteY)
        ),
        (
            0x81,
            OpCode::offical(OpGroup::STA, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0x91,
            OpCode::offical(OpGroup::STA, 2, 6, AddressingMode::IndirectY)
        ),
        (
            0x86,
            OpCode::offical(OpGroup::STX, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x96,
            OpCode::offical(OpGroup::STX, 2, 4, AddressingMode::ZeroPageY)
        ),
        (
            0x8E,
            OpCode::offical(OpGroup::STX, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x84,
            OpCode::offical(OpGroup::STY, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x94,
            OpCode::offical(OpGroup::STY, 2, 4, AddressingMode::ZeroPageX)
        ),
        (
            0x8c,
            OpCode::offical(OpGroup::STY, 3, 4, AddressingMode::Absolute)
        ),
        (
            0xAA,
            OpCode::offical(OpGroup::TAX, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xA8,
            OpCode::offical(OpGroup::TAY, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xba,
            OpCode::offical(OpGroup::TSX, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x8a,
            OpCode::offical(OpGroup::TXA, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x9a,
            OpCode::offical(OpGroup::TXS, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x98,
            OpCode::offical(OpGroup::TYA, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x48,
            OpCode::offical(OpGroup::PHA, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x04,
            OpCode::unoffical(OpGroup::NOP, 2, 2, AddressingMode::ZeroPage)
        ),
        (
            0x44,
            OpCode::unoffical(OpGroup::NOP, 2, 2, AddressingMode::ZeroPage)
        ),
        (
            0x64,
            OpCode::unoffical(OpGroup::NOP, 2, 2, AddressingMode::ZeroPage)
        ),
        (
            0x0C,
            OpCode::unoffical(OpGroup::NOP, 3, 2, AddressingMode::Absolute)
        ),
        (
            0x14,
            OpCode::unoffical(OpGroup::NOP, 2, 2, AddressingMode::ZeroPageX)
        ),
        (
            0x34,
            OpCode::unoffical(OpGroup::NOP, 2, 2, AddressingMode::ZeroPageX)
        ),
        (
            0x54,
            OpCode::unoffical(OpGroup::NOP, 2, 2, AddressingMode::ZeroPageX)
        ),
        (
            0x74,
            OpCode::unoffical(OpGroup::NOP, 2, 2, AddressingMode::ZeroPageX)
        ),
        (
            0xD4,
            OpCode::unoffical(OpGroup::NOP, 2, 2, AddressingMode::ZeroPageX)
        ),
        (
            0xF4,
            OpCode::unoffical(OpGroup::NOP, 2, 2, AddressingMode::ZeroPageX)
        ),
        (
            0x1A,
            OpCode::unoffical(OpGroup::NOP, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x3A,
            OpCode::unoffical(OpGroup::NOP, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x5A,
            OpCode::unoffical(OpGroup::NOP, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x7A,
            OpCode::unoffical(OpGroup::NOP, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xDA,
            OpCode::unoffical(OpGroup::NOP, 1, 2, AddressingMode::Implicit)
        ),
        (
            0xFA,
            OpCode::unoffical(OpGroup::NOP, 1, 2, AddressingMode::Implicit)
        ),
        (
            0x80,
            OpCode::unoffical(OpGroup::NOP, 2, 2, AddressingMode::Immediate)
        ),
        (
            0x1C,
            OpCode::unoffical(OpGroup::NOP, 3, 2, AddressingMode::AbsoluteX)
        ),
        (
            0x3C,
            OpCode::unoffical(OpGroup::NOP, 3, 2, AddressingMode::AbsoluteX)
        ),
        (
            0x5C,
            OpCode::unoffical(OpGroup::NOP, 3, 2, AddressingMode::AbsoluteX)
        ),
        (
            0x7C,
            OpCode::unoffical(OpGroup::NOP, 3, 2, AddressingMode::AbsoluteX)
        ),
        (
            0xDC,
            OpCode::unoffical(OpGroup::NOP, 3, 2, AddressingMode::AbsoluteX)
        ),
        (
            0xFC,
            OpCode::unoffical(OpGroup::NOP, 3, 2, AddressingMode::AbsoluteX)
        ),
        (
            0xA7,
            OpCode::unoffical(OpGroup::LAX, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0xB7,
            OpCode::unoffical(OpGroup::LAX, 2, 4, AddressingMode::ZeroPageY)
        ),
        (
            0xAF,
            OpCode::unoffical(OpGroup::LAX, 3, 2, AddressingMode::Absolute)
        ),
        (
            0xBF,
            OpCode::unoffical(OpGroup::LAX, 3, 4, AddressingMode::AbsoluteY)
        ),
        (
            0xA3,
            OpCode::unoffical(OpGroup::LAX, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0xB3,
            OpCode::unoffical(OpGroup::LAX, 2, 2, AddressingMode::IndirectY)
        ),
        (
            0x87,
            OpCode::unoffical(OpGroup::SAX, 2, 3, AddressingMode::ZeroPage)
        ),
        (
            0x97,
            OpCode::unoffical(OpGroup::SAX, 2, 4, AddressingMode::ZeroPageY)
        ),
        (
            0x8F,
            OpCode::unoffical(OpGroup::SAX, 3, 4, AddressingMode::Absolute)
        ),
        (
            0x83,
            OpCode::unoffical(OpGroup::SAX, 2, 6, AddressingMode::IndirectX)
        ),
        (
            0xEB,
            OpCode::unoffical(OpGroup::SBC, 2, 2, AddressingMode::Immediate)
        ),
        (
            0xc7,
            OpCode::unoffical(OpGroup::DCP, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0xD7,
            OpCode::unoffical(OpGroup::DCP, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0xCF,
            OpCode::unoffical(OpGroup::DCP, 3, 6, AddressingMode::Absolute)
        ),
        (
            0xDF,
            OpCode::unoffical(OpGroup::DCP, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0xDB,
            OpCode::unoffical(OpGroup::DCP, 3, 7, AddressingMode::AbsoluteY)
        ),
        (
            0xC3,
            OpCode::unoffical(OpGroup::DCP, 2, 8, AddressingMode::IndirectX)
        ),
        (
            0xd3,
            OpCode::unoffical(OpGroup::DCP, 2, 8, AddressingMode::IndirectY)
        ),
        (
            0xe7,
            OpCode::unoffical(OpGroup::ISB, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0xf7,
            OpCode::unoffical(OpGroup::ISB, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0xef,
            OpCode::unoffical(OpGroup::ISB, 3, 6, AddressingMode::Absolute)
        ),
        (
            0xff,
            OpCode::unoffical(OpGroup::ISB, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0xfb,
            OpCode::unoffical(OpGroup::ISB, 3, 7, AddressingMode::AbsoluteY)
        ),
        (
            0xe3,
            OpCode::unoffical(OpGroup::ISB, 2, 8, AddressingMode::IndirectX)
        ),
        (
            0xf3,
            OpCode::unoffical(OpGroup::ISB, 2, 4, AddressingMode::IndirectY)
        ),
        (
            0x07,
            OpCode::unoffical(OpGroup::SLO, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0x17,
            OpCode::unoffical(OpGroup::SLO, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0x0F,
            OpCode::unoffical(OpGroup::SLO, 3, 6, AddressingMode::Absolute)
        ),
        (
            0x1F,
            OpCode::unoffical(OpGroup::SLO, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0x1B,
            OpCode::unoffical(OpGroup::SLO, 3, 7, AddressingMode::AbsoluteY)
        ),
        (
            0x03,
            OpCode::unoffical(OpGroup::SLO, 2, 8, AddressingMode::IndirectX)
        ),
        (
            0x13,
            OpCode::unoffical(OpGroup::SLO, 2, 8, AddressingMode::IndirectY)
        ),
        (
            0x27,
            OpCode::unoffical(OpGroup::RLA, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0x37,
            OpCode::unoffical(OpGroup::RLA, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0x2f,
            OpCode::unoffical(OpGroup::RLA, 3, 6, AddressingMode::Absolute)
        ),
        (
            0x3F,
            OpCode::unoffical(OpGroup::RLA, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0x3B,
            OpCode::unoffical(OpGroup::RLA, 3, 7, AddressingMode::AbsoluteY)
        ),
        (
            0x23,
            OpCode::unoffical(OpGroup::RLA, 2, 8, AddressingMode::IndirectX)
        ),
        (
            0x33,
            OpCode::unoffical(OpGroup::RLA, 2, 8, AddressingMode::IndirectY)
        ),
        (
            0x47,
            OpCode::unoffical(OpGroup::SRE, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0x57,
            OpCode::unoffical(OpGroup::SRE, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0x4f,
            OpCode::unoffical(OpGroup::SRE, 3, 6, AddressingMode::Absolute)
        ),
        (
            0x5f,
            OpCode::unoffical(OpGroup::SRE, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0x5b,
            OpCode::unoffical(OpGroup::SRE, 3, 7, AddressingMode::AbsoluteY)
        ),
        (
            0x43,
            OpCode::unoffical(OpGroup::SRE, 2, 8, AddressingMode::IndirectX)
        ),
        (
            0x53,
            OpCode::unoffical(OpGroup::SRE, 2, 8, AddressingMode::IndirectY)
        ),
        (
            0x67,
            OpCode::unoffical(OpGroup::RRA, 2, 5, AddressingMode::ZeroPage)
        ),
        (
            0x77,
            OpCode::unoffical(OpGroup::RRA, 2, 6, AddressingMode::ZeroPageX)
        ),
        (
            0x6f,
            OpCode::unoffical(OpGroup::RRA, 3, 6, AddressingMode::Absolute)
        ),
        (
            0x7f,
            OpCode::unoffical(OpGroup::RRA, 3, 7, AddressingMode::AbsoluteX)
        ),
        (
            0x7b,
            OpCode::unoffical(OpGroup::RRA, 3, 7, AddressingMode::AbsoluteY)
        ),
        (
            0x63,
            OpCode::unoffical(OpGroup::RRA, 2, 8, AddressingMode::IndirectX)
        ),
        (
            0x73,
            OpCode::unoffical(OpGroup::RRA, 2, 8, AddressingMode::IndirectY)
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
    DCP,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    ISB,
    JMP,
    JSR,
    LAX,
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
    RLA,
    ROL,
    ROR,
    RRA,
    RTI,
    RTS,
    SAX,
    SBC,
    SEC,
    SED,
    SEI,
    SLO,
    SRE,
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
    pub bytes: Value,
    pub cycles: u8,
    pub mode: AddressingMode,
    pub is_offical: bool,
}

impl OpCode {
    const fn offical(group: OpGroup, bytes: u8, cycles: u8, mode: AddressingMode) -> OpCode {
        OpCode {
            group,
            bytes: Value::from_const(bytes),
            cycles,
            mode,
            is_offical: true,
        }
    }

    const fn unoffical(group: OpGroup, bytes: u8, cycles: u8, mode: AddressingMode) -> OpCode {
        OpCode {
            group,
            bytes: Value::from_const(bytes),
            cycles,
            mode,
            is_offical: false,
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
        None => panic!("Opcode {:02X} is not defined yet", raw_opcode),
    }
}
