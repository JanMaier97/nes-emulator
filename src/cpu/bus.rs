use core::fmt;
use std::{
    hash::Hash,
    ops::{Add, AddAssign, BitAnd, BitOrAssign, BitXorAssign, Shl, Shr, Sub, Not},
};

use crate::rom::Rom;

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;
const ROM_START: u16 = 0x8000;
const ROM_END: u16 = 0xFFFF;
const VRAM_SIZE: i32 = 2048;
const RAM_ADDRESS_MASK: u16 = 0b00000111_11111111;
const PPU_ADDRESS_MASK: u16 = 0b00100000_00000111;
pub const DEFAUKT_STACK_POINTER: u16 = 0;

// Raw value read from memory
#[derive(Debug, Eq, Hash, PartialOrd, Clone, Copy)]
pub struct Value(u8);

#[derive(Debug, Clone, Copy)]
pub struct Value16(u16);

#[derive(Debug, Clone, Copy)]
pub struct Address(u16);

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Not for Value {
    type Output = Self;

    fn not(self) -> Self::Output {
        Value(!self.0)
    }
}

impl Add for Address {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Address(self.0 + rhs.0)
    }
}

impl BitOrAssign for Value {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0;
    }
}
impl BitXorAssign for Value {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0;
    }
}

impl Sub for Value {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Value(self.0 + rhs.0)
    }
}

impl Add for Value {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Value(self.0 + rhs.0)
    }
}

impl From<Value> for Address {
    fn from(value: Value) -> Self {
        Address(value.0 as u16)
    }
}

impl AddAssign for Address {
    fn add_assign(&mut self, rhs: Self) {
        *self = Address(self.0 + rhs.0)
    }
}

impl AddAssign for Value {
    fn add_assign(&mut self, rhs: Self) {
        *self = Value(self.0 + rhs.0)
    }
}

impl From<Value> for u8 {
    fn from(val: Value) -> Self {
        val.0
    }
}

impl From<Address> for u16 {
    fn from(val: Address) -> Self {
        val.0
    }
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl From<u8> for Value {
    fn from(val: u8) -> Self {
        Value(val)
    }
}

impl BitAnd for Value {
    type Output = Self;

    // rhs is the "right-hand side" of the expression `a & b`
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl Shr for Value {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        Self(self.0 >> rhs.0)
    }
}

impl Shl for Value {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        Self(self.0 << rhs.0)
    }
}

impl fmt::UpperHex for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = self.0;
        fmt::UpperHex::fmt(&val, f)
    }
}

impl fmt::UpperHex for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = self.0;
        fmt::UpperHex::fmt(&val, f)
    }
}

impl From<u16> for Value16 {
    fn from(val: u16) -> Self {
        Value16(val)
    }
}

impl From<u16> for Address {
    fn from(addr: u16) -> Self {
        Address(addr)
    }
}

impl From<Value16> for Address {
    fn from(val: Value16) -> Self {
        let (high, low) = split_high_low(val.0);
        Address(combine_high_low(low, high))
    }
}

impl From<Address> for Value16 {
    fn from(addr: Address) -> Self {
        let (high, low) = split_high_low(addr.0);
        Value16::from_values(low.into(), high.into())
    }
}

impl Address {
    pub const fn from_const(value: u16) -> Address {
        Address(value)
    }

    pub fn from_values(high: Value, low: Value) -> Address {
        Address(combine_high_low(high.0, low.0))
    }
}

fn split_high_low(value: u16) -> (u8, u8) {
    let low = value & 0xFF;
    let high = value >> 8;

    (high as u8, low as u8)
}

fn combine_high_low(high: u8, low: u8) -> u16 {
    let high = high as u16;
    let low = low as u16;
    (high << 8) | low
}

impl Value16 {
    pub fn from_values(high: Value, low: Value) -> Value16 {
        let high = high.0 as u16;
        let low = low.0 as u16;
        Value16((high << 8) | low)
    }

    pub fn split(self) -> (Value, Value) {
        let (high, low) = split_high_low(self.0);
        (high.into(), low.into())
    }
}

impl Value {
    pub const fn from_const(value: u8) -> Value {
        Value(value)
    }

    pub fn wrapping_add(self, value: Value) -> Value {
        self.0.wrapping_add(value.0).into()
    }

    pub fn wrapping_sub(self, value: Value) -> Value {
        self.0.wrapping_sub(value.0).into()
    }

    pub fn overflowing_add(self, value: Value) -> (Value, bool) {
        let (val, b) = self.0.overflowing_add(value.0);
        (val.into(), b)
    }

    pub fn overflowing_sub(self, value: Value) -> (Value, bool) {
        let (val, b) = self.0.overflowing_sub(value.0);
        (val.into(), b)
    }
}

pub trait Mem {
    fn mem_read(&self, addr: Address) -> Value;
    fn mem_read_u16(&self, addr: Address) -> Value16;
    fn mem_read_addr(&self, addr: Address) -> Address;
    fn mem_write(&mut self, addr: Address, data: Value);
    fn mem_write_u16(&mut self, addr: Address, data: Value16);
}

pub struct Bus {
    cpu_vram: [u8; 2048],
    rom: Rom,
}

impl Bus {
    pub fn new(rom: &Rom) -> Self {
        Bus {
            cpu_vram: [0; 2048],
            rom: rom.clone(),
        }
    }

    fn read_prg_rom(&self, addr: Address) -> Value {
        let mut addr = addr.0 - 0x8000;
        if self.rom.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            //mirror if needed
            addr %= 0x4000
        };

        Value(self.rom.prg_rom[addr as usize])
    }
}

impl Mem for Bus {
    fn mem_read(&self, addr: Address) -> Value {
        match addr.0 {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr.0 & RAM_ADDRESS_MASK;
                Value(self.cpu_vram[mirror_down_addr as usize])
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr.0 & PPU_ADDRESS_MASK;
                todo!("PPU is not supported yet")
            }
            ROM_START..=ROM_END => self.read_prg_rom(addr),
            _ => {
                println!("Ignoring mem access at {}", addr);
                Value(0)
            }
        }
    }

    fn mem_write(&mut self, addr: Address, data: Value) {
        match addr.0 {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr.0 & RAM_ADDRESS_MASK;
                self.cpu_vram[mirror_down_addr as usize] = data.0;
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr.0 & PPU_ADDRESS_MASK;
                todo!("PPU is not supported yet");
            }
            ROM_START..=ROM_END => {
                panic!("Attempt to write to Cartridge ROM space");
            }
            _ => {
                println!("Ignoring mem write-access at {}", addr);
            }
        }
    }

    fn mem_read_addr(&self, addr: Address) -> Address {
        self.mem_read_u16(addr).into()
    }

    fn mem_read_u16(&self, addr: Address) -> Value16 {
        let high = self.mem_read(addr);
        let low = self.mem_read(addr + 1.into());

        Value16(combine_high_low(high.0, low.0))
    }

    fn mem_write_u16(&mut self, addr: Address, data: Value16) {
        let (high, low) = split_high_low(data.0);
        self.mem_write(addr, high.into());
        self.mem_write(addr + 1.into(), low.into());
    }
}
