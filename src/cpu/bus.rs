use crate::rom::Rom;

use self::{
    address::Address,
    values::{Value, Value16},
};

pub mod address;
pub mod values;

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

pub trait Mem {
    fn mem_read(&self, addr: Address) -> Value;
    fn mem_read_u16(&self, addr: Address) -> Value16;
    fn mem_read_addr(&self, addr: Address) -> Address;
    fn mem_read_addr_zero_page(&self, addr: Value) -> Address;
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
        let mut addr = u16::from(addr) - 0x8000;

        if self.rom.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            //mirror if needed
            addr %= 0x4000
        };

        Value::from(self.rom.prg_rom[addr as usize])
    }
}

impl Mem for Bus {
    fn mem_read(&self, addr: Address) -> Value {
        let raw_addr: u16 = addr.into();
        match raw_addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = raw_addr & RAM_ADDRESS_MASK;
                Value::from(self.cpu_vram[mirror_down_addr as usize])
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = raw_addr & PPU_ADDRESS_MASK;
                todo!("PPU is not supported yet")
            }
            ROM_START..=ROM_END => self.read_prg_rom(addr),
            _ => {
                println!("Ignoring mem access at {}", addr);
                Value::from(0)
            }
        }
    }

    fn mem_write(&mut self, addr: Address, data: Value) {
        let raw_addr: u16 = addr.into();
        match raw_addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = raw_addr & RAM_ADDRESS_MASK;
                self.cpu_vram[mirror_down_addr as usize] = data.into();
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = raw_addr & PPU_ADDRESS_MASK;
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

    fn mem_read_addr_zero_page(&self, addr: Value) -> Address {
        let val1 = self.mem_read(addr.into());
        let val2 = self.mem_read(addr.wrapping_add(1.into()).into());
        Address::from_values(val2, val1)
    }

    fn mem_read_addr(&self, addr: Address) -> Address {
        self.mem_read_u16(addr).into()
    }

    fn mem_read_u16(&self, addr: Address) -> Value16 {
        let high = self.mem_read(addr);
        let low = self.mem_read(addr + 1.into());

        Value16::from_values(high, low)
    }

    fn mem_write_u16(&mut self, addr: Address, data: Value16) {
        let (high, low) = data.split();
        self.mem_write(addr, high.into());
        self.mem_write(addr + 1.into(), low.into());
    }
}
