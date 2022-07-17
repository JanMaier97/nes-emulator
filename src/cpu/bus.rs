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

pub trait Mem {
    fn mem_read(&self, addr: u16) -> u8;
    fn mem_read_u16(&self, addr: u16) -> u16;
    fn mem_write(&mut self, addr: u16, data: u8);
    fn mem_write_u16(&mut self, addr: u16, data: u16);
}

pub fn combine_high_low(high: u8, low: u8) -> u16 {
    let high = high as u16;
    let low = low as u16;
    (high << 8) | low
}

pub fn split_high_low(value: u16) -> (u8, u8) {
    let low = value & 0xFF;
    let high = value >> 8;

    (high as u8, low as u8)
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

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.rom.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            //mirror if needed
            addr = addr % 0x4000;
        }
        self.rom.prg_rom[addr as usize]
    }
}

impl Mem for Bus {
    fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & RAM_ADDRESS_MASK;
                self.cpu_vram[mirror_down_addr as usize]
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & PPU_ADDRESS_MASK;
                todo!("PPU is not supported yet")
            }
            ROM_START..=ROM_END => self.read_prg_rom(addr),
            _ => {
                println!("Ignoring mem access at {}", addr);
                0
            }
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & RAM_ADDRESS_MASK;
                self.cpu_vram[mirror_down_addr as usize] = data;
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & PPU_ADDRESS_MASK;
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

    fn mem_read_u16(&self, addr: u16) -> u16 {
        let addr = addr as usize;
        let low = self.cpu_vram[addr];
        let high = self.cpu_vram[addr + 1];

        combine_high_low(high, low)
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        let (low, high) = split_high_low(data);
        self.mem_write(addr, low);
        self.mem_write(addr, high);
    }
}
