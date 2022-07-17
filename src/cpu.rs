use crate::rom::Rom;

use self::{
    bus::{Bus, Mem, combine_high_low},
    opcodes::{OpCode, CPU_OPS_CODES, OpGroup, AddressingMode}, flags::CpuFlags,
};

mod bus;
mod flags;
mod opcodes;

pub struct CPU {
    register_a: u8,
    register_x: u8,
    register_y: u8,
    status: CpuFlags,
    program_counter: u16,
    stack_pointer: u16,
    bus: Bus,
}

impl CPU {
    fn from_rom(rom: &Rom) -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: CpuFlags::from_bits(0),
            program_counter: 0,
            stack_pointer: 0,
            bus: Bus::new(rom),
        }
    }

    fn execute_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        self.execute();
        callback(self);
    }

    fn execute(&mut self) {
        let opcode = self.read_opcode();
        self.handle_opcode(&opcode);
        self.program_counter += opcode.bytes -1;
    }

    fn read_opcode(&mut self) -> OpCode {
        let opcode = self.bus.mem_read(self.program_counter);
        self.program_counter += 1;

        *CPU_OPS_CODES.get(&opcode).unwrap()
    }

    fn handle_opcode(&mut self, opcode: &OpCode) {
        match opcode.group {
            OpGroup::ADC => self.adc(opcode),
            OpGroup::AND => self.and(opcode),
            OpGroup::ASL => self.asl(opcode),
            OpGroup::BCC => self.bcc(opcode),
            OpGroup::BCS => self.bcs(opcode),
            OpGroup::BEQ => self.beq(opcode),
            OpGroup::BIT => self.bit(opcode),
            OpGroup::BMI => self.bmi(opcode),
            OpGroup::BNE => self.bne(opcode),
            OpGroup::BPL => self.bpl(opcode),
            OpGroup::BRK => self.brk(opcode),
            OpGroup::BVC => self.bvc(opcode),
            OpGroup::BVS => self.bvs(opcode),
            OpGroup::CLC => self.clc(opcode),
            OpGroup::CLD => self.cld(opcode),
            OpGroup::CLI => self.cli(opcode),
            OpGroup::CLV => self.clv(opcode),
            OpGroup::CMP => self.cmp(opcode),
            OpGroup::CPX => self.cpx(opcode),
            OpGroup::CPY => self.cpy(opcode),
            OpGroup::DEC => self.dec(opcode),
            OpGroup::DEX => self.dex(opcode),
            OpGroup::DEY => self.dey(opcode),
            OpGroup::EOR => self.eor(opcode),
            OpGroup::INC => self.inc(opcode),
            OpGroup::INX => self.inx(opcode),
            OpGroup::INY => self.iny(opcode),
            OpGroup::JMP => self.jmp(opcode),
            OpGroup::JSR => self.jsr(opcode),
            OpGroup::LDA => self.lda(opcode),
            OpGroup::LDX => self.ldx(opcode),
            OpGroup::LDY => self.ldy(opcode),
            OpGroup::LSR => self.lsr(opcode),
            OpGroup::NOP => self.nop(),
            OpGroup::ORA => self.ora(opcode),
            OpGroup::PHA => self.pha(opcode),
            OpGroup::PHP => self.php(opcode),
            OpGroup::PLA => self.pla(opcode),
            OpGroup::PLP => self.plp(opcode),
            OpGroup::ROL => self.rol(opcode),
            OpGroup::ROR => self.ror(opcode),
            OpGroup::RTI => self.rti(),
            OpGroup::RTS => self.rts(),
            OpGroup::SBC => self.sbc(opcode),
            OpGroup::SEC => self.sec(),
            OpGroup::SED => self.sed(),
            OpGroup::SEI => self.sei(),
            OpGroup::STA => self.sta(opcode),
            OpGroup::STX => self.stx(opcode),
            OpGroup::STY => self.sty(opcode),
            OpGroup::TAX => self.tax(),
            OpGroup::TAY => self.tay(),
            OpGroup::TSX => self.tsx(),
            OpGroup::TXA => self.txa(),
            OpGroup::TXS => self.txs(),
            OpGroup::TYA => self.tya(),
            _ => panic!("Opcode {:?} is not implemented yet", opcode),
        }
    }

    fn handle_comparision(&mut self, lhs: u8, rhs: u8) {
        // this function call has to stay at the top
        // most likely not the beast idea;
        self.status.negative = (rhs >> 7) == 1;
        self.status.carry = lhs >= rhs;
        self.status.zero = lhs == rhs;
    }

    fn asl(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);
        let msb = operand & 0b10000000; 
        let result = operand << 1;

        if matches!(opcode.mode, AddressingMode::Accumulator) {
            self.register_a = result;
        } else {
            let addr = self.convert_to_memory_address(opcode.mode);
            self.bus.mem_write(addr, result);
        }

        self.status.carry = msb > 0;
        self.handle_zero_and_negative_flags(result);
    }

    fn and(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);
        self.register_a = self.register_a & operand;

        self.handle_zero_and_negative_flags(self.register_a);
    }

    fn adc(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);
        let (result, is_overflow) = self.register_a.overflowing_add(operand);

        let mut result = result;
        let mut is_overflow_after_carry = false;
        if self.status.carry {
            (result, is_overflow_after_carry) = result.overflowing_add(1);
        }

        self.status.carry = is_overflow || is_overflow_after_carry;
        self.register_a = result;
        self.handle_zero_and_negative_flags(result);
    }

    fn bcc(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);

        if !self.status.carry {
            self.program_counter += operand as u16; 
        }
    }

    fn bcs(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);

        if self.status.carry {
            self.program_counter += operand as u16; 
        }
    }
    
    fn beq(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);

        if self.status.zero {
            self.program_counter += operand as u16; 
        }
    }

    fn bit(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);

        let result = self.register_a & operand;
        self.handle_zero_and_negative_flags(result);
        self.status.zero = result == 0;
        self.status.overflow = result & 0b0100_0000 > 0;
    }

    fn bmi(&mut self, opcode: &OpCode) {
        if self.status.negative {
            let operand = self.get_operand_value(opcode);

            self.program_counter += operand as u16;
        }
    }

    fn bne(&mut self, opcode: &OpCode) {
        if !self.status.zero {
            let operand = self.get_operand_value(opcode);

            self.program_counter += operand as u16;
        }
    }

    fn bpl(&mut self, opcode: &OpCode) {
        if !self.status.negative {
            let operand = self.get_operand_value(opcode);

            self.program_counter += operand as u16;
        }
    }

    fn brk(&mut self, opcode: &OpCode) {
        todo!("Implement brk");
    }

    
    fn bvc(&mut self, opcode: &OpCode) {
        if !self.status.overflow {
            let operand = self.get_operand_value(opcode);

            self.program_counter += operand as u16;
        }
    }

    fn bvs(&mut self, opcode: &OpCode) {
        if self.status.overflow {
            let operand = self.get_operand_value(opcode);

            self.program_counter += operand as u16;
        }
    }

    fn clc(&mut self, opcode: &OpCode) {
        self.status.carry = false;
    }

    fn cld(&mut self, opcode: &OpCode) {
        self.status.decimal_mode = false;
    }

    fn cli(&mut self, opcode: &OpCode) {
        self.status.interrupt = false;
    }

    fn clv(&mut self, opcode: &OpCode) {
        self.status.overflow = false;
    }

    fn cmp(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);
        self.handle_comparision(self.register_a, operand);
    }

    fn cpx(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);
        self.handle_comparision(self.register_x, operand);
    }

    fn cpy(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);
        self.handle_comparision(self.register_y, operand);
    }

    fn dec(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);
        let (result, _) = operand.overflowing_sub(1);
        let addr = self.convert_to_memory_address(opcode.mode);
        
        self.bus.mem_write(addr, result);
        self.handle_zero_and_negative_flags(result);
    }

    fn dex(&mut self, opcode: &OpCode) {
        let (result, _) = self.register_x.overflowing_sub(1);
        
        self.register_x = result;
        self.handle_zero_and_negative_flags(result);
    }

    fn dey(&mut self, opcode: &OpCode) {
        let (result, _) = self.register_y.overflowing_sub(1);
        
        self.register_y = result;
        self.handle_zero_and_negative_flags(result);
    }

    fn eor(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);
        self.register_a ^= operand;

        self.handle_zero_and_negative_flags(self.register_a);
    }

    fn inc(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);
        let (result, _) = operand.overflowing_add(1);
        let addr = self.convert_to_memory_address(opcode.mode);
        
        self.bus.mem_write(addr, result);
        self.handle_zero_and_negative_flags(result);
    }

    fn inx(&mut self, opcode: &OpCode) {
        let (result, _) = self.register_x.overflowing_add(1);
        
        self.register_x = result;
        self.handle_zero_and_negative_flags(result);
    }

    fn iny(&mut self, opcode: &OpCode) {
        let (result, _) = self.register_y.overflowing_add(1);
        
        self.register_y = result;
        self.handle_zero_and_negative_flags(result);
    }

    fn jmp(&mut self, opcode: &OpCode) {
         self.program_counter = self.get_operand_value_u16(opcode);
    }

    fn jsr(&mut self, opcode: &OpCode) {
        todo!("Implement jsr");
    }

    fn lda(&mut self, opcode: &OpCode) {
        self.register_a = self.get_operand_value(opcode);
        self.handle_zero_and_negative_flags(self.register_a);
    }

    fn ldx(&mut self, opcode: &OpCode) {
        self.register_x = self.get_operand_value(opcode);
        self.handle_zero_and_negative_flags(self.register_x);
    }

    fn ldy(&mut self, opcode: &OpCode) {
        self.register_y = self.get_operand_value(opcode);
        self.handle_zero_and_negative_flags(self.register_y);
    }

    fn lsr(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);

        self.status.carry = operand & 1 == 1;

        let result = operand >> 2;
        if matches!(opcode.mode, AddressingMode::Accumulator) {
            self.register_a = result;
        }

        self.handle_zero_and_negative_flags(result);
    }

    fn nop(&mut self) {
    }

    fn ora(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);
        self.register_a |= operand;

        self.handle_zero_and_negative_flags(self.register_a);
    }

    fn pha(&mut self, opcode: &OpCode) {
        todo!("not implemented yet");
    }

    fn php(&mut self, opcode: &OpCode) {
        todo!("not implemented yet");
    }

    fn pla(&mut self, opcode: &OpCode) {
        todo!("not implemented yet");
    }

    fn plp(&mut self, opcode: &OpCode) {
        todo!("not implemented yet");
    }

    fn rol(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);
        let result = if self.status.carry {
            operand << 1 | 1
        } else {
            operand << 1
        };

        self.status.carry = (operand >> 7) == 1;
        self.handle_zero_and_negative_flags(result);

        if matches!(opcode.mode, AddressingMode::Accumulator) {
            self.register_a = result;
        }
    }

    fn ror(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);
        let result = if self.status.carry {
            operand >> 1 | 0b1000_0000
        } else {
            operand >> 1
        };

        self.status.carry = (operand & 1) == 1;
        self.handle_zero_and_negative_flags(result);

        if matches!(opcode.mode, AddressingMode::Accumulator) {
            self.register_a = result;
        }
    }

    fn rti(&mut self) {
        todo!();
    }

    fn rts(&mut self) {
        todo!();
    }

    fn sbc(&mut self, opcode: &OpCode) {
        let operand = self.get_operand_value(opcode);

        let carry = if self.status.carry {
            1
        } else {
            0
        };

        let (result, overflow1) = self.register_a.overflowing_sub(operand);
        let (result, overflow2) = result.overflowing_sub(1-carry);
        
        self.register_a = result;
        self.status.carry = overflow1 || overflow2;
        self.handle_zero_and_negative_flags(result);
    }

    fn sec(&mut self) {
        self.status.carry = true;
    }

    fn sed(&mut self) {
        self.status.decimal_mode = true;
    }

    fn sei(&mut self) {
        self.status.interrupt = true;
    }

    fn sta(&mut self, opcode: &OpCode) {
        todo!();
    }

    fn stx(&mut self, opcode: &OpCode) {
        todo!();
    }

    fn sty(&mut self, opcode: &OpCode) {
        todo!();
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.handle_zero_and_negative_flags(self.register_x);
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.handle_zero_and_negative_flags(self.register_y);
    }

    fn tsx(&mut self) {
        todo!();
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.handle_zero_and_negative_flags(self.register_a);
    }

    fn txs(&mut self) {
        todo!();
    }

    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.handle_zero_and_negative_flags(self.register_a);
    }

    fn handle_zero_and_negative_flags(&mut self, value: u8) {
        self.status.zero = value == 0;
        self.status.negative = (value >> 8) > 0;
    }

    fn get_operand_value(&self, opcode: &OpCode) -> u8 {
        match opcode.mode {
            AddressingMode::Implicit => panic!("Cannot get operand of opcode {:?} because it is implicit", opcode),
            AddressingMode::Accumulator => self.register_a,
            _ => self.bus.mem_read(self.convert_to_memory_address(opcode.mode))
        }
    }

    fn get_operand_value_u16(&self, opcode: &OpCode) -> u16 {
        match opcode.mode {
            AddressingMode::Implicit => panic!("Cannot get operand of opcode {:?} because it is implicit", opcode),
            AddressingMode::Accumulator => panic!("Cannot access register a while trying to fetch a 2 byte operand"),
            _ => self.bus.mem_read_u16(self.convert_to_memory_address(opcode.mode))
        }
    }

    fn convert_to_memory_address(&self, mode: AddressingMode) -> u16 {
        match mode {
            AddressingMode::Implicit => panic!("Cannot convert implicit addressing mode to an address"),
            AddressingMode::Accumulator => panic!("Cannot convert accumulator addressing mode to address"),
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.bus.mem_read(self.program_counter) as u16 & 0x00FF, 
            AddressingMode::ZeroPageX => (self.bus.mem_read(self.program_counter) as u16 + self.register_x as u16) & 0x00FF,
            AddressingMode::ZeroPageY => (self.bus.mem_read(self.program_counter) as u16 + self.register_y as u16) & 0x00FF,
            AddressingMode::Relative => self.program_counter + self.bus.mem_read(self.program_counter) as u16,
            AddressingMode::Absolute => self.bus.mem_read_u16(self.program_counter),
            AddressingMode::AbsoluteX => self.bus.mem_read_u16(self.program_counter) + self.register_x as u16,
            AddressingMode::AbsoluteY => self.bus.mem_read_u16(self.program_counter) + self.register_y as u16,
            AddressingMode::Indirect => self.bus.mem_read_u16(self.bus.mem_read_u16(self.program_counter)),
            AddressingMode::IndirectX => {
                let operand = self.bus.mem_read(self.program_counter);
                let (table_addr,_) = operand.overflowing_add(self.register_x);

                let low = self.bus.mem_read(table_addr as u16);
                let high = self.bus.mem_read((table_addr+1) as u16);

                combine_high_low(high, low)
            }

            AddressingMode::IndirectY => {
                let operand = self.bus.mem_read(self.program_counter) as u16 + self.register_y as u16;
                let low = self.bus.mem_read(operand);
                let high = self.bus.mem_read(operand+1);
                
                combine_high_low(high, low) + self.register_y as u16
            }

        }
    }
}

#[cfg(test)]
mod tests {
    use super::{bus::Mem, *};

    #[test]
    fn execute_nestest_rom() {
        let rom = Rom::from_file("./test_roms/nestest.nes").expect("Failed to load rom for test");
        let mut cpu = CPU::from_rom(&rom);

        cpu.program_counter = 0x0c000;

        for _ in 0..1000 {
            cpu.execute_with_callback(move |cpu| {
                let test_result = cpu.bus.mem_read(0x0002);

                assert_eq!(0, test_result);
            });
        }
    }
}
