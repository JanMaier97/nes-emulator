use std::fmt::format;

use either::Either;
use Either::{Left, Right};

use crate::rom::Rom;

use self::{
    bus::{Address, Bus, Mem, Value, Value16},
    flags::{CpuFlags, NEGATIVE_MASK},
    opcodes::{AddressingMode, OpCode, OpGroup, CPU_OPS_CODES, get_opcode},
};

mod bus;
mod flags;
mod opcodes;

const STACK_BASE_ADDR: Address = Address::from_const(0xFF00);
const STACK_MSB: Value = Value::from_const(0x01);

struct Instruction {
    raw_opcode: Value,
    raw_operand: Option<Either<Value, Value16>>,
    opcode: OpCode,
    operand: Option<Either<Value, Address>>,
}

pub struct CPU {
    register_a: Value,
    register_x: Value,
    register_y: Value,
    status: CpuFlags,
    program_counter: Address,
    stack_pointer: Value,
    bus: Bus,
}

impl CPU {
    fn from_rom(rom: &Rom) -> Self {
        CPU {
            register_a: 0.into(),
            register_x: 0.into(),
            register_y: 0.into(),
            status: CpuFlags::from(0x24),
            program_counter: 0.into(),
            stack_pointer: 0xFD.into(),
            bus: Bus::new(rom),
        }
    }

    pub fn execute_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        callback(self);
        self.execute();
    }

    pub fn execute(&mut self) {
        // let opcode = self.read_opcode();
        let instruction = self.read_instruction();
        self.handle_opcode(&instruction);
        match instruction.opcode.group {
            OpGroup::BCC => {}
            OpGroup::BCS => {}
            OpGroup::BEQ => {}
            OpGroup::BMI => {}
            OpGroup::BNE => {}
            OpGroup::BPL => {}
            OpGroup::BVC => {}
            OpGroup::BVS => {}
            OpGroup::JMP => {}
            OpGroup::JSR => {}
            OpGroup::RTS => {}
            OpGroup::RTI => {}
            _ => self.program_counter += instruction.opcode.bytes.into(),
        }
    }

    pub fn read_instruction(&self) -> Instruction {
        let raw_opcode = self.bus.mem_read(self.program_counter);
        let opcode = get_opcode(raw_opcode);
        let raw_operand = self.get_raw_operand(opcode);
        let operand = self.get_operand(raw_operand, &opcode);

        Instruction {
            raw_opcode,
            opcode,
            raw_operand,
            operand,
        }
    }

    fn get_raw_operand(&self, opcode: OpCode) -> Option<Either<Value, Value16>> {
        let operand_addr = self.program_counter + 1.into();
        match opcode.mode {
            AddressingMode::Absolute => Some(Right(self.bus.mem_read_u16(operand_addr))),
            AddressingMode::AbsoluteX => Some(Right(self.bus.mem_read_u16(operand_addr))),
            AddressingMode::AbsoluteY => Some(Right(self.bus.mem_read_u16(operand_addr))),
            AddressingMode::Accumulator => None,
            AddressingMode::Immediate => Some(Left(self.bus.mem_read(operand_addr))),
            AddressingMode::Implicit => None,
            AddressingMode::Indirect => Some(Right(self.bus.mem_read_u16(operand_addr))),
            AddressingMode::IndirectX => Some(Left(self.bus.mem_read(operand_addr))),
            AddressingMode::IndirectY => Some(Left(self.bus.mem_read(operand_addr))),
            AddressingMode::Relative => Some(Left(self.bus.mem_read(operand_addr))),
            AddressingMode::ZeroPage => Some(Left(self.bus.mem_read(operand_addr))),
            AddressingMode::ZeroPageX => Some(Left(self.bus.mem_read(operand_addr))),
            AddressingMode::ZeroPageY => Some(Left(self.bus.mem_read(operand_addr))),
        }
    }

    fn get_operand(
        &self,
        raw_operand: Option<Either<Value, Value16>>,
        opcode: &OpCode,
    ) -> Option<Either<Value, Address>> {
        match opcode.mode {
            AddressingMode::Absolute => Some(Right(
                raw_operand
                    .unwrap()
                    .expect_right("Absolute canot have left value")
                    .into(),
            )),
            AddressingMode::AbsoluteX => {
                let base_addr: Address = raw_operand.unwrap().unwrap_right().into();
                let offset: Address = self.register_x.into();

                Some(Right(base_addr.wrapping_add(offset)))
            },
            AddressingMode::AbsoluteY => {
                let base_addr: Address = raw_operand.unwrap().unwrap_right().into();
                let offset: Address = self.register_y.into();

                Some(Right(base_addr.wrapping_add(offset)))
            },
            AddressingMode::Accumulator => None,
            AddressingMode::Immediate => Some(Left(
                raw_operand
                    .unwrap()
                    .expect_left("Immediate canot have right value")
                    .into(),
            )),
            AddressingMode::Implicit => None,
            AddressingMode::IndirectX => {
                let value = raw_operand.unwrap().unwrap_left();
                let addr = value.wrapping_add(self.register_x);

                Some(Right(self.bus.mem_read_addr_zero_page(addr)))
            }
            AddressingMode::IndirectY => {
                let value = raw_operand.unwrap().unwrap_left();
                let base_addr = self.bus.mem_read_addr_zero_page(value);
                
                Some(Right(base_addr.wrapping_add(self.register_y.into())))
            },
            AddressingMode::ZeroPage => Some(Right(
                raw_operand
                    .unwrap()
                    .expect_left("Immediate canot have right value")
                    .into(),
            )),
            AddressingMode::ZeroPageX => {
                let operand = raw_operand.unwrap().unwrap_left();
                let addr: Address = operand.wrapping_add(self.register_x).into();

                Some(Right(addr))
            },
            AddressingMode::ZeroPageY => {
                let operand = raw_operand.unwrap().unwrap_left();
                let addr: Address = operand.wrapping_add(self.register_y).into();

                Some(Right(addr))
            },
            AddressingMode::Relative => Some(Right(
                self.program_counter
                    + opcode.bytes.into()
                    + raw_operand
                        .unwrap()
                        .expect_left("Relative cannot have right value")
                        .into(),
            )),
            AddressingMode::Indirect => {
                let indirect_addr: Address = raw_operand.unwrap().unwrap_right().into();
                let (msb, lsb) = indirect_addr.split();
                
                let addr = if lsb == 0xFF.into() {
                    let addr_lsb = self.bus.mem_read(indirect_addr);
                    let addr_msb = self.bus.mem_read(Address::from_values(msb, 00.into())); 
                    Address::from_values(addr_msb, addr_lsb)
                } else {
                    self.bus.mem_read_addr(indirect_addr)
                };

                Some(Right(addr))
            },
        }
    }

    fn push_stack(&mut self, data: Value) {
        let addr = Address::from_values(STACK_MSB.into(), self.stack_pointer.into());

        self.bus.mem_write(addr, data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1.into());
    }

    fn pop_stack(&mut self) -> Value {
        self.stack_pointer = self.stack_pointer.wrapping_add(1.into());
        let addr = Address::from_values(STACK_MSB.into(), self.stack_pointer);
        self.bus.mem_read(addr)
    }

    fn read_opcode(&self) -> OpCode {
        let opcode = self.bus.mem_read(self.program_counter);
        // self.program_counter += 1.into();

        get_opcode(opcode)
    }

    fn handle_opcode(&mut self, instruction: &Instruction) {
        match instruction.opcode.group {
            OpGroup::ADC => self.adc(instruction.operand.unwrap()),
            OpGroup::AND => self.and(instruction.operand.unwrap()),
            OpGroup::ASL => self.asl(if instruction.operand.is_none() { None } else { Some(instruction.operand.unwrap().unwrap_right())}),
            OpGroup::BCC => self.bcc(instruction.operand.unwrap().unwrap_right()),
            OpGroup::BCS => self.bcs(instruction.operand.unwrap().unwrap_right()),
            OpGroup::BEQ => self.beq(instruction.operand.unwrap().unwrap_right()),
            OpGroup::BIT => self.bit(instruction.operand.unwrap().unwrap_right()),
            OpGroup::BMI => self.bmi(instruction.operand.unwrap().unwrap_right()),
            OpGroup::BNE => self.bne(instruction.operand.unwrap().unwrap_right()),
            OpGroup::BPL => self.bpl(instruction.operand.unwrap().unwrap_right()),
            OpGroup::BRK => self.brk(),
            OpGroup::BVC => self.bvc(instruction.operand.unwrap().unwrap_right()),
            OpGroup::BVS => self.bvs(instruction.operand.unwrap().unwrap_right()),
            OpGroup::CLC => self.clc(),
            OpGroup::CLD => self.cld(),
            OpGroup::CLI => self.cli(),
            OpGroup::CLV => self.clv(),
            OpGroup::CMP => self.cmp(instruction.operand.unwrap()),
            OpGroup::CPX => self.cpx(instruction.operand.unwrap()),
            OpGroup::CPY => self.cpy(instruction.operand.unwrap()),
            OpGroup::DEC => self.dec(instruction.operand.unwrap().unwrap_right()),
            OpGroup::DEX => self.dex(),
            OpGroup::DEY => self.dey(),
            OpGroup::EOR => self.eor(instruction.operand.unwrap()),
            OpGroup::INC => self.inc(instruction.operand.unwrap().unwrap_right()),
            OpGroup::INX => self.inx(),
            OpGroup::INY => self.iny(),
            OpGroup::JMP => self.jmp(instruction.operand.unwrap().unwrap_right()),
            OpGroup::JSR => self.jsr(instruction.operand.unwrap().unwrap_right()),
            OpGroup::LDA => self.lda(instruction.operand.unwrap()),
            OpGroup::LDX => self.ldx(instruction.operand.unwrap()),
            OpGroup::LDY => self.ldy(instruction.operand.unwrap()),
            OpGroup::LSR => self.lsr(if instruction.operand.is_none() { None } else { Some(instruction.operand.unwrap().unwrap_right())}),
            OpGroup::NOP => self.nop(),
            OpGroup::ORA => self.ora(instruction.operand.unwrap()),
            OpGroup::PHA => self.pha(),
            OpGroup::PHP => self.php(),
            OpGroup::PLA => self.pla(),
            OpGroup::PLP => self.plp(),
            OpGroup::ROL => self.rol(if instruction.operand.is_none() { None } else { Some(instruction.operand.unwrap().unwrap_right())}),
            OpGroup::ROR => self.ror(if instruction.operand.is_none() { None } else { Some(instruction.operand.unwrap().unwrap_right())}),
            OpGroup::RTI => self.rti(),
            OpGroup::RTS => self.rts(),
            OpGroup::SBC => self.sbc(instruction.operand.unwrap()),
            OpGroup::SEC => self.sec(),
            OpGroup::SED => self.sed(),
            OpGroup::SEI => self.sei(),
            OpGroup::STA => self.sta(instruction.operand.unwrap().unwrap_right()),
            OpGroup::STX => self.stx(instruction.operand.unwrap().unwrap_right()),
            OpGroup::STY => self.sty(instruction.operand.unwrap().unwrap_right()),
            OpGroup::TAX => self.tax(),
            OpGroup::TAY => self.tay(),
            OpGroup::TSX => self.tsx(),
            OpGroup::TXA => self.txa(),
            OpGroup::TXS => self.txs(),
            OpGroup::TYA => self.tya(),
            _ => panic!("Opcode {:?} is not implemented yet", instruction.opcode),
        }
    }

    fn fetch_value(&self, operand: Either<Value, Address>) -> Value {
        match operand {
            Left(val) => val,
            Right(addr) => self.bus.mem_read(addr)
        }
    }

    fn handle_comparision(&mut self, lhs: Value, rhs: Value) {
        let result = lhs.wrapping_sub(rhs);
        self.status.negative = (result >> 7.into()) == 1.into();
        self.status.zero = result == 0.into();
        self.status.carry = lhs >= rhs;
    }

    fn asl(&mut self, operand: Option<Address>) {
        let value = if let Some(addr) = operand {
            self.fetch_value(Right(addr))
        } else {
            self.register_a
        };

        let result = value << 1.into();
        match operand {
            Some(addr) => self.bus.mem_write(addr, result),
            None => self.register_a = result,
        }

        self.status.carry = value >> 7.into() == 1.into();
        self.handle_zero_and_negative_flags(result);
    }

    fn and(&mut self, operand: Either<Value, Address>) {
        let value = self.fetch_value(operand);
        self.register_a = self.register_a & value;

        self.handle_zero_and_negative_flags(self.register_a);
    }

    fn adc(&mut self, operand: Either<Value, Address>) {
        let value = self.fetch_value(operand);

        self.adc_value(value);
    }

    fn adc_value(&mut self, value: Value) {
        let (result, is_overflow) = self.register_a.overflowing_add(value);

        let carry = if self.status.carry {
            1
        } else {
            0
        };

        let (result, is_overflow2) = result.overflowing_add(carry.into());

        self.status.carry = is_overflow || is_overflow2;
        self.status.overflow = self.status.carry;
        self.handle_zero_and_negative_flags(result);

        self.status.overflow = self.register_a & NEGATIVE_MASK == value & NEGATIVE_MASK && result & NEGATIVE_MASK != value & NEGATIVE_MASK;
        self.register_a = result;
    }

    fn bcc(&mut self, operand: Address) {
        if !self.status.carry {
            self.program_counter = operand;
        } else {
            self.program_counter += 2.into();
        }
    }

    fn bcs(&mut self, operand: Address) {
        if self.status.carry {
            self.program_counter = operand;
        } else {
            self.program_counter += 2.into();
        }
    }

    fn beq(&mut self, operand: Address) {
        if self.status.zero {
            self.program_counter = operand.into();
        } else {
            self.program_counter += 2.into();
        }
    }

    fn bit(&mut self, operand: Address) {
        let memory = self.bus.mem_read(operand);
        let result = self.register_a & memory;

        self.status.zero = result == 0.into();
        self.status.negative = (memory >> 7.into()) != 0.into();
        self.status.overflow = memory & 0b0100_0000.into() != 0.into();
    }

    fn bmi(&mut self, operand: Address) {
        if self.status.negative {
            self.program_counter = operand;
        } else {
            self.program_counter += 2.into();
        }
    }

    fn bne(&mut self, operand: Address) {
        if !self.status.zero {
            self.program_counter = operand;
        } else {
            self.program_counter += 2.into();
        }
    }

    fn bpl(&mut self, operand: Address) {
        if !self.status.negative {
            self.program_counter = operand;
        } else {
            self.program_counter += 2.into();
        }
    }

    fn brk(&mut self) {
        let value: Value16 = self.program_counter.into();
        let (v1, v2) = value.split();

        self.push_stack(v2);
        self.push_stack(v1);
        self.push_stack(self.status.into());

        self.program_counter = self.bus.mem_read_addr(0xFFFE.into());
        self.status.break_cmd = true;
    }

    fn bvc(&mut self, operand: Address) {
        if !self.status.overflow {
            self.program_counter = operand;
        } else {
            self.program_counter += 2.into();
        }
    }

    fn bvs(&mut self, operand: Address) {
        if self.status.overflow {
            self.program_counter = operand;
        } else {
            self.program_counter += 2.into();
        }
    }

    fn clc(&mut self) {
        self.status.carry = false;
    }

    fn cld(&mut self) {
        self.status.decimal_mode = false;
    }

    fn cli(&mut self) {
        self.status.interrupt = false;
    }

    fn clv(&mut self) {
        self.status.overflow = false;
    }

    fn cmp(&mut self, operand: Either<Value, Address>) {
        let value = self.fetch_value(operand);
        self.handle_comparision(self.register_a, value);
    }

    fn cpx(&mut self, operand: Either<Value, Address>) {
        let value = self.fetch_value(operand);
        self.handle_comparision(self.register_x, value);
    }

    fn cpy(&mut self, operand: Either<Value, Address>) {
        let value = self.fetch_value(operand);
        self.handle_comparision(self.register_y, value);
    }

    fn dec(&mut self, operand: Address) {
        let value = self.bus.mem_read(operand);
        let (result, _) = value.overflowing_sub(1.into());

        self.bus.mem_write(operand, result);
        self.handle_zero_and_negative_flags(result);
    }

    fn dex(&mut self) {
        let (result, _) = self.register_x.overflowing_sub(1.into());

        self.register_x = result;
        self.handle_zero_and_negative_flags(result);
    }

    fn dey(&mut self) {
        let (result, _) = self.register_y.overflowing_sub(1.into());

        self.register_y = result;
        self.handle_zero_and_negative_flags(result);
    }

    fn eor(&mut self, operand: Either<Value, Address>) {
        let value = self.fetch_value(operand);
        self.register_a ^= value;

        self.handle_zero_and_negative_flags(self.register_a);
    }

    fn inc(&mut self, operand: Address) {
        let value = self.bus.mem_read(operand);
        let (result, _) = value.overflowing_add(1.into());

        self.bus.mem_write(operand, result);
        self.handle_zero_and_negative_flags(result);
    }

    fn inx(&mut self) {
        let (result, _) = self.register_x.overflowing_add(1.into());

        self.register_x = result;
        self.handle_zero_and_negative_flags(result);
    }

    fn iny(&mut self) {
        let (result, _) = self.register_y.overflowing_add(1.into());

        self.register_y = result;
        self.handle_zero_and_negative_flags(result);
    }

    fn jmp(&mut self, operand: Address) {
        self.program_counter = operand;
    }

    fn jsr(&mut self, operand: Address) {
        let val: Value16 = (self.program_counter + 2.into()).into();
        let (v1, v2) = val.split();

        self.push_stack(v2);
        self.push_stack(v1);
        self.program_counter = operand;
    }

    fn lda(&mut self, operand: Either<Value, Address>) {
        self.register_a = self.fetch_value(operand);
        self.handle_zero_and_negative_flags(self.register_a);
    }

    fn ldx(&mut self, operand: Either<Value, Address>) {
        self.register_x = self.fetch_value(operand);
        self.handle_zero_and_negative_flags(self.register_x);
    }

    fn ldy(&mut self, operand: Either<Value, Address>) {
        self.register_y = self.fetch_value(operand);
        self.handle_zero_and_negative_flags(self.register_y);
    }

    fn lsr(&mut self, operand: Option<Address>) {
        let value = if let Some(addr) = operand {
            self.fetch_value(Right(addr))
        } else {
            self.register_a
        };

        let result = value >> 1.into();
        match operand {
            Some(addr) => self.bus.mem_write(addr, result),
            None => self.register_a = result,
        }

        self.status.carry = value & 1.into() == 1.into();
        self.handle_zero_and_negative_flags(result);
    }

    fn nop(&mut self) {}

    fn ora(&mut self, operand: Either<Value, Address>) {
        let operand = self.fetch_value(operand);
        self.register_a |= operand;

        self.handle_zero_and_negative_flags(self.register_a);
    }

    fn pha(&mut self) {
        self.push_stack(self.register_a);
    }

    fn php(&mut self) {
        let mut status = self.status.clone();
        status.break_cmd = true;
        self.push_stack(status.into());
    }

    fn pla(&mut self) {
        self.register_a = self.pop_stack();
        self.handle_zero_and_negative_flags(self.register_a);
    }

    fn plp(&mut self) {
        self.status = self.pop_stack().into();
        self.status.break_cmd = false;
    }

    fn rol(&mut self, operand: Option<Address>) {
        let value = if let Some(addr) = operand {
            self.fetch_value(Right(addr))
        } else {
            self.register_a
        };

        let result = if self.status.carry {
            (value << 1.into()) + 1.into()
        } else {
            value << 1.into()
        };

        match operand {
            Some(addr) => self.bus.mem_write(addr, result),
            None => self.register_a = result,
        }

        self.status.carry = (value >> 7.into()) == 1.into();
        self.handle_zero_and_negative_flags(result);
    }

    fn ror(&mut self, operand: Option<Address>) {
        let value = if let Some(addr) = operand {
            self.fetch_value(Right(addr))
        } else {
            self.register_a
        };

        let result = if self.status.carry {
            (value >> 1.into()) + 0b1000_0000.into()
        } else {
            value >> 1.into()
        };

        self.status.carry = (value & 1.into()) == 1.into();
        self.handle_zero_and_negative_flags(result);

        match operand {
            Some(addr) => self.bus.mem_write(addr, result),
            None => self.register_a = result,
        }
    }

    fn rti(&mut self) {
        self.status = self.pop_stack().into();
        let high = self.pop_stack();
        let low = self.pop_stack();
        self.program_counter = Value16::from_values(high, low).into();
    }

    fn rts(&mut self) {
        let high = self.pop_stack();
        let low = self.pop_stack();
        self.program_counter = Value16::from_values(high, low).into();
        self.program_counter += 1.into();
    }

    fn sbc(&mut self, operand: Either<Value, Address>) {
        let value = self.fetch_value(operand);

        self.adc_value(!value);
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

    fn sta(&mut self, operand: Address) {
        self.bus.mem_write(operand, self.register_a);
    }

    fn stx(&mut self, operand: Address) {
        self.bus.mem_write(operand, self.register_x);
    }

    fn sty(&mut self, operand: Address) {
        self.bus.mem_write(operand, self.register_y);
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
        self.register_x = self.stack_pointer;
        self.handle_zero_and_negative_flags(self.register_x);
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.handle_zero_and_negative_flags(self.register_a);
    }

    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
    }

    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.handle_zero_and_negative_flags(self.register_a);
    }

    fn handle_zero_and_negative_flags(&mut self, value: Value) {
        self.status.zero = value == 0.into();
        self.status.negative = (value >> 7.into()) != 0.into();
    }

    fn get_operand_value(&self, opcode: &OpCode) -> Value {
        match opcode.mode {
            AddressingMode::Implicit => panic!(
                "Cannot get operand of opcode {:?} because it is implicit",
                opcode
            ),
            AddressingMode::Accumulator => self.register_a,
            _ => self
                .bus
                .mem_read(self.convert_to_memory_address(opcode.mode)),
        }
    }

    fn convert_to_memory_address(&self, mode: AddressingMode) -> Address {
        match mode {
            AddressingMode::Implicit => {
                panic!("Cannot convert implicit addressing mode to an address")
            }
            AddressingMode::Accumulator => {
                panic!("Cannot convert accumulator addressing mode to address")
            }
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.bus.mem_read(self.program_counter).into(),
            AddressingMode::ZeroPageX => {
                (self.bus.mem_read(self.program_counter) + self.register_x).into()
            }
            AddressingMode::ZeroPageY => {
                (self.bus.mem_read(self.program_counter) + self.register_y).into()
            }
            AddressingMode::Relative => {
                self.program_counter + self.bus.mem_read(self.program_counter).into()
            }
            AddressingMode::Absolute => self.bus.mem_read_addr(self.program_counter),
            AddressingMode::AbsoluteX => {
                self.bus.mem_read_addr(self.program_counter) + self.register_x.into()
            }
            AddressingMode::AbsoluteY => {
                self.bus.mem_read_addr(self.program_counter) + self.register_y.into()
            }
            AddressingMode::Indirect => self
                .bus
                .mem_read_addr(self.bus.mem_read_addr(self.program_counter).into()),
            AddressingMode::IndirectX => {
                let base_addr = self.bus.mem_read(self.program_counter);
                let table_addr = base_addr.wrapping_add(self.register_x).into();

                self.bus.mem_read_addr(table_addr)
            }
            AddressingMode::IndirectY => {
                self.bus.mem_read_addr(self.program_counter) + self.register_y.into()
            }
        }
    }
}

fn trace(cpu: &CPU) -> String {
    let instruction = cpu.read_instruction();
    let raw_instruction = format_raw_instruction(&instruction);
    let interpreted_instruction = format_interpreted_instruction(&instruction, &cpu);
    let registers = format_registers(&cpu);

    format!(
        "{:04X}  {:8}  {:30}  {}",
        cpu.program_counter, raw_instruction, interpreted_instruction, registers
    )
}

fn format_raw_instruction(instruction: &Instruction) -> String {
    let raw_operand_str = match instruction.raw_operand {
        Some(operand) => match operand {
            Left(op) => format!("{:02X}", op),
            Right(op) => {
                let (h, l) = op.split();
                format!("{:02X} {:02X}", h, l)
            }
        },
        None => "".to_string(),
    };

    format!("{:02X} {:5}", instruction.raw_opcode, raw_operand_str)
}

fn format_interpreted_instruction(instruction: &Instruction, cpu: &CPU) -> String {
    let operand = match instruction.operand {
        Some(op) => Some(op.either_into::<Address>()),
        None => None,
    };

    let formatted_addr_operand = match operand {
        Some(op) => format_address_operand(op, instruction, cpu),
        None => match instruction.opcode.mode {
            AddressingMode::Accumulator => "A".to_string(),
            _ => "".to_string()
        }
    };
     
    format!("{} {}", instruction.opcode.group, formatted_addr_operand)
}

fn format_address_operand(addr: Address, instruction: &Instruction, cpu: &CPU) -> String {
    match instruction.opcode.group {
        OpGroup::JMP => match instruction.opcode.mode {
           AddressingMode::Indirect =>
           {
               let raw_address: Address = instruction.raw_operand.unwrap().unwrap_right().into();
               format!("(${:04X}) = {:04X}", raw_address, addr)
           }
            _ => format!("${:04X}", addr)
        },
        OpGroup::JSR => format!("${:04X}", addr),
        _ => {
            let value = cpu.bus.mem_read(addr);
            match instruction.opcode.mode {
            AddressingMode::Absolute => format!("${:04X} = {:02X}", addr, value),
            AddressingMode::AbsoluteX => {
                let raw_addr: Address = instruction.raw_operand.unwrap().unwrap_right().into();
                let addr = instruction.operand.unwrap().unwrap_right();
                let value = cpu.bus.mem_read(addr);

                format!("${:04X},X @ {:04X} = {:02X}", raw_addr, addr, value)
            },
            AddressingMode::AbsoluteY => {
                let raw_addr: Address = instruction.raw_operand.unwrap().unwrap_right().into();
                let addr = instruction.operand.unwrap().unwrap_right();
                let value = cpu.bus.mem_read(addr);

                format!("${:04X},Y @ {:04X} = {:02X}", raw_addr, addr, value)
            },
            AddressingMode::Accumulator => "4".to_string(),
            AddressingMode::Immediate => format!("#${:02X}", addr),
            AddressingMode::Implicit => "".to_string(),
            AddressingMode::IndirectX => {
                let raw_op = instruction.raw_operand.unwrap().unwrap_left();
                let zero_page_addr = raw_op.wrapping_add(cpu.register_x);
                let addr = instruction.operand.unwrap().unwrap_right();
                format!("(${:02X},X) @ {:02X} = {:04X} = {:02X}", raw_op, zero_page_addr, addr, value)
            }
            AddressingMode::IndirectY => {
                let raw_op = instruction.raw_operand.unwrap().unwrap_left();
                let base_addr = cpu.bus.mem_read_addr_zero_page(raw_op);
                let addr = instruction.operand.unwrap().unwrap_right();

                format!("(${:02X}),Y = {:04X} @ {:04X} = {:02X}", raw_op, base_addr, addr, value)
            },
            AddressingMode::ZeroPage => format!(
                "${:02X} = {:02X}",
                addr,
                cpu.bus.mem_read(addr)
            ),
            AddressingMode::ZeroPageX => {
                let raw_operand = instruction.raw_operand.unwrap().unwrap_left();
                let operand = instruction.operand.unwrap().unwrap_right();
                let value = cpu.bus.mem_read(operand);
                

                format!("${:02X},X @ {:02X} = {:02X}", raw_operand, operand, value)
            },
            AddressingMode::ZeroPageY => {
                let raw_operand = instruction.raw_operand.unwrap().unwrap_left();
                let operand = instruction.operand.unwrap().unwrap_right();
                let value = cpu.bus.mem_read(operand);
                

                format!("${:02X},Y @ {:02X} = {:02X}", raw_operand, operand, value)
            },
            AddressingMode::Relative => format!("${:04X}", addr),
            AddressingMode::Indirect => "13".to_string(),
        } 
        }
    }
}

fn format_registers(cpu: &CPU) -> String {
    format!(
        "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
        cpu.register_a,
        cpu.register_x,
        cpu.register_y,
        u8::from(cpu.status),
        cpu.stack_pointer
    )
}

#[cfg(test)]
mod tests {
    use std::{
        fs::File,
        io::{self, BufRead},
        path::Path,
    };

    use super::*;

    #[test]
    fn execute_nestest_rom() {
        let rom =
            Rom::from_file("./test_roms/nestest/nestest.nes").expect("Failed to load rom for test");
        let log_lines =
            read_lines("./test_roms/nestest/nestest.log").expect("Failed to load log file");

        let mut cpu = CPU::from_rom(&rom);

        cpu.program_counter = 0x0c000.into();

        for (i, log) in log_lines.enumerate() {
            let log = log.expect("failed to read line");
            let (log, _) = log.split_at(73);

            println!("Attempting log line {}", i+1);

            cpu.execute_with_callback(move |cpu| {
                let trace = trace(&cpu);

                assert_eq!(trace, log, "Comparision failed at line {}", i + 1);
            });
        }
    }

    fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
    where
        P: AsRef<Path>,
    {
        let file = File::open(filename)?;
        Ok(io::BufReader::new(file).lines())
    }
}
