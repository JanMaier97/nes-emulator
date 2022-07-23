use core::fmt;
use std::ops;

use super::{values::{Value, Value16}, split_high_low, combine_high_low};


#[derive(Debug, Clone, Copy)]
pub struct Address(u16);


impl From<Address> for u16 {
    fn from(val: Address) -> Self {
        val.0
    }
}

impl From<Value> for Address {
    fn from(value: Value) -> Self {
        Address(u8::from(value) as u16)
    }
}

impl From<u16> for Address {
    fn from(addr: u16) -> Self {
        Address(addr)
    }
}

impl From<Value16> for Address {
    fn from(val: Value16) -> Self {
        let (high, low) = val.split();
        Address(combine_high_low(low.into(), high.into()))
    }
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl fmt::UpperHex for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = self.0;
        fmt::UpperHex::fmt(&val, f)
    }
}

impl ops::AddAssign for Address {
    fn add_assign(&mut self, rhs: Self) {
        *self = Address(self.0 + rhs.0)
    }
}

impl ops::Add for Address {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Address(self.0 + rhs.0)
    }
}

impl Address {
    pub const fn from_const(value: u16) -> Address {
        Address(value)
    }

    pub fn wrapping_add(self, value: Address) -> Address {
        self.0.wrapping_add(value.0).into()
    }

    pub fn from_values(high: Value, low: Value) -> Address {
        Address(combine_high_low(high.into(), low.into()))
    }

    pub fn split(self) -> (Value, Value) {
        let (h, l) = split_high_low(self.0);
        (Value::from(h), Value::from(l))
    }
}
