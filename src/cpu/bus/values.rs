use core::fmt;
use std::ops;

use super::{address::Address, split_high_low};

// Raw value read from memory
#[derive(Debug, Eq, Hash, PartialOrd, Clone, Copy)]
pub struct Value(u8);

pub struct SignedValue(i8);

#[derive(Debug, Clone, Copy)]
pub struct Value16(u16);

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl ops::Not for Value {
    type Output = Self;

    fn not(self) -> Self::Output {
        Value(!self.0)
    }
}

impl ops::BitOrAssign for Value {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl ops::BitXorAssign for Value {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0;
    }
}

impl ops::BitXor for Value {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Value(self.0 ^ rhs.0)
    }
}

impl ops::Sub for Value {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Value(self.0 + rhs.0)
    }
}

impl ops::Add for Value {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Value(self.0 + rhs.0)
    }
}

impl ops::BitAnd for Value {
    type Output = Self;

    // rhs is the "right-hand side" of the expression `a & b`
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl ops::Shr for Value {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        Self(self.0 >> rhs.0)
    }
}

impl ops::Shl for Value {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        Self(self.0 << rhs.0)
    }
}

impl ops::AddAssign for Value {
    fn add_assign(&mut self, rhs: Self) {
        *self = Value(self.0 + rhs.0)
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

impl From<Value16> for u16 {
    fn from(val: Value16) -> Self {
        val.0
    }
}

impl From<u8> for Value {
    fn from(val: u8) -> Self {
        Value(val)
    }
}

impl From<Value> for u8 {
    fn from(val: Value) -> Self {
        val.0
    }
}

impl From<Address> for Value16 {
    fn from(addr: Address) -> Self {
        let (high, low) = addr.split();
        Value16::from_values(low.into(), high.into())
    }
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
