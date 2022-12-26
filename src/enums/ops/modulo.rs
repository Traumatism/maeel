use crate::enums::vmtype::VMType;
use std::ops::Rem;

/// Modulo operation trait implementation for all types
impl Rem for VMType {
    type Output = VMType;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m % n),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x % y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 % x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x % n as f64),
            (a, b) => panic!("can't modulo {a:?} and {b:?}"),
        }
    }
}
