use crate::enums::vmtype::VMType;
use std::ops::Add;

/// Add operation trait implementation for all types
impl Add for VMType {
    type Output = VMType;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Str(s), VMType::Str(t)) => VMType::Str(s + &t),
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m + n),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x + y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 + x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x + n as f64),
            (other, VMType::Array(mut array)) => {
                array.push(other);
                VMType::Array(array)
            }
            (VMType::Array(mut array), other) => {
                array.push(other);
                VMType::Array(array)
            }
            (a, b) => panic!("can't add {a:?} and {b:?}"),
        }
    }
}
