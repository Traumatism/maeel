use crate::enums::vmtype::VMType;
use std::ops::Div;

/// Div operation trait implementation for all types
impl Div for VMType {
    type Output = VMType;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Float(m as f64 / n as f64),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x / y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 / x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x / n as f64),
            (a, b) => panic!("can't divide {a:?} and {b:?}"),
        }
    }
}
