use crate::enums::vmtype::VMType;
use std::ops::Not;

/// Not operation trait implementation for all types
impl Not for VMType {
    type Output = VMType;

    fn not(self) -> Self::Output {
        match self {
            VMType::Float(x) => VMType::Float(x * -1.),
            VMType::Integer(n) => VMType::Integer(-n),
            VMType::Bool(p) => VMType::Bool(!p),
            a => panic!("can't invert {a:?}"),
        }
    }
}
