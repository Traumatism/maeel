use crate::enums::vmtype::VMType;
use std::ops::BitAnd;

/// Bitwise and operation trait implementation for all types
impl BitAnd for VMType {
    type Output = VMType;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p & q),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}
