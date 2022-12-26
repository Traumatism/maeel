use crate::enums::vmtype::VMType;
use std::ops::BitOr;

/// Bitwise or operation trait implementation for all types
impl BitOr for VMType {
    type Output = VMType;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p | q),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}
