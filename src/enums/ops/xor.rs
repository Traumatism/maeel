use crate::enums::vmtype::VMType;
use std::ops::BitXor;

/// Bitwise xor operation trait implementation for all types
impl BitXor for VMType {
    type Output = VMType;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p ^ q),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}
