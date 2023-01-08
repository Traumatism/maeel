use crate::enums::vmtype::VMType;
use std::ops::Len;

/// Len operation trait implementation for all types
impl Len for VMType {
    fn len(&self) -> usize {
        match self {
            VMType::Integer(n) => n.to_string().len(),
            VMType::Float(x) => x.to_string().len(),
            VMType::String(s) => s.len(),
            VMType::Boolean(b) => b.to_string().len(),
            VMType::List(l) => l.len(),
            VMType::Tuple(t) => t.len(),
            VMType::Dictionary(d) => d.len(),
            VMType::Function(f) => f.len(),
            VMType::None => 0,
        }
    }
}
