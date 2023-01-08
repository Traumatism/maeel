use crate::enums::vmtype::VMType;

/// Len operation trait implementation for all types
impl VMType {
    pub fn len(&self) -> i64 {
        (match self {
            VMType::Integer(n) => n.to_string().len(),
            VMType::Float(x) => x.to_string().len(),
            VMType::Str(s) => s.len(),
            VMType::Bool(b) => b.to_string().len(),
            VMType::Array(l) => l.len(),
        }) as i64
    }
}
