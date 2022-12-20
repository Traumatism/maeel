pub enum Token {
    Separator,
    String(String, u16),
    Integer(i64, u16),
    Float(f64, u16),
    Identifier(String, u16),
}

pub enum VMType {
    Float(f64),
    Integer(i64),
    String(String),
    Array(Vec<VMType>),
}

impl Clone for VMType {
    fn clone(&self) -> Self {
        match self {
            Self::Float(x) => Self::Float(*x),
            Self::Integer(n) => Self::Integer(*n),
            Self::String(c) => Self::String(c.clone()),
            Self::Array(array) => Self::Array(array.clone()),
        }
    }
}
