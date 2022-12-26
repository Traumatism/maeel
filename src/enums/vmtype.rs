/// Every single type VM can work with
#[derive(Debug, Clone)]
pub enum VMType {
    Float(f64),
    Integer(i64),
    Str(String),
    Bool(bool),
    Array(Vec<VMType>),
}

/// PartialOrd trait implementation for all types (perform comparaisons)
impl PartialOrd for VMType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (VMType::Integer(m), VMType::Integer(n)) => Some(m.cmp(n)),
            (VMType::Float(x), VMType::Float(y)) => Some(x.total_cmp(y)),
            (VMType::Float(x), VMType::Integer(n)) => Some(x.total_cmp(&(*n as f64))),
            (VMType::Integer(n), VMType::Float(x)) => Some(x.total_cmp(&(*n as f64))),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

/// PartialEq trait implementation for all types
impl PartialEq for VMType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VMType::Str(a), VMType::Str(b)) => a == b,
            (VMType::Array(a), VMType::Array(b)) => a == b,
            (VMType::Float(x), VMType::Integer(n)) => *x == (*n as f64),
            (VMType::Integer(n), VMType::Float(x)) => (*n as f64) == *x,
            (VMType::Integer(m), VMType::Integer(n)) => m == n,
            (VMType::Float(x), VMType::Float(y)) => x == y,
            (VMType::Bool(p), VMType::Bool(q)) => p == q,
            _ => false,
        }
    }
}

/// ToString trait implementation for all types (convert types => string)
impl ToString for VMType {
    fn to_string(&self) -> String {
        match self {
            VMType::Float(x) => x.to_string(),
            VMType::Integer(n) => n.to_string(),
            VMType::Str(c) => c.to_string(),
            VMType::Bool(p) => p.to_string(),
            VMType::Array(array) => format!("{array:?}"),
        }
    }
}
