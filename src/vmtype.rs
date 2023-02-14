use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Sub};

#[derive(Debug, Clone)]
pub enum VMType {
    Float(f64),
    Integer(i64),
    Str(String),
    Bool(bool),
    Array(Vec<VMType>),
}

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

impl BitXor for VMType {
    type Output = VMType;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p ^ q),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

impl Sub for VMType {
    type Output = VMType;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m - n),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x - y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 - x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x - n as f64),
            (a, b) => panic!("can't sub {a:?} and {b:?}"),
        }
    }
}

impl BitOr for VMType {
    type Output = VMType;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p | q),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

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

impl Mul for VMType {
    type Output = VMType;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m * n),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x * y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 * x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x * n as f64),
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p & q),
            (a, b) => panic!("can't mul {a:?} and {b:?}"),
        }
    }
}

impl Rem for VMType {
    type Output = VMType;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m % n),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x % y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 % x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x % n as f64),
            (a, b) => panic!("can't modulo {a:?} and {b:?}"),
        }
    }
}

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

impl BitAnd for VMType {
    type Output = VMType;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p & q),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

impl Add for VMType {
    type Output = VMType;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Str(s), VMType::Str(t)) => VMType::Str(s + &t),
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m + n),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x + y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 + x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x + n as f64),
            (VMType::Bool(p), VMType::Bool(q)) => VMType::Bool(p | q),
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
