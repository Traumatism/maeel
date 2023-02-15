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
            (VMType::Integer(a), VMType::Integer(b)) => Some(a.cmp(b)),
            (VMType::Float(a), VMType::Float(b)) => Some(a.total_cmp(b)),
            (VMType::Integer(a), VMType::Float(b)) | (VMType::Float(b), VMType::Integer(a)) => {
                Some(b.total_cmp(&(*a as f64)))
            }

            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

impl PartialEq for VMType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VMType::Str(a), VMType::Str(b)) => a == b,
            (VMType::Array(a), VMType::Array(b)) => a == b,
            (VMType::Float(a), VMType::Integer(b)) => *a == (*b as f64),
            (VMType::Integer(a), VMType::Float(b)) => (*a as f64) == *b,
            (VMType::Integer(a), VMType::Integer(b)) => a == b,
            (VMType::Float(a), VMType::Float(b)) => a == b,
            (VMType::Bool(a), VMType::Bool(b)) => a == b,
            _ => false,
        }
    }
}

impl ToString for VMType {
    fn to_string(&self) -> String {
        match self {
            VMType::Float(a) => a.to_string(),
            VMType::Integer(a) => a.to_string(),
            VMType::Str(a) => a.to_string(),
            VMType::Bool(a) => a.to_string(),
            VMType::Array(a) => format!("{a:?}"),
        }
    }
}

impl BitXor for VMType {
    type Output = VMType;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(a), VMType::Bool(b)) => VMType::Bool(a ^ b),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

impl Sub for VMType {
    type Output = VMType;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Integer(a - b),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a - b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 - b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a - b as f64),
            (a, b) => panic!("can't sub {a:?} and {b:?}"),
        }
    }
}

impl BitOr for VMType {
    type Output = VMType;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(a), VMType::Bool(b)) => VMType::Bool(a | b),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

impl Not for VMType {
    type Output = VMType;

    fn not(self) -> Self::Output {
        match self {
            VMType::Float(a) => VMType::Float(a * -1.),
            VMType::Integer(a) => VMType::Integer(-a),
            VMType::Bool(a) => VMType::Bool(!a),
            a => panic!("can't invert {a:?}"),
        }
    }
}

impl Mul for VMType {
    type Output = VMType;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Integer(a * b),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a * b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 * b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a * b as f64),
            (VMType::Bool(a), VMType::Bool(b)) => VMType::Bool(a & b),
            (a, b) => panic!("can't mul {a:?} and {b:?}"),
        }
    }
}

impl Rem for VMType {
    type Output = VMType;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Integer(a % b),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a % b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 % b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a % b as f64),
            (a, b) => panic!("can't modulo {a:?} and {b:?}"),
        }
    }
}

impl Div for VMType {
    type Output = VMType;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Float(a as f64 / b as f64),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a / b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 / b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a / b as f64),
            (a, b) => panic!("can't divide {a:?} and {b:?}"),
        }
    }
}

impl BitAnd for VMType {
    type Output = VMType;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Bool(a), VMType::Bool(b)) => VMType::Bool(a & b),
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

impl Add for VMType {
    type Output = VMType;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Str(a), VMType::Str(b)) => VMType::Str(a + &b),
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Integer(a + b),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a + b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 + b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a + b as f64),
            (VMType::Bool(a), VMType::Bool(b)) => VMType::Bool(a | b),

            (other, VMType::Array(mut array)) | (VMType::Array(mut array), other) => {
                array.push(other);
                VMType::Array(array)
            }

            (a, b) => panic!("can't add {a:?} and {b:?}"),
        }
    }
}
