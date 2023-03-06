use std::ops::{Add, Div, Mul, Not, Rem, Sub};

/// The `VMType` enum stores all data types that the
/// interpreter can work with
///
/// The `Clone` and `Debug` traits are implemented, which allows
/// the enum members to be easily copied, cloned, and printed.
#[derive(Debug, Clone)]
pub enum VMType {
    Float(f64),
    Integer(i64),
    IntPointer(Box<i64>),
    Str(String),
    StrPointer(String),
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
            _ => panic!(),
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

impl Not for VMType {
    type Output = VMType;

    fn not(self) -> Self::Output {
        match self {
            VMType::Float(a) => VMType::Float(a * -1.),
            VMType::Integer(a) => VMType::Integer(-a),
            VMType::Bool(a) => VMType::Bool(a.not()),
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

            (VMType::Bool(false), VMType::Bool(_)) => VMType::Bool(false),
            (VMType::Bool(_), VMType::Bool(false)) => VMType::Bool(false),
            (VMType::Bool(true), VMType::Bool(true)) => VMType::Bool(true),

            (a, b) => panic!("can't mul {a:?} and {b:?}"),
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

            (VMType::Bool(true), VMType::Bool(_)) => VMType::Bool(true),
            (VMType::Bool(_), VMType::Bool(true)) => VMType::Bool(true),
            (VMType::Bool(false), VMType::Bool(false)) => VMType::Bool(false),

            (other, VMType::Array(mut array)) | (VMType::Array(mut array), other) => {
                array.push(other);
                VMType::Array(array)
            }

            (a, b) => panic!("can't add {a:?} and {b:?}"),
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
