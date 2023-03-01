use std::ops::{Add, Div, Mul, Not, Rem, Sub};

#[derive(Debug, Clone)]
pub enum Type {
    Float,
    Integer,
    IntPointer,
    Str,
    StrPointer,
    Bool,
    Array,
    TypeError(String),
}

impl Add for Type {
    type Output = Type;

    fn add(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Type::Float, Type::Float)
            | (Type::Integer, Type::Float)
            | (Type::Float, Type::Integer) => Type::Float,

            (Type::Integer, Type::Integer) => Type::Integer,
            (Type::Bool, Type::Bool) => Type::Bool,
            (Type::Str, Type::Str) => Type::Str,

            (Type::Array, _) | (_, Type::Array) => Type::Array,

            _ => Type::TypeError(format!("Couldn't add {:?} and {:?}", self, rhs)),
        }
    }
}

impl Sub for Type {
    type Output = Type;

    fn sub(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Type::Float, Type::Float)
            | (Type::Integer, Type::Float)
            | (Type::Float, Type::Integer) => Type::Float,

            (Type::Integer, Type::Integer) => Type::Integer,

            _ => Type::TypeError(format!("Couldn't substract {:?} to {:?}", rhs, self)),
        }
    }
}

impl Mul for Type {
    type Output = Type;

    fn mul(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Type::Float, Type::Float)
            | (Type::Integer, Type::Float)
            | (Type::Float, Type::Integer) => Type::Float,

            (Type::Integer, Type::Integer) => Type::Integer,
            (Type::Bool, Type::Bool) => Type::Bool,

            _ => Type::TypeError(format!("Couldn't multiply {:?} by {:?}", self, rhs)),
        }
    }
}

impl Div for Type {
    type Output = Type;

    fn div(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Type::Float, Type::Float)
            | (Type::Integer, Type::Float)
            | (Type::Float, Type::Integer) => Type::Float,

            (Type::Integer, Type::Integer) => Type::Integer,

            _ => Type::TypeError(format!("Couldn't divide {:?} by {:?}", self, rhs)),
        }
    }
}

impl Rem for Type {
    type Output = Type;

    fn rem(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Type::Float, Type::Float)
            | (Type::Integer, Type::Float)
            | (Type::Float, Type::Integer) => Type::Float,

            (Type::Integer, Type::Integer) => Type::Integer,

            _ => Type::TypeError(format!("Couldn't perform {:?} mod {:?}", self, rhs)),
        }
    }
}

impl Not for Type {
    type Output = Type;

    fn not(self) -> Self::Output {
        match self {
            Type::Float => Type::Float,
            Type::Integer => Type::Integer,
            Type::Bool => Type::Bool,

            _ => Type::TypeError(format!("Couldn't perform not({:?})", self)),
        }
    }
}
