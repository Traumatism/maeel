use std::{
    fmt::Display,
    ops::{
        Add,
        Div,
        Mul,
        Not,
        Rem,
        Sub,
    },
};

use crate::tokens::Token;

#[derive(Debug, Clone)]

pub enum VMType
{
    Float(f64),
    Integer(i64),
    Str(String),
    Bool(bool),
    Array(Vec<VMType>),
    Procedure(Vec<Token>),
    None,
}

impl Display for VMType
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>)
        -> std::fmt::Result
    {
        match self {
            VMType::Procedure(tokens) => write!(f, "{:?}", tokens),
            VMType::None => write!(f, "None"),
            VMType::Float(x) => write!(f, "{}", x),
            VMType::Integer(x) => write!(f, "{}", x),
            VMType::Str(x) => write!(f, "{}", x),
            VMType::Bool(x) => write!(f, "{}", x),
            VMType::Array(xs) => {
                write!(f, "{{")?;

                for (i, x) in xs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }

                    write!(f, "{}", x)?;
                }

                write!(f, "}}")
            }
        }
    }
}

impl PartialOrd for VMType
{
    fn partial_cmp(&self, other: &Self)
        -> Option<std::cmp::Ordering>
    {
        match (self, other) {
            (VMType::Integer(a), VMType::Integer(b)) => {
                Some(a.cmp(b))
            }
            (VMType::Float(a), VMType::Float(b)) => {
                Some(a.total_cmp(b))
            }
            (VMType::Integer(a), VMType::Float(b))
            | (VMType::Float(b), VMType::Integer(a)) => {
                Some(b.total_cmp(&(*a as f64)))
            }
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

impl PartialEq for VMType
{
    fn eq(&self, other: &Self) -> bool
    {
        match (self, other) {
            (VMType::Str(a), VMType::Str(b)) => a == b,
            (VMType::Array(a), VMType::Array(b)) => a == b,
            (VMType::Float(a), VMType::Integer(b)) => {
                *a == (*b as f64)
            }
            (VMType::Integer(a), VMType::Float(b)) => {
                (*a as f64) == *b
            }
            (VMType::Integer(a), VMType::Integer(b)) => a == b,
            (VMType::Float(a), VMType::Float(b)) => a == b,
            (VMType::Bool(a), VMType::Bool(b)) => a == b,
            _ => false,
        }
    }
}

impl Sub for VMType
{
    type Output = VMType;

    fn sub(self, rhs: Self) -> Self::Output
    {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => {
                VMType::Integer(a - b)
            }
            (VMType::Float(a), VMType::Float(b)) => {
                VMType::Float(a - b)
            }
            (VMType::Integer(a), VMType::Float(b)) => {
                VMType::Float(a as f64 - b)
            }
            (VMType::Float(a), VMType::Integer(b)) => {
                VMType::Float(a - b as f64)
            }
            (a, b) => panic!("can't sub {a:?} and {b:?}"),
        }
    }
}

impl Not for VMType
{
    type Output = VMType;

    fn not(self) -> Self::Output
    {
        match self {
            VMType::Float(a) => VMType::Float(a * -1.),
            VMType::Integer(a) => VMType::Integer(-a),
            VMType::Bool(a) => VMType::Bool(a.not()),
            a => panic!("can't invert {a:?}"),
        }
    }
}

impl Mul for VMType
{
    type Output = VMType;

    fn mul(self, rhs: Self) -> Self::Output
    {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => {
                VMType::Integer(a * b)
            }
            (VMType::Float(a), VMType::Float(b)) => {
                VMType::Float(a * b)
            }
            (VMType::Integer(a), VMType::Float(b)) => {
                VMType::Float(a as f64 * b)
            }
            (VMType::Float(a), VMType::Integer(b)) => {
                VMType::Float(a * b as f64)
            }
            (VMType::Bool(false), VMType::Bool(_)) => {
                VMType::Bool(false)
            }
            (VMType::Bool(_), VMType::Bool(false)) => {
                VMType::Bool(false)
            }
            (VMType::Bool(true), VMType::Bool(true)) => {
                VMType::Bool(true)
            }
            (VMType::Array(a), VMType::Array(b)) => {
                let mut new_array = Vec::default();

                for ea in &a {
                    for eb in &b {
                        new_array.push(VMType::Array(Vec::from([
                            ea.clone(),
                            eb.clone(),
                        ])))
                    }
                }

                VMType::Array(new_array)
            }
            (a, b) => panic!("can't mul {a:?} and {b:?}"),
        }
    }
}

impl Add for VMType
{
    type Output = VMType;

    fn add(self, rhs: Self) -> Self::Output
    {
        match (self, rhs) {
            (VMType::Str(a), VMType::Str(b)) => VMType::Str(a + &b),
            (VMType::Integer(a), VMType::Integer(b)) => {
                VMType::Integer(a + b)
            }
            (VMType::Float(a), VMType::Float(b)) => {
                VMType::Float(a + b)
            }
            (VMType::Integer(a), VMType::Float(b)) => {
                VMType::Float(a as f64 + b)
            }
            (VMType::Float(a), VMType::Integer(b)) => {
                VMType::Float(a + b as f64)
            }
            (VMType::Bool(true), VMType::Bool(_)) => {
                VMType::Bool(true)
            }
            (VMType::Bool(_), VMType::Bool(true)) => {
                VMType::Bool(true)
            }
            (VMType::Bool(false), VMType::Bool(false)) => {
                VMType::Bool(false)
            }
            (VMType::Array(a), VMType::Array(b)) => {
                let mut new = Vec::default();

                for ea in a {
                    new.push(ea)
                }

                for eb in b {
                    new.push(eb)
                }

                VMType::Array(new)
            }

            (other, VMType::Array(mut array))
            | (VMType::Array(mut array), other) => {
                array.push(other);

                VMType::Array(array)
            }

            (a, b) => panic!("can't add {a:?} and {b:?}"),
        }
    }
}

impl Rem for VMType
{
    type Output = VMType;

    fn rem(self, rhs: Self) -> Self::Output
    {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => {
                VMType::Integer(a % b)
            }
            (VMType::Float(a), VMType::Float(b)) => {
                VMType::Float(a % b)
            }
            (VMType::Integer(a), VMType::Float(b)) => {
                VMType::Float(a as f64 % b)
            }
            (VMType::Float(a), VMType::Integer(b)) => {
                VMType::Float(a % b as f64)
            }
            (a, b) => panic!("can't modulo {a:?} and {b:?}"),
        }
    }
}

impl Div for VMType
{
    type Output = VMType;

    fn div(self, rhs: Self) -> Self::Output
    {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => {
                VMType::Float(a as f64 / b as f64)
            }
            (VMType::Float(a), VMType::Float(b)) => {
                VMType::Float(a / b)
            }
            (VMType::Integer(a), VMType::Float(b)) => {
                VMType::Float(a as f64 / b)
            }
            (VMType::Float(a), VMType::Integer(b)) => {
                VMType::Float(a / b as f64)
            }
            (a, b) => panic!("can't divide {a:?} and {b:?}"),
        }
    }
}
