use std::fmt::{Debug, Display};

#[derive(Debug)]
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

impl Debug for VMType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(x) => write!(f, "Float<{x}>"),
            Self::Integer(n) => write!(f, "Int<{n}>"),
            Self::String(c) => write!(f, "Str<{c}>"),
            Self::Array(array) => {
                {
                    write!(f, "Array<").unwrap();
                    array.iter().for_each(|element| {
                        write!(f, "{element}, ").unwrap();
                    });
                    write!(f, ">").unwrap();
                };
                Ok(())
            }
        }
        .unwrap();

        Ok(())
    }
}

impl Display for VMType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(x) => write!(f, "Float<{x}>"),
            Self::Integer(n) => write!(f, "Int<{n}>"),
            Self::String(c) => write!(f, "Str<{c}>"),
            Self::Array(array) => {
                {
                    write!(f, "Array<").unwrap();
                    array.iter().for_each(|element| {
                        write!(f, "{element}, ").unwrap();
                    });
                    write!(f, ">").unwrap();
                };
                Ok(())
            }
        }
        .unwrap();

        Ok(())
    }
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
