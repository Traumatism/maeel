use std::fmt::{Debug, Display};

pub enum Token {
    Separator,
    Str(String, u16),        // "..."
    Integer(i64, u16),       // 123, -123
    Float(f64, u16),         // 123.4, -123.45
    Identifier(String, u16), // abc
}

/// Data types/structures parser can work with
pub enum VMType {
    Float(f64),
    Integer(i64),
    Str(String),
    Array(Vec<VMType>),
}

impl Debug for VMType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(x) => write!(f, "{x}"),
            Self::Integer(n) => write!(f, "{n}"),
            Self::Str(c) => write!(f, "'{c}'"),
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
            Self::Float(x) => write!(f, "{x}"),
            Self::Integer(n) => write!(f, "{n}"),
            Self::Str(c) => write!(f, "'{c}'"),
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
            Self::Str(c) => Self::Str(c.clone()),
            Self::Array(array) => Self::Array(array.clone()),
        }
    }
}
