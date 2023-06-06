use crate::lexer::*;

#[derive(Clone)]
pub enum MaeelType {
    Float(f32),
    Integer(i64),
    String(String),
    Array(Vec<Self>),
    Function(Vec<Token>),
}

impl std::fmt::Display for MaeelType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Write a string to stdout (impossible to implement in maeel, by design)
            MaeelType::String(x) => write!(f, "{}", x),

            // Write an anonymous function to stdout (impossible to implement in maeel)
            MaeelType::Function(tokens) => write!(f, "{:?}", tokens),

            // Write a float to stdout (float2str[maeel] is in dev)
            MaeelType::Float(x) => write!(f, "{}", x),

            // Write an integer to stdout (int2str[maeel] is in dev)
            MaeelType::Integer(x) => write!(f, "{}", x),

            // Write an array to stdout
            MaeelType::Array(xs) => {
                write!(f, "{{")?;

                xs.iter().enumerate().for_each(|(i, x)| {
                    if i > 0 {
                        write!(f, " ").unwrap();
                    }

                    write!(f, "{}", &x).unwrap();
                });

                write!(f, "}}")
            }
        }
    }
}

impl PartialOrd for MaeelType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (MaeelType::Integer(a), MaeelType::Integer(b)) => Some(a.cmp(b)),
            (MaeelType::Float(a), MaeelType::Float(b)) => Some(a.total_cmp(b)),
            (MaeelType::Integer(a), MaeelType::Float(b))
            | (MaeelType::Float(b), MaeelType::Integer(a)) => Some(b.total_cmp(&(*a as f32))),

            (a, b) => panic!("Cannot compare {a} and {b}"),
        }
    }
}

impl PartialEq for MaeelType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // string == string
            (MaeelType::String(a), MaeelType::String(b)) => a == b,

            // array == array
            (MaeelType::Array(a), MaeelType::Array(b)) => a == b,

            // float == int or int == float
            (MaeelType::Integer(a), MaeelType::Float(b))
            | (MaeelType::Float(b), MaeelType::Integer(a)) => (*a as f32) == *b,

            // int == int
            (MaeelType::Integer(a), MaeelType::Integer(b)) => a == b,

            // float == float
            (MaeelType::Float(a), MaeelType::Float(b)) => a == b,

            _ => false,
        }
    }
}

impl std::ops::Sub for MaeelType {
    type Output = MaeelType;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // int - int
            (MaeelType::Integer(m), MaeelType::Integer(n)) => MaeelType::Integer(m - n),

            // float - float
            (MaeelType::Float(x), MaeelType::Float(y)) => MaeelType::Float(x - y),

            // int - float
            (MaeelType::Float(x), MaeelType::Integer(m))
            | (MaeelType::Integer(m), MaeelType::Float(x)) => MaeelType::Float(m as f32 - x),

            (a, b) => panic!("Cannot substract {a} and {b}"),
        }
    }
}

impl std::ops::Mul for MaeelType {
    type Output = MaeelType;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // int * int
            (MaeelType::Integer(m), MaeelType::Integer(n)) => MaeelType::Integer(m * n),

            // float * float
            (MaeelType::Float(x), MaeelType::Float(y)) => MaeelType::Float(x * y),

            // float * float
            (MaeelType::Float(x), MaeelType::Integer(m))
            | (MaeelType::Integer(m), MaeelType::Float(x)) => MaeelType::Float(x * m as f32),

            // string * float
            (MaeelType::Integer(m), MaeelType::String(s))
            | (MaeelType::String(s), MaeelType::Integer(m)) => {
                MaeelType::String(s.repeat(m as usize))
            }

            (a, b) => panic!("Cannot multiply {a} and {b}"),
        }
    }
}

impl std::ops::Add for MaeelType {
    type Output = MaeelType;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // str + str
            (MaeelType::String(a), MaeelType::String(b)) => MaeelType::String(a + &b),

            // int + int
            (MaeelType::Integer(m), MaeelType::Integer(n)) => MaeelType::Integer(m + n),

            // float + float
            (MaeelType::Float(x), MaeelType::Float(y)) => MaeelType::Float(x + y),

            // int + float
            (MaeelType::Integer(m), MaeelType::Float(x))
            | (MaeelType::Float(x), MaeelType::Integer(m)) => MaeelType::Float(m as f32 + x),

            // array + e and e + array
            (other, MaeelType::Array(mut xs)) | (MaeelType::Array(mut xs), other) => {
                xs.push(other);
                MaeelType::Array(xs)
            }

            (a, b) => panic!("Cannot add {a} and {b}"),
        }
    }
}

impl std::ops::Rem for MaeelType {
    type Output = MaeelType;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // int % int
            (MaeelType::Integer(m), MaeelType::Integer(n)) => MaeelType::Integer(m % n),

            // float % float
            (MaeelType::Float(x), MaeelType::Float(y)) => MaeelType::Float(x % y),

            // int % float
            (MaeelType::Integer(m), MaeelType::Float(x)) => MaeelType::Float(m as f32 % x),

            // float % int
            (MaeelType::Float(x), MaeelType::Integer(m)) => MaeelType::Float(x % m as f32),

            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}

impl std::ops::Div for MaeelType {
    type Output = MaeelType;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // int / int
            (MaeelType::Integer(m), MaeelType::Integer(n)) => MaeelType::Float(m as f32 / n as f32),

            // float / float
            (MaeelType::Float(x), MaeelType::Float(y)) => MaeelType::Float(x / y),

            // int / float
            (MaeelType::Integer(m), MaeelType::Float(x)) => MaeelType::Float(m as f32 / x),

            // float / int
            (MaeelType::Float(x), MaeelType::Integer(m)) => MaeelType::Float(x / m as f32),

            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}
