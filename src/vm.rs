use super::lexer::*;

/// Values that can be processed by the virtual machine.
#[derive(Clone)]
pub enum VMType {
    Float(f64),
    Integer(i64),
    String(String),
    Array(Vec<VMType>),
    Function(Vec<Token>),
}

impl std::fmt::Display for VMType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        /*
        TODO: implement a function to check the type of the top element
        of the array. Should make function overloading (like print) easier.
        So their could be a print function for integer, a print function for strings
        etc.
        */

        match self {
            // Write a string to stdout (impossible to implement in maeel, by design)
            VMType::String(x) => write!(f, "{}", x),

            // Write an anonymous function to stdout (impossible to implement in maeel)
            VMType::Function(tokens) => write!(f, "{:?}", tokens),

            // Write a float to stdout (float2str[maeel] is in dev)
            VMType::Float(x) => write!(f, "{}", x),

            // Write an integer to stdout (int2str[maeel] is in dev)
            VMType::Integer(x) => write!(f, "{}", x),

            // Write an array to stdout
            VMType::Array(xs) => {
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

impl PartialOrd for VMType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (VMType::Integer(a), VMType::Integer(b)) => Some(a.cmp(b)),
            (VMType::Float(a), VMType::Float(b)) => Some(a.total_cmp(b)),
            (VMType::Integer(a), VMType::Float(b)) | (VMType::Float(b), VMType::Integer(a)) => {
                Some(b.total_cmp(&(*a as f64)))
            }

            (a, b) => panic!("Cannot compare {a} and {b}"),
        }
    }
}

impl PartialEq for VMType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VMType::String(a), VMType::String(b)) => a == b,
            (VMType::Array(a), VMType::Array(b)) => a == b,
            (VMType::Float(a), VMType::Integer(b)) => *a == (*b as f64),
            (VMType::Integer(a), VMType::Float(b)) => (*a as f64) == *b,
            (VMType::Integer(a), VMType::Integer(b)) => a == b,
            (VMType::Float(a), VMType::Float(b)) => a == b,

            _ => false,
        }
    }
}

impl std::ops::Sub for VMType {
    type Output = VMType;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // int - int
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m - n),

            // float - float
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x - y),

            // int - float
            (VMType::Float(x), VMType::Integer(m)) | (VMType::Integer(m), VMType::Float(x)) => {
                VMType::Float(m as f64 - x)
            }

            (a, b) => panic!("Cannot substract {a} and {b}"),
        }
    }
}

impl std::ops::Mul for VMType {
    type Output = VMType;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // int * int
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m * n),

            // float * float
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x * y),

            // float * float
            (VMType::Float(x), VMType::Integer(m)) | (VMType::Integer(m), VMType::Float(x)) => {
                VMType::Float(x * m as f64)
            }

            (a, b) => panic!("Cannot multiply {a} and {b}"),
        }
    }
}

impl std::ops::Add for VMType {
    type Output = VMType;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // str + str
            (VMType::String(a), VMType::String(b)) => VMType::String(a + &b),

            // int + int
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m + n),

            // float + float
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x + y),

            // int + float
            (VMType::Integer(m), VMType::Float(x)) | (VMType::Float(x), VMType::Integer(m)) => {
                VMType::Float(m as f64 + x)
            }

            // array + e and e + array
            (other, VMType::Array(mut xs)) | (VMType::Array(mut xs), other) => {
                xs.push(other);
                VMType::Array(xs)
            }

            (a, b) => panic!("Cannot add {a} and {b}"),
        }
    }
}

impl std::ops::Rem for VMType {
    type Output = VMType;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // int % int
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m % n),

            // float % float
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x % y),

            // int % float
            (VMType::Integer(m), VMType::Float(x)) => VMType::Float(m as f64 % x),

            // float % int
            (VMType::Float(x), VMType::Integer(m)) => VMType::Float(x % m as f64),

            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}

impl std::ops::Div for VMType {
    type Output = VMType;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // int / int
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Float(m as f64 / n as f64),

            // float / float
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x / y),

            // int / float
            (VMType::Integer(m), VMType::Float(x)) => VMType::Float(m as f64 / x),

            // float / int
            (VMType::Float(x), VMType::Integer(m)) => VMType::Float(x / m as f64),

            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}
struct Node(VMType, Option<Box<Node>>);

pub struct VMStack {
    head: Option<Box<Node>>,
}

impl VMStack {
    pub fn new() -> Self {
        VMStack { head: None }
    }

    /// Push a value on the top of the stack
    pub fn push(&mut self, value: VMType) {
        self.head = Some(Box::new(Node(value, self.head.take())));
    }

    /// Pop a value from the top of the stack
    pub fn pop(&mut self) -> Option<VMType> {
        self.head.take().map(|node| {
            self.head = node.1;
            node.0
        })
    }

    pub fn peek(&self) -> Option<&VMType> {
        self.head.as_ref().map(|node| &node.0)
    }

    pub fn clear(&mut self) {
        self.head = None;
    }
}
