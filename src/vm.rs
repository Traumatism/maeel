use crate::lexer::*;

use std::error::Error;
use std::ptr::*;

#[derive(Clone)]
pub enum VMType {
    Float(f32),
    Integer(i64),
    String(String),
    Array(Vec<VMType>),
    Function(Vec<Token>),
}

struct Node(VMType, *mut Node);

pub struct VM(*mut Node);

impl Default for VM {
    fn default() -> Self {
        VM(null_mut())
    }
}

impl VM {
    pub fn push(&mut self, value: VMType) {
        let new_node = Box::into_raw(Box::new(Node(value, null_mut())));

        if !self.0.is_null() {
            unsafe {
                (*new_node).1 = self.0;
            }
        }

        self.0 = new_node;
    }

    pub fn pop(&mut self) -> Result<VMType, Box<dyn Error>> {
        if self.0.is_null() {
            return Err("Stack is empty".into());
        }

        unsafe {
            let node = Box::from_raw(self.0);
            self.0 = node.1;

            Ok(node.0)
        }
    }

    pub fn peek(&self) -> Result<&VMType, Box<dyn Error>> {
        if self.0.is_null() {
            return Err("Stack is empty".into());
        }

        unsafe { Ok(&(*self.0).0) }
    }

    pub fn clear(&mut self) {
        while !self.0.is_null() {
            unsafe {
                self.0 = Box::from_raw(self.0).1;
            }
        }
    }

    pub fn swap(&mut self) -> Result<(), Box<dyn Error>> {
        if self.0.is_null() {
            return Err("Stack is empty".into());
        }

        if unsafe { (*self.0).1.is_null() } {
            return Err("Stack has only one element".into());
        }

        unsafe {
            let node1 = &mut *self.0;
            let node2 = &mut *node1.1;

            let temp = read(&node1.0);

            write(&mut node1.0, read(&node2.0));
            write(&mut node2.0, temp);
        }

        Ok(())
    }

    pub fn dup(&mut self) -> Result<(), Box<dyn Error>> {
        if self.0.is_null() {
            return Err("Stack is empty".into());
        }

        self.push(unsafe { (*self.0).0.clone() });

        Ok(())
    }

    pub fn over(&mut self) -> Result<(), Box<dyn Error>> {
        if self.0.is_null() {
            return Err("Stack is empty".into());
        }

        if unsafe { (*self.0).1.is_null() } {
            return Err("Stack has only one element".into());
        }

        self.push(unsafe { (*(*self.0).1).0.clone() });

        Ok(())
    }

    pub fn rot(&mut self) -> Result<(), Box<dyn Error>> {
        if self.0.is_null() {
            return Err("Stack is empty".into());
        }

        if unsafe { (*self.0).1.is_null() } || unsafe { (*(*self.0).1).1.is_null() } {
            return Err("Stack has less than three elements".into());
        }

        unsafe {
            let node1 = &mut *self.0;
            let node2 = &mut *(*self.0).1;
            let node3 = &mut *(*(*self.0).1).1;

            let temp = read(&node1.0);

            write(&mut node1.0, read(&node2.0));
            write(&mut node2.0, read(&node3.0));
            write(&mut node3.0, temp);
        }

        Ok(())
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        self.clear()
    }
}

impl std::fmt::Display for VMType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
                Some(b.total_cmp(&(*a as f32)))
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
            (VMType::Float(a), VMType::Integer(b)) => *a == (*b as f32),
            (VMType::Integer(a), VMType::Float(b)) => (*a as f32) == *b,
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
                VMType::Float(m as f32 - x)
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
                VMType::Float(x * m as f32)
            }

            (VMType::Integer(m), VMType::String(s)) | (VMType::String(s), VMType::Integer(m)) => {
                VMType::String(s.repeat(m as usize))
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
                VMType::Float(m as f32 + x)
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
            (VMType::Integer(m), VMType::Float(x)) => VMType::Float(m as f32 % x),

            // float % int
            (VMType::Float(x), VMType::Integer(m)) => VMType::Float(x % m as f32),

            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}

impl std::ops::Div for VMType {
    type Output = VMType;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // int / int
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Float(m as f32 / n as f32),

            // float / float
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x / y),

            // int / float
            (VMType::Integer(m), VMType::Float(x)) => VMType::Float(m as f32 / x),

            // float / int
            (VMType::Float(x), VMType::Integer(m)) => VMType::Float(x / m as f32),

            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}
