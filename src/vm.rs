#![allow(dead_code)]

use std::{
    fmt::{Debug, Display},
    ops::{Add, Mul, Sub},
};

use crate::stack::Stack;

/// Stack machine implementation
/// - https://en.wikipedia.org/wiki/Stack-oriented_programming
/// - https://en.wikipedia.org/wiki/Stack_machine
pub struct MaeelMachine<T> {
    stack: Stack<T>,
}

impl<T> MaeelMachine<T> {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
        }
    }

    /// Pop the top-most value from the stack
    pub fn pop(&mut self) -> Option<T> {
        self.stack.pop()
    }

    /// Push a new value to the Maeel stack
    pub fn push(&mut self, value: T) {
        self.stack.push(value)
    }
}

impl<T: Display> MaeelMachine<T> {
    /// Pop the top value and print it
    pub fn print(&mut self) {
        println!("{}", self.stack.pop().unwrap())
    }
}

impl<T: Debug> MaeelMachine<T> {
    /// Pop the top value and print it (debug)
    pub fn printdb(&mut self) {
        println!("{:?}", self.stack.pop().unwrap())
    }
}

impl<T: Add<Output = T>> MaeelMachine<T> {
    /// Pop the two top-most values from the stack
    /// and push their sum
    pub fn add(&mut self) {
        let a = self.stack.pop();
        let b = self.stack.pop();

        self.stack.push(a.unwrap() + b.unwrap())
    }
}

impl<T: Sub<Output = T>> MaeelMachine<T> {
    /// Pop the two top-most values from the stack
    /// and push their difference (second - first)
    pub fn sub(&mut self) {
        let a = self.stack.pop();
        let b = self.stack.pop();

        self.stack.push(b.unwrap() - a.unwrap())
    }
}

impl<T: Mul<Output = T>> MaeelMachine<T> {
    /// Pop the two top-most values from the stack
    /// and push their product
    pub fn mul(&mut self) {
        let a = self.stack.pop();
        let b = self.stack.pop();

        self.stack.push(a.unwrap() * b.unwrap())
    }
}
