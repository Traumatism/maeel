#![allow(dead_code)]

use std::fmt::{Debug, Display};

use crate::stack::Stack;

mod ops;

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
    /// Print the top value
    pub fn print(&mut self) {
        let value = self.stack.pop().unwrap();
        println!("{}", value);
        self.stack.push(value)
    }

    pub fn dump(&mut self) {
        let mut values = Vec::new();

        while let Some(value) = self.pop() {
            values.push(value)
        }

        let values_str = values.iter().map(|v| format!("{}", v));
        let max_size = values_str.clone().max().unwrap().len();

        values_str
            .for_each(|value| println!("| {}{} |", value, " ".repeat(max_size - value.len())));
    }
}

impl<T: Debug> MaeelMachine<T> {
    /// Pop the top value and print it (debug)
    pub fn printdb(&mut self) {
        let value = self.stack.pop().unwrap();
        println!("{:?}", value);
        self.stack.push(value)
    }
}
