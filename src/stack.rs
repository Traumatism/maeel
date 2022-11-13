use std::fmt::{Debug, Display};

/// Stack data structure implementation
/// - https://en.wikipedia.org/wiki/Stack_(abstract_data_type)
pub struct Stack<T> {
    values: Vec<T>,
}

impl<T> Stack<T> {
    pub fn new() -> Self {
        Self { values: Vec::new() }
    }

    /// Push a value to the top
    pub fn push(&mut self, value: T) {
        self.values.push(value)
    }

    /// Pop the top-most value
    pub fn pop(&mut self) -> Option<T> {
        self.values.pop()
    }
}

impl<T: Debug> Display for Stack<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.values)
    }
}
