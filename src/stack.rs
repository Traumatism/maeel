use std::vec::Vec;

use crate::enums::vmtype::VMType;

#[derive(Default, Clone)]
pub struct Stack {
    data: Vec<VMType>,
}

impl Stack {
    /// Pushes an element onto the top of the stack.
    pub fn push(&mut self, element: VMType) {
        self.data.push(element);
    }

    /// Removes the top element from the stack and returns it, if the stack is not empty.
    pub fn pop(&mut self) -> Option<VMType> {
        self.data.pop()
    }

    /// Returns the number of elements in the stack.
    pub fn size(&self) -> usize {
        self.data.len()
    }

    /// Rotates the top three elements of the stack.
    pub fn rotate(&mut self) {
        if self.data.len() >= 3 {
            let third = self.data.pop().unwrap();
            let second = self.data.pop().unwrap();
            let first = self.data.pop().unwrap();

            self.data.push(second);
            self.data.push(third);
            self.data.push(first);
        }
    }

    /// Duplicates the top element of the stack.
    pub fn dup(&mut self) {
        if let Some(x) = self.data.last() {
            self.data.push(x.clone());
        }
    }

    /// Pushes a copy of the second element from the top onto the top of the stack.
    pub fn over(&mut self) {
        if self.data.len() >= 2 {
            let second = &self.data[self.data.len() - 2];
            self.data.push(second.clone());
        }
    }

    /// Swaps the top two elements of the stack.
    pub fn swap(&mut self) {
        if self.data.len() >= 2 {
            let second = self.data.pop().unwrap();
            let first = self.data.pop().unwrap();
            self.data.push(second);
            self.data.push(first);
        }
    }

    /// Removes all elements from the stack.
    pub fn clear(&mut self) {
        self.data.clear()
    }
}
