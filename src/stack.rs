use std::vec::Vec;

use crate::enums::vmtype::VMType;

#[derive(Default, Clone)]
pub struct Stack {
    data: Vec<VMType>,
}

impl Stack {
    pub fn push(&mut self, element: VMType) {
        self.data.push(element);
    }

    pub fn pop(&mut self) -> Option<VMType> {
        self.data.pop()
    }

    pub fn size(&self) -> usize {
        self.data.len()
    }

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

    pub fn dup(&mut self) {
        if let Some(x) = self.data.last() {
            self.data.push(x.clone());
        }
    }

    pub fn over(&mut self) {
        if self.data.len() >= 2 {
            let second = &self.data[self.data.len() - 2];
            self.data.push(second.clone());
        }
    }

    pub fn swap(&mut self) {
        if self.data.len() >= 2 {
            let second = self.data.pop().unwrap();
            let first = self.data.pop().unwrap();
            self.data.push(second);
            self.data.push(first);
        }
    }

    pub fn clear(&mut self) {
        self.data.clear()
    }
}
