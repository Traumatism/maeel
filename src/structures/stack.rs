use crate::enums::vmtype::VMType;
use std::mem::replace;

/// A frame in a stack
pub struct Frame(VMType, Option<Box<Frame>>);

impl Frame {
    pub fn new(value: VMType) -> Frame {
        Self(value, None)
    }
}

impl Clone for Frame {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}

/// Stack data structure implementation
pub struct Stack {
    pub head: Option<Frame>,
}

impl Clone for Stack {
    fn clone(&self) -> Self {
        Self {
            head: self.head.clone(),
        }
    }
}

#[allow(clippy::derivable_impls)]
impl Default for Stack {
    fn default() -> Stack {
        Stack { head: None }
    }
}

impl Stack {
    /// Duplicate the head value
    pub fn dup(&mut self) {
        let a = self.pop().unwrap();
        self.push(a.clone());
        self.push(a)
    }
}

impl From<Vec<VMType>> for Stack {
    fn from(value: Vec<VMType>) -> Self {
        let mut new_stack = Stack::default();
        let mut cloned_value = value;

        cloned_value.reverse();

        cloned_value
            .iter()
            .for_each(|ele| new_stack.push(ele.clone()));

        new_stack
    }
}

impl Stack {
    /// Clear all the stack values
    pub fn clear(&mut self) {
        while self.head.is_some() {
            self.pop();
        }
    }

    /// Swap the two head values
    pub fn swap(&mut self) {
        let a = self.pop().unwrap();
        let b = self.pop().unwrap();

        self.push(a);
        self.push(b);
    }

    /// Push a new value to the stack
    pub fn push(&mut self, value: VMType) {
        let mut node = Frame::new(value);

        if let Some(stack) = replace(&mut self.head, None) {
            node.1 = Some(Box::new(stack))
        }

        self.head = Some(node);
    }

    /// Pop the head value and return it in an Option
    pub fn pop(&mut self) -> Option<VMType> {
        match replace(&mut self.head, None) {
            Some(stack) => {
                self.head = stack.1.map(|n| *n);
                Some(stack.0)
            }
            _ => None,
        }
    }

    /// Pop the head value and return it in an Option
    pub fn pop_nr(&mut self) {
        match replace(&mut self.head, None) {
            Some(stack) => {
                self.head = stack.1.map(|n| *n);
            }
            None => todo!(),
        }
    }

    /// Checks whether the stack is empty or not
    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        matches!(self.head, None)
    }

    pub fn reverse_stack(&mut self) {
        let mut temp_stack = Vec::default();

        while let Some(val) = self.pop() {
            temp_stack.push(val);
        }

        temp_stack.reverse();

        while let Some(val) = temp_stack.pop() {
            self.push(val);
        }
    }
}
