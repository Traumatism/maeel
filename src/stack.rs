use std::mem;

/// A node in the stack
pub struct Node<T> {
    value: T,
    next: Option<Box<Node<T>>>,
}

impl<T> Node<T> {
    fn new(value: T) -> Node<T> {
        Self { value, next: None }
    }
}

/// Stack value structure implementation
pub struct Stack<T> {
    stack: Option<Node<T>>,
}

impl<T> Stack<T> {
    pub fn new() -> Stack<T> {
        Stack { stack: None }
    }

    /// Push a value to the top of the stack
    pub fn push(&mut self, value: T) {
        let mut node = Node::new(value);

        if let Some(stack) = mem::replace(&mut self.stack, None) {
            node.next = Some(Box::new(stack))
        }

        self.stack = Some(node);
    }

    /// Pop the top-most value from the stack
    pub fn pop(&mut self) -> Option<T> {
        match mem::replace(&mut self.stack, None) {
            Some(stack) => {
                self.stack = stack.next.map(|n| *n);
                Some(stack.value)
            }
            _ => None,
        }
    }
}
