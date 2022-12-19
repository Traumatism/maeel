mod frame;
use frame::Frame;

pub struct Stack<T> {
    pub head: Option<Frame<T>>,
}

impl<T> Default for Stack<T> {
    /// Create a new empty stack
    fn default() -> Stack<T> {
        Stack { head: None }
    }
}

impl<T> Stack<T> {
    /// Push a value to the top of the stack
    pub fn push(&mut self, value: T) {
        let mut node = Frame::new(value);

        if let Some(stack) = std::mem::replace(&mut self.head, None) {
            node.next = Some(Box::new(stack))
        }

        self.head = Some(node);
    }

    /// Pop the top-most value without checking for the existence
    /// (might result a panic).
    pub fn fast_pop(&mut self) -> T {
        let stack = std::mem::replace(&mut self.head, None).unwrap();
        self.head = stack.next.map(|n| *n);
        stack.value
    }

    /// Pop the top-most value from the stack
    pub fn pop(&mut self) -> Option<T> {
        match std::mem::replace(&mut self.head, None) {
            Some(stack) => {
                self.head = stack.next.map(|n| *n);
                Some(stack.value)
            }
            _ => None,
        }
    }
}
