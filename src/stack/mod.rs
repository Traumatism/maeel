pub mod node;

use std::fmt::Debug;

use crate::stack::node::Node;

#[derive(Clone, Debug)]
/// Stack data structure implementation
/// ```
/// /*
///    node
///   /    \
/// value   node
///        /    \
///      value   node
///             /    \
///           value  none
/// */
/// ```
pub struct Stack<T> {
    stack: Option<Node<T>>,
}

impl<T> Stack<T> {
    /// Create a new empty stack
    pub fn new() -> Stack<T> {
        Stack { stack: None }
    }

    /// Push a value to the top of the stack
    pub fn push(&mut self, value: T) {
        let mut node = Node::new(value);

        if let Some(stack) = std::mem::replace(&mut self.stack, None) {
            node.next = Some(Box::new(stack))
        }

        self.stack = Some(node);
    }

    /// Pop the top-most value from the stack
    pub fn pop(&mut self) -> Option<T> {
        match std::mem::replace(&mut self.stack, None) {
            Some(stack) => {
                self.stack = stack.next.map(|n| *n);
                Some(stack.value)
            }
            _ => None,
        }
    }
}

impl<T: Clone + Debug> Stack<T> {
    #[allow(dead_code)]
    pub fn repr(&self) {
        let cloned = &mut (*self).clone();

        while let Some(node) = cloned.pop() {
            println!("{:?}", node)
        }
    }
}
