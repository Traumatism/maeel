pub mod node;

use crate::stack::node::Node;

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
    pub head: Option<Node<T>>,
}

impl<T> Stack<T> {
    /// Create a new empty stack
    pub fn new() -> Stack<T> {
        Stack { head: None }
    }

    /// Push a value to the top of the stack
    pub fn push(&mut self, value: T) {
        let mut node = Node::new(value);

        if let Some(stack) = std::mem::replace(&mut self.head, None) {
            node.next = Some(Box::new(stack))
        }

        self.head = Some(node);
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
