mod node;
mod ops;

use node::Node;

#[derive(Clone)]
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

impl<T: std::fmt::Display> Stack<T> {
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

impl<T: std::fmt::Debug> Stack<T> {
    pub fn dumpdb(&mut self) {
        let mut values = Vec::new();

        while let Some(value) = self.pop() {
            values.push(value)
        }

        values.iter().for_each(|v| println!("{:?}", v));
    }
}
