use crate::vm::Stack;

impl<T: std::ops::Sub<Output = T>> Stack<T> {
    /// Pop the two top-most values from the stack
    /// and push their difference (second - first)
    pub fn sub(&mut self) {
        let a = self.pop();
        let b = self.pop();

        self.push(b.unwrap() - a.unwrap())
    }
}
