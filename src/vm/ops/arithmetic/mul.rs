use crate::vm::Stack;

impl<T: std::ops::Mul<Output = T>> Stack<T> {
    /// Pop the two top-most values from the stack
    /// and push their product
    pub fn mul(&mut self) {
        let a = self.pop();
        let b = self.pop();

        self.push(a.unwrap() * b.unwrap())
    }
}
