use crate::vm::Stack;

impl<T: std::ops::Add<Output = T>> Stack<T> {
    /// Pop the two top-most values from the stack
    /// and push their sum
    pub fn add(&mut self) {
        let a = self.pop();
        let b = self.pop();

        self.push(a.unwrap() + b.unwrap())
    }
}
