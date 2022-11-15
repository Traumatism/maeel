use super::super::MaeelMachine;

impl<T: std::ops::Mul<Output = T>> MaeelMachine<T> {
    /// Pop the two top-most values from the stack
    /// and push their product
    pub fn mul(&mut self) {
        let a = self.stack.pop();
        let b = self.stack.pop();

        self.stack.push(a.unwrap() * b.unwrap())
    }
}
