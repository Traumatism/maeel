use super::super::MaeelMachine;

impl<T: std::ops::Sub<Output = T>> MaeelMachine<T> {
    /// Pop the two top-most values from the stack
    /// and push their difference (second - first)
    pub fn sub(&mut self) {
        let a = self.stack.pop();
        let b = self.stack.pop();

        self.stack.push(b.unwrap() - a.unwrap())
    }
}
