use super::super::MaeelMachine;

impl<T: std::ops::Add<Output = T>> MaeelMachine<T> {
    /// Pop the two top-most values from the stack
    /// and push their sum
    pub fn add(&mut self) {
        let a = self.stack.pop();
        let b = self.stack.pop();

        self.stack.push(a.unwrap() + b.unwrap())
    }
}
