pub type VMOutput<T> = Result<T, Box<dyn std::error::Error>>;

pub trait MaeelVM {
    type Data;

    /// Get the top value from the stack (without dropping it)
    fn peek(&self) -> VMOutput<&Self::Data>;

    /// Push a value to the stack
    fn push(&mut self, value: Self::Data) -> VMOutput<()>;

    /// Get the top value from the stack (and drop it)
    fn pop(&mut self) -> VMOutput<Self::Data>;

    /// Drop the top value from the stack
    fn fastpop(&mut self) -> VMOutput<()>;

    /// Clear the stack
    fn clear(&mut self) -> VMOutput<()>;

    /// Duplicate the highest value of the stack
    fn dup(&mut self) -> VMOutput<()>;

    /// Exchange the two highests values of the stack
    fn swap(&mut self) -> VMOutput<()>;

    /// Duplicate the 2nd highest value of the stack
    fn over(&mut self) -> VMOutput<()>;

    /// Rotate the three highests values of the stack
    fn rot(&mut self) -> VMOutput<()>;
}
