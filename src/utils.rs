/// Get next/previous values in a vector
pub struct Peeker<T> {
    values: Vec<T>,
    cursor: usize,
}

impl<T: Clone> Peeker<T> {
    pub fn new(values: Vec<T>) -> Self {
        Self { values, cursor: 0 }
    }

    /// Get the previous value
    pub fn previous(&mut self) -> Option<T> {
        self.cursor -= 2;
        self.next()
    }

    /// Get the next value
    pub fn next(&mut self) -> Option<T> {
        if self.cursor >= self.values.len() {
            return None;
        }

        self.cursor += 1;

        Some(self.values[self.cursor - 1].to_owned())
    }
}
