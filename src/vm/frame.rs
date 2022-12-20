pub struct Frame<T> {
    pub value: T,
    pub next: Option<Box<Frame<T>>>,
}

impl<T> Frame<T> {
    pub fn new(value: T) -> Frame<T> {
        Self { value, next: None }
    }
}
