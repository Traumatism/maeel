mod frame;
use frame::Frame;

pub struct Stack<T> {
    pub head: Option<Frame<T>>,
}

impl<T> Default for Stack<T> {
    /// Create a new empty stack
    fn default() -> Stack<T> {
        Stack { head: None }
    }
}

impl<T: Clone> Stack<T> {
    pub fn dup(&mut self) {
        let a = self.fast_pop_1();

        self.push(a.clone());
        self.push(a)
    }

    #[allow(dead_code)]
    pub fn peek(&mut self) -> T {
        let a = self.fast_pop_1();
        self.push(a.clone());

        a
    }
}

impl<T> Stack<T> {
    pub fn clear(&mut self) {
        while self.head.is_some() {
            self.fast_pop_2()
        }
    }

    pub fn swap(&mut self) {
        let a = self.fast_pop_1();
        let b = self.fast_pop_1();

        self.push(a);
        self.push(b);
    }

    pub fn push(&mut self, value: T) {
        let mut node = Frame::new(value);

        if let Some(stack) = std::mem::replace(&mut self.head, None) {
            node.next = Some(Box::new(stack))
        }

        self.head = Some(node);
    }
    pub fn fast_pop_2(&mut self) {
        self.head = std::mem::replace(&mut self.head, None)
            .unwrap()
            .next
            .map(|n| *n)
    }

    pub fn fast_pop_1(&mut self) -> T {
        let stack = std::mem::replace(&mut self.head, None).unwrap();
        self.head = stack.next.map(|n| *n);
        stack.value
    }

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
