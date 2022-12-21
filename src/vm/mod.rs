mod frame;
use frame::Frame;

pub struct Stack<T> {
    pub head: Option<Frame<T>>,
}

impl<T> Default for Stack<T> {
    fn default() -> Stack<T> {
        Stack { head: None }
    }
}

impl<T: Clone> Stack<T> {
    /// Duplicate the head value
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

    /// Swap the two head values
    pub fn swap(&mut self) {
        let a = self.fast_pop_1();
        let b = self.fast_pop_1();

        self.push(a);
        self.push(b);
    }

    /// Push a new value to the stack
    pub fn push(&mut self, value: T) {
        let mut node = Frame::new(value);

        if let Some(stack) = std::mem::replace(&mut self.head, None) {
            node.next = Some(Box::new(stack))
        }

        self.head = Some(node);
    }

    /// Pop the head value without returning it
    pub fn fast_pop_2(&mut self) {
        self.head = std::mem::replace(&mut self.head, None)
            .unwrap()
            .next
            .map(|n| *n)
    }

    /// Pop the head value and return it (might result a runtime panic)
    pub fn fast_pop_1(&mut self) -> T {
        let stack = std::mem::replace(&mut self.head, None).unwrap();
        self.head = stack.next.map(|n| *n);
        stack.value
    }

    /// Pop the head value and return it in an Option
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
