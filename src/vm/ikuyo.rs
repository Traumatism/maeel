use super::*;

pub struct IkuyoVM<T, const MAX_SIZE: usize> {
    buffer: [T; MAX_SIZE],
    top: usize,
}

impl<T, const MAX_SIZE: usize> Default for IkuyoVM<T, MAX_SIZE> {
    fn default() -> IkuyoVM<T, MAX_SIZE> {
        IkuyoVM {
            buffer: unsafe { std::mem::zeroed() },
            top: 0,
        }
    }
}

impl<T: Clone, const MAX_SIZE: usize> MaeelVM for IkuyoVM<T, MAX_SIZE> {
    type Data = T;

    fn push(&mut self, item: T) -> VMOutput<()> {
        assert!(self.top < self.buffer.len(), "Stack overflow");

        unsafe {
            ptr::write(self.buffer.as_mut_ptr().add(self.top), item);
        }

        self.top += 1;

        Ok(())
    }

    fn pop(&mut self) -> VMOutput<T> {
        if self.top == 0 {
            Err("Stack is empty".into())
        } else {
            self.top -= 1;

            unsafe { Ok(ptr::read(self.buffer.as_ptr().add(self.top))) }
        }
    }

    fn dup(&mut self) -> VMOutput<()> {
        if let Ok(item) = self.peek() {
            self.push(item.clone())?;
        }

        Ok(())
    }

    fn swap(&mut self) -> VMOutput<()> {
        if self.top < 2 {
            return Err("Not enough elements on the stack".into());
        }

        unsafe {
            ptr::swap(
                self.buffer.as_mut_ptr().add(self.top - 1),
                self.buffer.as_mut_ptr().add(self.top - 2),
            );
        }

        Ok(())
    }

    fn fastpop(&mut self) -> VMOutput<()> {
        self.pop()?;

        Ok(())
    }

    fn clear(&mut self) -> VMOutput<()> {
        self.top = 0;

        Ok(())
    }

    fn peek(&self) -> VMOutput<&T> {
        if self.top == 0 {
            Err("Stack is empty".into())
        } else {
            unsafe { Ok(&*self.buffer.as_ptr().add(self.top - 1)) }
        }
    }

    fn rot(&mut self) -> VMOutput<()> {
        if self.top >= 3 {
            unsafe {
                let node1 = self.buffer.as_mut_ptr().add(self.top - 1);
                let node2 = self.buffer.as_mut_ptr().add(self.top - 2);
                let node3 = self.buffer.as_mut_ptr().add(self.top - 3);

                let temp = ptr::read(node1);

                ptr::write(node1, ptr::read(node2));
                ptr::write(node2, ptr::read(node3));
                ptr::write(node3, temp);
            }
        }

        Ok(())
    }

    fn over(&mut self) -> VMOutput<()> {
        if self.top >= 2 {
            self.push(unsafe { ptr::read(self.buffer.as_ptr().add(self.top - 2)) })?;
        }

        Ok(())
    }
}
