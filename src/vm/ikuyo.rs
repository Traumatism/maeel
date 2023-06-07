use super::*;

pub struct IkuyoVM<T, const MAX_SIZE: usize> {
    buffer: [T; MAX_SIZE], /* Main stack buffer */
    sp: usize,             /* Stack pointer */
}

impl<T, const MAX_SIZE: usize> Default for IkuyoVM<T, MAX_SIZE> {
    fn default() -> IkuyoVM<T, MAX_SIZE> {
        IkuyoVM {
            buffer: unsafe { std::mem::zeroed() },
            sp: 0,
        }
    }
}

impl<T: Clone, const MAX_SIZE: usize> MaeelVM for IkuyoVM<T, MAX_SIZE> {
    type Data = T;

    fn push(&mut self, item: T) -> VMOutput<()> {
        /* Making sure the stack is not filled */
        assert!(self.sp < self.buffer.len(), "Stack overflow");

        unsafe { ptr::write(self.buffer.as_mut_ptr().add(self.sp), item) }

        self.sp += 1; /* Increase stack pointer */

        Ok(())
    }

    fn pop(&mut self) -> VMOutput<T> {
        /* Making sure the stack is not empty */
        assert!(self.sp > 0, "Stack is empty");

        self.sp -= 1; /* Decrease stack pointer */

        unsafe { Ok(ptr::read(self.buffer.as_ptr().add(self.sp))) }
    }

    fn dup(&mut self) -> VMOutput<()> {
        /* Making sure the stack is not empty */
        assert!(self.sp > 0, "Stack is empty");

        self.push(self.peek()?.clone())
    }

    fn swap(&mut self) -> VMOutput<()> {
        /* Making sure the stack contains at least two values */
        assert!(self.sp > 1, "Stack has less than two elements");

        unsafe {
            ptr::swap(
                self.buffer.as_mut_ptr().add(self.sp - 1),
                self.buffer.as_mut_ptr().add(self.sp - 2),
            )
        }

        Ok(())
    }

    fn fastpop(&mut self) -> VMOutput<()> {
        self.pop()?;

        Ok(())
    }

    fn clear(&mut self) -> VMOutput<()> {
        self.sp = 0;

        Ok(())
    }

    fn peek(&self) -> VMOutput<&T> {
        /* Making sure the stack is not empty */
        assert!(self.sp > 0, "Stack is empty");

        unsafe { Ok(&*self.buffer.as_ptr().add(self.sp - 1)) }
    }

    fn rot(&mut self) -> VMOutput<()> {
        /* Making sure the stack contains at least three values */
        assert!(self.sp > 2, "Stack has less than three elements");

        unsafe {
            let node1 = self.buffer.as_mut_ptr().add(self.sp - 1);
            let node2 = self.buffer.as_mut_ptr().add(self.sp - 2);
            let node3 = self.buffer.as_mut_ptr().add(self.sp - 3);

            let temp = ptr::read(node1);

            ptr::write(node1, ptr::read(node2));
            ptr::write(node2, ptr::read(node3));
            ptr::write(node3, temp)
        }

        Ok(())
    }

    fn over(&mut self) -> VMOutput<()> {
        /* Making sure the stack contains at least two values */
        assert!(self.sp > 1, "Stack has less than two elements");

        self.push(unsafe { ptr::read(self.buffer.as_ptr().add(self.sp - 2)) })
    }
}
