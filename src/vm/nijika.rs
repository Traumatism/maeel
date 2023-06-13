use hashbrown::HashMap;

use super::*;

pub struct NijikaVM<const MAX_SIZE: usize> {
    buffer: [MaeelType; MAX_SIZE], /* Main stack buffer */
    sp: usize,                     /* Stack pointer */
    vars: HashMap<String, MaeelType>,
}

impl<const MAX_SIZE: usize> Default for NijikaVM<MAX_SIZE> {
    fn default() -> NijikaVM<MAX_SIZE> {
        NijikaVM {
            vars: HashMap::default(),
            buffer: unsafe { std::mem::zeroed() },
            sp: 0,
        }
    }
}

impl<const MAX_SIZE: usize> MaeelVM for NijikaVM<MAX_SIZE> {
    fn push(&mut self, item: MaeelType) -> VMOutput<()> {
        /* Making sure the stack is not filled */
        assert!(self.sp < self.buffer.len(), "Stack overflow");

        unsafe { ptr::write(self.buffer.as_mut_ptr().add(self.sp), item) }

        self.sp += 1; /* Increase stack pointer */

        Ok(())
    }

    fn pop(&mut self) -> VMOutput<MaeelType> {
        /* Making sure the stack is not empty */
        assert!(self.sp > 0, "Stack is empty");

        self.sp -= 1; /* Decrease stack pointer */

        unsafe { Ok(ptr::read(self.buffer.as_ptr().add(self.sp))) }
    }

    fn dup(&mut self) -> VMOutput<()> {
        /* Making sure the stack is not empty */
        assert!(self.sp > 0, "Stack is empty");
        assert!(self.sp < self.buffer.len(), "Stack overflow");

        let buffer_ptr = self.buffer.as_mut_ptr();

        unsafe {
            ptr::write(
                buffer_ptr.add(self.sp),
                (*buffer_ptr.add(self.sp - 1)).clone(),
            )
        }

        self.sp += 1; /* Increase stack pointer */

        Ok(())
    }

    fn swap(&mut self) -> VMOutput<()> {
        /* Making sure the stack contains at least two values */
        assert!(self.sp > 1, "Stack has less than two elements");

        let buffer_ptr = self.buffer.as_mut_ptr();

        unsafe { ptr::swap(buffer_ptr.add(self.sp - 1), buffer_ptr.add(self.sp - 2)) }

        Ok(())
    }

    fn fastpop(&mut self) -> VMOutput<()> {
        /* Making sure the stack is not empty */
        assert!(self.sp > 0, "Stack is empty");

        self.sp -= 1; /* Decrease stack pointer */

        Ok(())
    }

    fn clear(&mut self) -> VMOutput<()> {
        self.sp = 0; /* Reset stack pointer */

        Ok(())
    }

    fn peek(&self) -> VMOutput<&MaeelType> {
        /* Making sure the stack is not empty */
        assert!(self.sp > 0, "Stack is empty");

        unsafe { Ok(&*self.buffer.as_ptr().add(self.sp - 1)) }
    }

    fn rot(&mut self) -> VMOutput<()> {
        /* Making sure the stack contains at least three values */
        assert!(self.sp > 2, "Stack has less than three elements");

        let buffer_ptr = self.buffer.as_mut_ptr();

        unsafe {
            let node1 = buffer_ptr.add(self.sp - 1);
            let node2 = buffer_ptr.add(self.sp - 2);
            let node3 = buffer_ptr.add(self.sp - 3);

            let temp = ptr::read(node1);

            ptr::swap(node1, node2);
            ptr::swap(node2, node3);
            ptr::write(node3, temp)
        }

        Ok(())
    }

    fn over(&mut self) -> VMOutput<()> {
        /* Making sure the stack contains at least two values */
        assert!(self.sp > 1, "Stack has less than two elements");
        assert!(self.sp < self.buffer.len(), "Stack overflow");

        let buffer_ptr = self.buffer.as_mut_ptr();

        unsafe {
            ptr::write(
                buffer_ptr.add(self.sp),
                ptr::read(buffer_ptr.add(self.sp - 2)),
            )
        }

        self.sp += 1; /* Increase stack pointer */

        Ok(())
    }

    fn push_variable(&mut self, name: String, value: MaeelType) -> VMOutput<()> {
        self.vars.insert(name, value);

        Ok(())
    }

    fn get_variable(&mut self, name: String) -> VMOutput<&MaeelType> {
        if let Some(value) = self.vars.get(&name) {
            Ok(value)
        } else {
            Err("Variable not found".into())
        }
    }
}
