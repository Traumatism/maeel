use hashbrown::HashMap;

use super::*;

struct VMNode<T> {
    value: T,             /* Node type */
    next: *mut VMNode<T>, /* Raw pointer to the next node */
}

impl<T> VMNode<T> {
    /// Create a new VMNode instance
    pub fn new(value: T) -> *mut Self {
        Box::into_raw(Box::new(VMNode {
            value,
            next: ptr::null_mut(), /* Mutable null pointer */
        }))
    }
}

pub struct BocchiVM {
    vars: HashMap<String, MaeelType>,
    head: *mut VMNode<MaeelType>, /* Raw pointer to the head node */
}

impl Default for BocchiVM {
    fn default() -> Self {
        BocchiVM {
            vars: HashMap::default(),
            head: ptr::null_mut(),
        }
    }
}

impl MaeelVM for BocchiVM {
    fn push(&mut self, value: MaeelType) -> VMOutput<()> {
        let future_head = VMNode::new(value); /* Create a new node */

        if !self.head.is_null() {
            unsafe {
                /* Set head as future_head next node */
                (*future_head).next = self.head;
            }
        }

        self.head = future_head; /* Replace head with future_head */

        Ok(())
    }

    fn pop(&mut self) -> VMOutput<MaeelType> {
        if self.head.is_null()
        /* Making sure the stack contains at least one value */
        {
            return Err("Stack is empty".into());
        }

        let current_head = unsafe { Box::from_raw(self.head) };

        /* Replace the current head with her next node */
        self.head = current_head.next;

        Ok(current_head.value)
    }

    fn fastpop(&mut self) -> VMOutput<()> {
        if self.head.is_null()
        /* Making sure the stack contains at least one value */
        {
            return Err("Stack is empty".into());
        }

        let current_head = unsafe { Box::from_raw(self.head) };

        /* Replace the current head with her next node */
        self.head = current_head.next;

        Ok(())
    }

    fn peek(&self) -> VMOutput<&MaeelType> {
        if self.head.is_null()
        /* Making sure the stack contains at least one value */
        {
            return Err("Stack is empty".into());
        }

        Ok(unsafe { &(*self.head).value })
    }

    fn clear(&mut self) -> VMOutput<()> {
        while !self.head.is_null()
        /* Dropping all the next_node until head is not null */
        {
            self.head = unsafe { Box::from_raw(self.head) }.next
        }

        Ok(())
    }

    fn swap(&mut self) -> VMOutput<()> {
        if self.head.is_null() || unsafe { (*self.head).next.is_null() }
        /* Making sure the stack contains at least two values */
        {
            return Err("Not enough elements on the stack".into());
        }

        unsafe {
            ptr::swap(&mut (*self.head).value, &mut (*(*self.head).next).value);
        }

        Ok(())
    }

    fn dup(&mut self) -> VMOutput<()> {
        if self.head.is_null()
        /* Making sure the stack contains at least one value */
        {
            return Err("Stack is empty".into());
        }

        self.push(unsafe { (*self.head).value.clone() })
    }

    fn over(&mut self) -> VMOutput<()> {
        if self.head.is_null() || unsafe { (*self.head).next.is_null() }
        /* Making sure the stack contains at least two values */
        {
            return Err("Stack has less than two elements".into());
        }

        self.push(
            /* Get the value under the stack top */
            unsafe { (*(*self.head).next).value.clone() },
        )
    }

    fn rot(&mut self) -> VMOutput<()> {
        if self.head.is_null()
            || unsafe { (*self.head).next.is_null() }
            || unsafe { (*(*self.head).next).next.is_null() }
        /* Making sure the stack contains at least three values */
        {
            return Err("Stack has less than three elements".into());
        }

        unsafe {
            let node1 /* top node */ = &mut *self.head;
            let node2 /* mid node */ = &mut *(*self.head).next;
            let node3 /* bot node */ = &mut *(*(*self.head).next).next;

            /* Store the node1 value in a temp variable,
            as we update its value first */
            let temp = ptr::read(&node1.value);

            ptr::swap /* V(top) <- V(mid) */ (&mut node1.value, &mut node2.value);
            ptr::swap /* V(mid) <- V(bot) */ (&mut node2.value, &mut node3.value);

            ptr::write /* V(bot) <- V(top) */ (&mut node3.value, temp);
        }

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
