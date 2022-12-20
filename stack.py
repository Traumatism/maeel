from typing import Generic, Optional, TypeVar


T = TypeVar("T")


class Frame(Generic[T]):
    """Stack frame"""

    def __init__(self, value: T, child: Optional["Frame[T]"]) -> None:
        self.value = value
        self.child = child
        super().__init__()


class Stack(Generic[T]):
    """Stack data structure implementation"""

    def __init__(self) -> None:
        self.head: Optional[Frame] = None
        super().__init__()

    def peek(self) -> T:
        """Get the head element without poping it"""
        e = self.pop()
        self.push(e)
        return e

    @property
    def is_empty(self) -> bool:
        """Check if the stack is empty"""
        return self.head == None

    def clear(self):
        """Clear the stack"""
        while self.is_empty is False:
            self.pop()

    def dup(self):
        """Dup the head element"""
        self.push(self.peek())

    def swap(self):
        """Swap the two head elements"""
        a, b = self.pop(), self.pop()
 
        self.push(b)
        self.push(a)

    def push(self, value: T):
        """Push a new element"""
        self.head = Frame(value, self.head)

    def pop(self) -> T:
        """Pop the head element"""
        if self.head is None:
            raise

        backup_head = self.head
        self.head = self.head.child
        return backup_head.value


if __name__ == "__main__":
    stack = Stack()
    stack.push(1)
    stack.push(2)
    stack.push(3)
    
    assert stack.pop() == 3 
    assert stack.pop() == 2
    assert stack.pop() == 1
