use tokenize::{lex_into_tokens, parse_into_instructions};

mod parse;
mod tokenize;
mod utils;
mod vm;

fn main() {
    let content = std::fs::read_to_string("hello.mae").unwrap();
    let mut instructions = parse_into_instructions(&mut lex_into_tokens(&content));
}

#[cfg(test)]
mod tests {
    #[test]
    fn base() {
        let mut vm = crate::vm::Stack::new();

        vm.push(1); // tail
        vm.push(2); // mid
        vm.push(3); // head

        assert_eq!(vm.pop(), Some(3)); // head
        assert_eq!(vm.pop(), Some(2)); // mid
        assert_eq!(vm.pop(), Some(1)); // tail
    }

    #[test]
    fn sub() {
        let mut vm = crate::vm::Stack::new();

        vm.push(5);
        vm.push(6);

        vm.sub();

        assert_eq!(vm.pop().unwrap(), -1)
    }

    #[test]
    fn mul() {
        let mut vm = crate::vm::Stack::new();

        vm.push(5);
        vm.push(6);

        vm.mul();

        assert_eq!(vm.pop().unwrap(), 30)
    }

    #[test]
    fn add() {
        let mut vm = crate::vm::Stack::new();

        vm.push(5.5);
        vm.push(5.6);

        vm.add();

        assert_eq!(vm.pop().unwrap(), 11.1)
    }
}
