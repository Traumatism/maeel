use std::io::Write;

use parse::parse;
use tokenize::tokenize;
use vm::MaeelMachine;

mod parse;
mod stack;
mod tokenize;
mod utils;
mod vm;

fn main() {
    let mut vm: MaeelMachine<isize> = crate::vm::MaeelMachine::new();

    loop {
        let mut input = String::new();

        let (_, _, _) = (
            std::io::stdout().write(">>> ".as_bytes()), // write the prompt
            std::io::stdout().flush(),                  // flush stdout (no return)
            std::io::stdin().read_line(&mut input),     // read the user input
        );

        parse(&mut tokenize(&input), &mut vm);
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn base() {
        let mut vm = crate::vm::MaeelMachine::new();

        vm.push(1); // tail
        vm.push(2); // mid
        vm.push(3); // head

        assert_eq!(vm.pop(), Some(3)); // head
        assert_eq!(vm.pop(), Some(2)); // mid
        assert_eq!(vm.pop(), Some(1)); // tail
    }

    #[test]
    fn sub() {
        let mut vm = crate::vm::MaeelMachine::new();

        vm.push(5);
        vm.push(6);

        vm.sub();

        assert_eq!(vm.pop().unwrap(), -1)
    }

    #[test]
    fn mul() {
        let mut vm = crate::vm::MaeelMachine::new();

        vm.push(5);
        vm.push(6);

        vm.mul();

        assert_eq!(vm.pop().unwrap(), 30)
    }

    #[test]
    fn add() {
        let mut vm = crate::vm::MaeelMachine::new();

        vm.push(5.5);
        vm.push(5.6);

        vm.add();

        assert_eq!(vm.pop().unwrap(), 11.1)
    }
}
