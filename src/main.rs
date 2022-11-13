use std::io::Write;

use tokenize::tokenize;

mod stack;
mod tokenize;
mod utils;
mod vm;

fn main() {
    loop {
        let mut input = String::new();

        let (_, _, _) = (
            std::io::stdout().write(">>> ".as_bytes()), // write the prompt
            std::io::stdout().flush(),                  // flush stdout (no return)
            std::io::stdin().read_line(&mut input),     // read the user input
        );

        let tokens = tokenize(&input);

        println!("{:?}", tokens);
    }
}

#[cfg(test)]
mod tests_operators {

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
