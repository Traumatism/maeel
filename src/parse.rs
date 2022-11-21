use crate::tokenize::{Operator, Token};
use crate::vm::Stack;

pub fn parse(tokens: &mut Stack<Token>, vm: &mut Stack<isize>) {
    while let Some(token) = tokens.pop() {
        match token {
            Token::Operator(operator) => {
                // Check for a and b in the expression,
                // if they are not present, take them from
                // the VM stack.
                let a = tokens
                    .pop()
                    .unwrap_or_else(|| Token::Number(vm.pop().expect("Missing `a`")));

                let b = tokens
                    .pop()
                    .unwrap_or_else(|| Token::Number(vm.pop().expect("Missing `b`")));

                let (a_val, b_val) = match (a, b) {
                    (Token::Number(a), Token::Number(b)) => (a, b),
                    _ => panic!("`a` and `b` must be integers"),
                };

                vm.push(a_val);
                vm.push(b_val);

                match operator {
                    Operator::Mul => vm.mul(),
                    Operator::Plus => vm.add(),
                    Operator::Minus => vm.sub(),
                }
            }
            Token::Number(value) => vm.push(value),
            Token::Dump => vm.dump(),
            Token::Erase => {
                while vm.pop().is_some() {
                    continue;
                }
            }

            _ => panic!(),
        }
    }
}
