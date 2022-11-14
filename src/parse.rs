use crate::stack::Stack;
use crate::tokenize::{Operator, Token};
use crate::vm::MaeelMachine;

pub fn parse(tokens: &mut Stack<Token>, vm: &mut MaeelMachine<isize>) {
    while let Some(token) = tokens.pop() {
        match token {
            Token::Operator(operator) => {
                let (a, b) = match (
                    tokens
                        .pop()
                        .unwrap_or_else(|| Token::Number(vm.pop().expect("Missing `a`"))),
                    tokens
                        .pop()
                        .unwrap_or_else(|| Token::Number(vm.pop().expect("Missing `b`"))),
                ) {
                    (Token::Number(a), Token::Number(b)) => (a, b),
                    _ => panic!("`a` and `b` must be integers"),
                };

                vm.push(a);
                vm.push(b);

                match operator {
                    Operator::Plus => vm.add(),
                    Operator::Minus => vm.sub(),
                }
            }
            Token::Number(value) => vm.push(value),
            _ => panic!(),
        }
    }
}
