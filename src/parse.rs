use crate::stack::Stack;
use crate::tokenize::{Operator, Token};
use crate::vm::MaeelMachine;

pub fn parse(tokens: &mut Stack<Token>, vm: &mut MaeelMachine<isize>) {
    while let Some(token) = tokens.pop() {
        match token {
            Token::Operator(operator) => match operator {
                Operator::Plus => vm.add(),
                Operator::Minus => vm.sub(),
            },
            Token::Number(number) => vm.push(number),
            _ => panic!(),
        }
    }
}
