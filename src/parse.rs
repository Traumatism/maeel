use crate::tokenize::Token;
use crate::vm::{Stack, VMTypes};

pub fn parse(tokens: &mut Stack<Token>, vm: &mut Stack<VMTypes>) {
    let top = tokens.pop();

    match top {
        Some(Token::Identifier(identifier)) => match &*identifier {
            "push" => {
                while let Some(next) = tokens.pop() {
                    vm.push(match next {
                        Token::Float(value) => VMTypes::Float(value),
                        Token::String(value) => VMTypes::String(value),
                        Token::Integer(value) => VMTypes::Integer(value),
                        _ => panic!(),
                    })
                }
            }
            "pop" => {
                vm.fast_pop();
            }
            "reverse" => {
                let mut vec = {
                    if let VMTypes::Array(vec) = vm.fast_pop() {
                        vec
                    } else {
                        panic!()
                    }
                };

                vec.reverse();

                vm.push(VMTypes::Array(vec));
            }
            "take" => {
                let i = {
                    if let Some(Token::Integer(n)) = tokens.pop() {
                        n
                    } else {
                        panic!()
                    }
                };

                let mut arr = Vec::new();

                for _ in 0..i {
                    arr.push(vm.fast_pop())
                }

                vm.push(VMTypes::Array(arr))
            }
            _ => panic!(),
        },
        token => {
            panic!("Panic with token: {token:?}")
        }
    }
}
