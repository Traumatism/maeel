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
            "print" => {
                let e = vm.fast_pop();
                println!("{:?}", &e);
                vm.push(e);
            }
            "pop" => {
                vm.fast_pop();
            }

            // Remove the top array
            "reverse" => {
                if let VMTypes::Array(mut vec) = vm.fast_pop() {
                    vec.reverse();
                    vm.push(VMTypes::Array(vec));
                } else {
                    panic!()
                }
            }

            // Pop n elements from the stack and push it as an array
            "take" => {
                if let Token::Integer(n) = tokens.fast_pop() {
                    let arr = (0..n).map(|_| vm.fast_pop()).collect();
                    vm.push(VMTypes::Array(arr))
                } else {
                    panic!("Expected an integer after `take` instruction")
                }
            }
            identifier => panic!("Unknown identifier: {identifier}"),
        },
        token => {
            panic!("Panic with token: {token:?}")
        }
    }
}
