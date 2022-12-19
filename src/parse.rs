use crate::enums::{Token, VMType};
use crate::vm::Stack;

pub fn parse(tokens: &mut Stack<Token>, vm: &mut Stack<VMType>) {
    let top = tokens.pop();

    match top {
        Some(Token::Identifier(identifier)) => {
            match &*identifier {
                "push" => {
                    while let Some(next) = tokens.pop() {
                        vm.push(match next {
                            Token::Float(value) => VMType::Float(value),
                            Token::String(value) => VMType::String(value),
                            Token::Integer(value) => VMType::Integer(value),
                            _ => panic!("`push` only accept floats, strings and integers"),
                        })
                    }
                }

                "range" => {
                    let (b, a) = match (vm.fast_pop(), vm.fast_pop()) {
                        (VMType::Integer(b1), VMType::Integer(a1)) => (b1, a1),
                        _ => panic!(),
                    };

                    vm.push(VMType::Array(
                        (a..b).map(|n| VMType::Integer(n)).collect::<Vec<VMType>>(),
                    ))
                }

                "print" | "println" => {
                    let f = if identifier == "print" {
                        |content: String| print!("{content}")
                    } else {
                        |content: String| println!("{content}")
                    };

                    let e = vm.fast_pop();

                    match &e {
                        VMType::Float(x) => f(format!("{x}")),
                        VMType::Integer(n) => f(format!("{n}")),
                        VMType::String(c) => f(format!("{c}")),
                        _ => panic!("`{identifier}` only accept floats, strings and integers"),
                    }

                    vm.push(e);
                }

                "pop" => {
                    vm.fast_pop();
                }

                "product" => match vm.fast_pop() {
                    VMType::Array(array) => {
                        let p = array
                            .iter()
                            .map(|element| match element.clone() {
                                VMType::Integer(n) => *n as f64,
                                VMType::Float(x) => *x,
                                _ => panic!("`product` expect an array of integers/floats"),
                            })
                            .product::<f64>();

                        vm.push(VMType::Float(p));
                    }
                    _ => panic!("`product` expect an array of integers/floats"),
                },

                "sum" => match vm.fast_pop() {
                    VMType::Array(array) => {
                        let s = array
                            .iter()
                            .map(|element| match element.clone() {
                                VMType::Integer(n) => *n as f64,
                                VMType::Float(x) => *x,
                                _ => panic!("`sum` expect an array of integers/floats"),
                            })
                            .sum::<f64>();

                        vm.push(VMType::Float(s))
                    }
                    _ => panic!("`sum` expect an array of integers/floats"),
                },

                // Remove the top array
                "reverse" => match vm.fast_pop() {
                    VMType::Array(mut array) => {
                        array.reverse();
                        vm.push(VMType::Array(array))
                    }
                    _ => panic!("`reverse` expect an array on top of the stack"),
                },

                // Pop n elements from the stack and push it as an array
                "take" => match vm.fast_pop() {
                    VMType::Integer(n) => {
                        let arr = (0..n).map(|_| vm.fast_pop()).collect();
                        vm.push(VMType::Array(arr))
                    }
                    _ => panic!("`take` expect an integer on top of the stack"),
                },
                identifier => panic!("Unknown identifier: {identifier}"),
            }
        }
        None => return,
        token => {
            panic!("Panic with token: {token:?}")
        }
    }
}
