use crate::enums::{Token, VMType};
use crate::vm::Stack;

pub fn parse(tokens: &mut Stack<Token>, vm: &mut Stack<VMType>) {
    match tokens.pop() {
        None => (),
        Some(Token::Identifier(identifier, identifier_line)) => match &*identifier {
            "push" => {
                while let Some(next) = tokens.pop() {
                    vm.push(match next {
                        Token::Float(value, _) => VMType::Float(value),
                        Token::String(value, _) => VMType::String(value),
                        Token::Integer(value, _) => VMType::Integer(value),
                        _ => panic!("line {identifier_line}: `push` only accept floats, strings and integers!"),
                    })
                }
            }

            "range" => {
                let (b, a) = match (vm.fast_pop(), vm.fast_pop()) {
                    (VMType::Integer(b1), VMType::Integer(a1)) => (b1, a1),
                    _ => {
                        panic!(
                            "line {identifier_line}: `range` requires two integers on the stack!"
                        )
                    }
                };

                vm.push(VMType::Array(
                    (a..b).map(VMType::Integer).collect::<Vec<VMType>>(),
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
                        VMType::String(c) => f(c.to_string()),
                        _ => panic!("line {identifier_line}: `{identifier}` requires string|integer|float on the stack!"),
                    }

                vm.push(e);
            }

            "pop" => {
                vm.fast_pop();
            }

            "product" => match vm.fast_pop() {
                VMType::Array(array) => {
                    let product = array
                            .iter()
                            .map(|element| match element {
                                VMType::Integer(n) => *n as f64,
                                VMType::Float(x) => *x,
                                _ => panic!("line {identifier_line}: `product` requires [integer|float] on the stack!"),
                            })
                            .product::<f64>();

                    vm.push(VMType::Float(product));
                }
                _ => panic!(
                    "line {identifier_line}: `product` requires [integer|float] on the stack!"
                ),
            },

            "sum" => match vm.fast_pop() {
                VMType::Array(array) => {
                    let sum = array
                            .iter()
                            .map(|element| match element {
                                VMType::Integer(n) => *n as f64,
                                VMType::Float(x) => *x,
                                _ => panic!("line {identifier_line}: `sum` requires [integer|float] on the stack!"),
                            })
                            .sum::<f64>();

                    vm.push(VMType::Float(sum))
                }
                _ => panic!("line {identifier_line}: `sum` requires [integer|float] on the stack!"),
            },

            "reverse" => match vm.fast_pop() {
                VMType::Array(mut array) => {
                    array.reverse();
                    vm.push(VMType::Array(array))
                }
                _ => panic!("`reverse` expect an array on top of the stack"),
            },

            "join" => {
                let join_string = match vm.fast_pop() {
                    VMType::String(content) => content,
                    _ => panic!("line {identifier_line}: `join` requires a string and a [string] on the stack!"),
                };

                let joined = match vm.fast_pop() {
                    VMType::Array(array) => array
                        .iter()
                        .map(|element| match element {
                            VMType::String(content) => content.clone(),
                            _ => panic!("line {identifier_line}: `join` requires a string and a [string] on the stack!"),
                        })
                        .collect::<Vec<String>>()
                        .join(&join_string),
                    _ => panic!("line {identifier_line}: `join` requires a string and a [string] on the stack!"),
                };

                vm.push(VMType::String(joined));
            }

            "take" => match vm.fast_pop() {
                VMType::Integer(n) => {
                    let array = (0..n).map(|_| vm.fast_pop()).collect();
                    vm.push(VMType::Array(array))
                }
                _ => panic!("line {identifier_line}: `take` requires an integer on the stack!"),
            },
            identifier => panic!("line {identifier_line}: unkown identifier: `{identifier}`!"),
        },
        _ => panic!(),
    }
}
