use crate::enums::{Token, VMType};
use crate::vm::Stack;

pub fn parse(tokens: &mut Stack<Token>, vm: &mut Stack<VMType>) {
    match tokens.pop() {
        None => (),
        Some(Token::Identifier(identifier, identifier_line)) => match &*identifier {
            "dup" => vm.dup(),

            "swap" => vm.swap(),

            "clear" => vm.clear(),

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

            "range" | "erange" => {
                if tokens.pop().is_some() {
                    panic!("line {identifier_line}: `{identifier}` requires no more argument.")
                }

                let (b, a) = match (vm.pop(), vm.pop()) {
                    (Some(VMType::Integer(b1)), Some(VMType::Integer(a1))) => (b1, a1),
                    _ => {
                        panic!(
                            "line {identifier_line}: `range` requires two integers on the stack!"
                        )
                    }
                };
                let range: Vec<i64> = match &*identifier {
                    "erange" => (a..=b).collect(),
                    "range" => (a..b).collect(),
                    _ => panic!(),
                };

                vm.push(VMType::Array(
                    range
                        .iter()
                        .map(|n| VMType::Integer(*n))
                        .collect::<Vec<VMType>>(),
                ))
            }

            "print" | "println" => {
                if tokens.pop().is_some() {
                    panic!("line {identifier_line}: `{identifier}` requires no more argument.")
                }

                let print = match &*identifier {
                    "println" => |content| println!("{content}"),
                    "print" => |content| print!("{content}"),
                    _ => panic!(),
                };

                let e = vm.pop().unwrap_or_else(
                    || panic!("line {identifier_line}: `{identifier}` requires string|integer|float on the stack!")
                );

                match &e {
                        VMType::Float(x) => print(x.to_string()),
                        VMType::Integer(n) => print(n.to_string()),
                        VMType::String(c) => print(c.to_string()),
                        _ => panic!("line {identifier_line}: `{identifier}` requires string|integer|float on the stack!"),
                    }

                vm.push(e);
            }

            "pop" => {
                if tokens.pop().is_some() {
                    panic!("line {identifier_line}: `{identifier}` requires no more argument.")
                }

                vm.fast_pop_2()
            }

            "product" => {
                if tokens.pop().is_some() {
                    panic!("line {identifier_line}: `{identifier}` requires no more argument.")
                }

                match vm.pop() {
                    Some(VMType::Array(array)) => {
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
                }
            }

            "sum" => {
                if tokens.pop().is_some() {
                    panic!("line {identifier_line}: `{identifier}` requires no more argument.")
                }

                match vm.pop() {
                    Some(VMType::Array(array)) => {
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
                    _ => panic!(
                        "line {identifier_line}: `sum` requires [integer|float] on the stack!"
                    ),
                }
            }

            "reverse" => {
                if tokens.pop().is_some() {
                    panic!("line {identifier_line}: `{identifier}` requires no more argument.")
                }

                match vm.pop() {
                    Some(VMType::Array(mut array)) => {
                        array.reverse();
                        vm.push(VMType::Array(array))
                    }
                    _ => panic!("`reverse` expect an array on top of the stack"),
                }
            }

            "join" => {
                if tokens.pop().is_some() {
                    panic!("line {identifier_line}: `{identifier}` requires no more argument.")
                }

                let join_string = match vm.pop() {
                    Some(VMType::String(content)) => content,
                    _ => panic!("line {identifier_line}: `join` requires a string and a [string] on the stack!"),
                };

                let to_string = |element: &VMType| {
                    match element {
                        VMType::String(content) => content.clone(),
                        _ => panic!("line {identifier_line}: `join` requires a string and a [string] on the stack!"),
                    }
                };

                let joined = match vm.pop() {
                    Some(VMType::Array(array)) => array
                        .iter()
                        .map(to_string)
                        .collect::<Vec<String>>()
                        .join(&join_string),
                    _ => panic!("line {identifier_line}: `join` requires a string and a [string] on the stack!"),
                };

                vm.push(VMType::String(joined));
            }

            "take" => {
                if tokens.pop().is_some() {
                    panic!("line {identifier_line}: `{identifier}` requires no more argument.")
                }

                match vm.pop() {
                    Some(VMType::Integer(n)) => {
                        let array = (0..n).map(|_| vm.fast_pop_1()).collect();
                        vm.push(VMType::Array(array))
                    }
                    _ => panic!("line {identifier_line}: `take` requires an integer on the stack!"),
                }
            }
            identifier => panic!("line {identifier_line}: unkown identifier: `{identifier}`!"),
        },
        other => panic!("{:?}", other.unwrap()),
    }
}
