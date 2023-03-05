use maeel_common::tokens::{Token, TokenData};
use maeel_common::vmtypes::VMType;

use std::collections::HashMap;
use std::slice::Iter;

mod syscalls;
use syscalls::handle_syscall;

macro_rules! next {
    ($tokens:expr, "identifier") => {{
        let next = $tokens.next().unwrap();
        match &next.token {
            Token::Identifier(value) => value.clone(),
            token => panic!("Expected identifier, got {:?}", token),
        }
    }};
    ($tokens:expr, "block") => {{
        let next = $tokens.next().unwrap();
        match &next.token {
            Token::Block(block) => block.to_vec(),
            token => panic!("Expected block, got {:?}", token),
        }
    }};
}

macro_rules! binary_op {
    ($data:expr, $operator:tt) => {{
        let (a, b) = ($data.pop().unwrap(), $data.pop().unwrap());
        $data.push(b $operator a);
    }};
    ($data:expr, $operator:tt, $vmtype:expr) => {{
        let (a, b) = ($data.pop().unwrap(), $data.pop().unwrap());
        $data.push($vmtype(b $operator a))
    }};
}

/// Handle a bunch of tokens
pub fn process_tokens(
    tokens: &mut Iter<TokenData>,
    data: &mut Vec<VMType>,
    vars: &mut HashMap<String, VMType>,
    procs: &mut HashMap<String, Vec<TokenData>>,
) {
    while let Some(token_data) = tokens.next() {
        let token = token_data.token.clone();
        let _line = token_data.line;

        match token.clone() {
            Token::BlockStart | Token::BlockEnd | Token::Include => panic!(),

            Token::Block(tokens) => process_tokens(&mut tokens.iter(), data, vars, procs),
            Token::Str(content) => data.push(VMType::Str(content)),
            Token::Bool(content) => data.push(VMType::Bool(content)),
            Token::Float(content) => data.push(VMType::Float(content)),
            Token::Integer(content) => data.push(VMType::Integer(content)),
            Token::Rot => {
                let (top, over_0, over_1) = (
                    data.pop().unwrap(),
                    data.pop().unwrap(),
                    data.pop().unwrap(),
                );

                data.push(over_0);
                data.push(top);
                data.push(over_1);
            }
            Token::Swap => {
                let (top, over) = (data.pop().unwrap(), data.pop().unwrap());

                data.push(top);
                data.push(over)
            }
            Token::Dup => data.push(data.last().cloned().unwrap()),
            Token::Over => data.push(data[data.len() - 2].to_owned()),
            Token::Clear => data.clear(),
            Token::Gt => binary_op!(data, >, VMType::Bool),
            Token::Lt => binary_op!(data, <, VMType::Bool),
            Token::Eq => binary_op!(data, ==, VMType::Bool),
            Token::Sub => binary_op!(data, -),
            Token::Add => binary_op!(data, +),
            Token::Mul => binary_op!(data, *),
            Token::Div => binary_op!(data, /),
            Token::Mod => binary_op!(data, %),
            Token::Pop => {
                data.pop().unwrap();
            }
            Token::ProcStart => {
                procs.insert(next!(tokens, "identifier"), next!(tokens, "block"));
            }
            Token::Not => {
                let p = data.pop().unwrap();
                data.push(!p)
            }
            Token::Get => match (data.pop(), data.pop()) {
                (Some(VMType::Integer(n)), Some(VMType::Array(array))) => {
                    data.push(array.get(n as usize).unwrap().clone());
                }
                _ => panic!(),
            },
            Token::Take => match data.pop() {
                Some(VMType::Integer(n)) => {
                    let array = (0..n).map(|_| data.pop().unwrap()).collect();
                    data.push(VMType::Array(array));
                }
                _ => panic!(),
            },
            Token::Identifier(identifier) => {
                match identifier.as_str() {
                    "ptr" => {
                        let new_element = match data.pop().unwrap() {
                            VMType::Str(content) => VMType::StrPointer(content),
                            VMType::Integer(content) => VMType::IntPointer(Box::new(content)),
                            _ => panic!(),
                        };
                        data.push(new_element);
                    }
                    "argv" => data.push(VMType::Array(
                        std::env::args().map(|arg| VMType::Str(arg)).collect(),
                    )),
                    "syscall" => {
                        let syscall_nr = match vars.get("syscall_number") {
                            Some(VMType::Integer(number)) => *number as usize,
                            _ => panic!(),
                        };

                        let mut syscalls_args = vars
                            .keys()
                            .filter(|name| name.starts_with("syscall_"))
                            .filter_map(|name| {
                                let number_e = name[8..].parse::<u8>().ok()?;
                                Some((number_e, vars[name].clone()))
                            })
                            .collect::<Vec<_>>();

                        syscalls_args.sort_by_key(|(n, _)| *n);

                        handle_syscall(
                            syscall_nr,
                            &syscalls_args
                                .into_iter()
                                .map(|(_, arg)| arg)
                                .collect::<Vec<VMType>>(),
                        );
                    }

                    "print" => {
                        let element = data.pop().unwrap();
                        let message = element.to_string();
                        data.push(element);

                        #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
                        handle_syscall(
                            4,
                            &[
                                VMType::Integer(1),
                                VMType::StrPointer(message.clone()),
                                VMType::Integer(message.len().try_into().unwrap()),
                            ],
                        );

                        #[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
                        print!("{}", message);
                    }

                    identifier => match vars.get(identifier) {
                        Some(value) => {
                            data.push(value.clone());
                        }
                        None => {
                            let tokens = procs.get(identifier).unwrap().clone();
                            process_tokens(&mut tokens.iter(), data, vars, procs);
                        }
                    },
                };
            }

            Token::Let => {
                let name = next!(tokens, "identifier");
                let next = tokens.next().unwrap();
                let value = match &next.token {
                    Token::Str(content) => VMType::Str(content.clone()),
                    Token::Bool(p) => VMType::Bool(*p),
                    Token::Float(x) => VMType::Float(*x),
                    Token::Integer(n) => VMType::Integer(*n),
                    Token::Pop => data.pop().unwrap(),
                    Token::Over => data[data.len() - 2].clone(),
                    Token::Dup => data.last().cloned().unwrap(),
                    _ => panic!(),
                };

                vars.insert(name, value);
            }
            Token::While => {
                let tokens = next!(tokens, "block");
                while let VMType::Bool(true) = data.pop().unwrap() {
                    process_tokens(&mut tokens.iter(), data, vars, procs);
                }
            }
            Token::For => {
                let tokens = next!(tokens, "block");
                match data.pop() {
                    Some(VMType::Array(array)) => array.iter().for_each(|element| {
                        data.push(element.clone());
                        process_tokens(&mut tokens.iter(), data, vars, procs);
                    }),
                    _ => panic!(),
                }
            }
            Token::If => {
                if let Some(VMType::Bool(true)) = data.pop() {
                    process_tokens(&mut next!(tokens, "block").iter(), data, vars, procs)
                } else {
                    tokens.next().unwrap();
                }
            }
        }
    }
}
