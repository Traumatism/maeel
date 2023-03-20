use maeel_common::tokens::{Token, TokenData};
use maeel_common::vmtypes::VMType;

use std::collections::HashMap;
use std::ops::Not;
use std::process::exit;
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

macro_rules! emit_error {
    ($file_name:expr, $line:expr, $pos:expr, $message:expr) => {{
        println!("{}:{}:{} {}", $file_name, $line, $pos, $message);
        exit(1);
    }};
}

/// Handle a bunch of tokens
pub fn process_tokens<'a>(
    file_name: &'a str,
    tokens: &'a mut Iter<TokenData>,
    data: &'a mut Vec<VMType>,
    vars: &'a mut HashMap<String, VMType>,
    procs: &'a mut HashMap<String, Vec<TokenData>>,
) -> (
    &'a mut Vec<VMType>,
    &'a mut HashMap<String, VMType>,
    &'a mut HashMap<String, Vec<TokenData>>,
) {
    while let Some(token_data) = tokens.next() {
        let token = token_data.token.clone();
        let line = token_data.line;
        let pos = token_data.pos;

        match token.clone() {
            Token::BlockStart | Token::BlockEnd => panic!(),

            Token::Include => {}

            Token::Block(tokens) => {
                process_tokens(file_name, &mut tokens.iter(), data, vars, procs);
            }

            Token::Str(content) => data.push(VMType::Str(content)),

            Token::Bool(content) => data.push(VMType::Bool(content)),

            Token::Float(content) => data.push(VMType::Float(content)),

            Token::Integer(content) => data.push(VMType::Integer(content)),

            Token::Rot => {
                if data.len() < 3 {
                    emit_error!(
                        file_name,
                        line,
                        pos,
                        "`rot` requires 3 values on the stack!"
                    )
                }

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
                if data.len() < 2 {
                    emit_error!(
                        file_name,
                        line,
                        pos,
                        "`swap` requires 2 values on the stack!"
                    )
                }

                let (top, over) = (data.pop().unwrap(), data.pop().unwrap());

                data.push(top);
                data.push(over)
            }

            Token::Dup => {
                if data.len() < 1 {
                    emit_error!(file_name, line, pos, "`dup` requires 1 value on the stack!")
                }

                data.push(data.last().cloned().unwrap())
            }

            Token::Over => {
                if data.len() < 2 {
                    emit_error!(
                        file_name,
                        line,
                        pos,
                        "`over` requires 2 values on the stack!"
                    )
                }

                data.push(data[data.len() - 2].to_owned())
            }

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
                if data.len() < 1 {
                    emit_error!(file_name, line, pos, "`pop` requires 1 value on the stack!")
                }

                data.pop();
            }

            Token::ProcStart => {
                procs.insert(next!(tokens, "identifier"), next!(tokens, "block"));
            }

            Token::Not => {
                if data.len() < 1 {
                    emit_error!(file_name, line, pos, "`not` requires 1 value on the stack!")
                }

                let p = data.pop().unwrap();
                data.push(p.not())
            }

            Token::Get => match (data.pop(), data.pop()) {
                (Some(VMType::Integer(n)), Some(VMType::Array(array))) => {
                    data.push(array.get(n as usize).unwrap().clone());
                }
                _ => emit_error!(
                    file_name,
                    line,
                    pos,
                    "`get` requires 1 integer and 1 array on the stack!"
                ),
            },

            Token::Take => match data.pop() {
                Some(VMType::Integer(n)) => {
                    let array = (0..n)
                        .map(|_| {
                            data.pop().unwrap_or_else(|| {
                                emit_error!(
                                    file_name,
                                    line,
                                    pos,
                                    format!("`{} take` requires {} elements on the stack!", n, n)
                                )
                            })
                        })
                        .collect();
                    data.push(VMType::Array(array));
                }
                _ => emit_error!(
                    file_name,
                    line,
                    pos,
                    "`take` requires 1 integer `n` and `n` elements on the stack!"
                ),
            },
            Token::Identifier(identifier) => {
                match identifier.as_str() {
                    "ptr" => {
                        if data.len() < 1 {
                            emit_error!(
                                file_name,
                                line,
                                pos,
                                "`ptr` requires 1 value on the stack!"
                            )
                        }

                        let new_element = match data.pop().unwrap() {
                            VMType::Str(content) => VMType::StrPointer(content),
                            VMType::Integer(content) => VMType::IntPointer(Box::new(content)),
                            _ => emit_error!(
                                file_name,
                                line,
                                pos,
                                "`ptr` value must be a string/an integer!"
                            ),
                        };
                        data.push(new_element);
                    }
                    "argv" => data.push(VMType::Array(
                        std::env::args().map(|arg| VMType::Str(arg)).collect(),
                    )),
                    "syscall" => {
                        let syscall_nr = match vars.get("syscall_number") {
                            Some(VMType::Integer(number)) => *number as usize,
                            _ => emit_error!(
                                file_name,
                                line,
                                pos,
                                "`syscall_number` must be defined!"
                            ),
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
                        if data.len() < 1 {
                            emit_error!(
                                file_name,
                                line,
                                pos,
                                "`print` requires 1 value on the stack!"
                            )
                        }

                        let message = data.last().unwrap().to_string();

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
                        Some(value) => data.push(value.clone()),
                        None => {
                            let tokens = procs.get(identifier).unwrap().clone();
                            process_tokens(file_name, &mut tokens.iter(), data, vars, procs);
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
                    _ => {
                        emit_error!(file_name, line, pos, "`let <name> <value>` accepts (as value) strings, booleans, floats, integers, pop, over, dup")
                    }
                };

                vars.insert(name, value);
            }

            Token::While => {
                let tokens = next!(tokens, "block");
                while let VMType::Bool(true) = data.pop().unwrap() {
                    process_tokens(file_name, &mut tokens.iter(), data, vars, procs);
                }
            }

            Token::For => {
                let tokens = next!(tokens, "block");
                match data.pop() {
                    Some(VMType::Array(array)) => array.iter().for_each(|element| {
                        data.push(element.clone());
                        process_tokens(file_name, &mut tokens.iter(), data, vars, procs);
                    }),
                    _ => emit_error!(
                        file_name,
                        line,
                        pos,
                        "`for` requires an array on the stack!"
                    ),
                }
            }

            Token::If => {
                if let Some(VMType::Bool(true)) = data.pop() {
                    process_tokens(
                        file_name,
                        &mut next!(tokens, "block").iter(),
                        data,
                        vars,
                        procs,
                    );
                } else {
                    tokens.next().unwrap();
                }
            }
        }
    }

    (data, vars, procs)
}
