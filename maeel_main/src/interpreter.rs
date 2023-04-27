use maeel_common::tokens::Token;
use maeel_common::vmtypes::VMType;

use std::collections::HashMap;
use std::io::Result;
use std::ops::Not;
use std::slice::Iter;

macro_rules! next {
    ($tokens:expr, "identifier") => {{
        match $tokens.next().unwrap() {
            Token::Identifier(value) => value.clone(),
            token => panic!("Expected identifier, got {token:?}"),
        }
    }};
    ($tokens:expr, "block") => {{
        match $tokens.next().unwrap() {
            Token::Block(block) => block.to_vec(),
            token => panic!("Expected block, got {token:?}"),
        }
    }};
}

macro_rules! binary_op {
    ($data:expr, $operator:tt) => {{
        let (a, b) = ($data.pop().unwrap(), $data.pop().unwrap()); $data.push(b $operator a);
    }};
    ($data:expr, $operator:tt, $vmtype:expr) => {{
        let (a, b) = ($data.pop().unwrap(), $data.pop().unwrap()); $data.push($vmtype(b $operator a))
    }};
}

/// Handle a bunch of tokens
pub fn process_tokens<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Vec<VMType>,
    vars: &'a mut HashMap<String, VMType>,
    procs: &'a mut HashMap<String, Vec<Token>>,
) -> Result<&'a mut Vec<VMType>> {
    while let Some(token) = tokens.next() {
        match token {
            Token::BlockStart | Token::BlockEnd | Token::ArrayEnd | Token::IEnd => panic!(),

            Token::ProcStart => {
                let name = next!(tokens, "identifier");

                let Some(Token::IStart) = tokens.next() else {
                    panic!()
                };

                let mut block = Vec::default();

                loop {
                    let token = tokens.next();
                    match token {
                        Some(Token::Identifier(_)) => {
                            block.append(&mut vec![Token::Let, token.unwrap().clone()])
                        }
                        Some(Token::IEnd) => break,
                        _ => panic!(),
                    }
                }

                block.append(&mut next!(tokens, "block"));

                procs.insert(name, block);
            }

            Token::While => {
                let tokens = next!(tokens, "block");
                while let VMType::Bool(true) = data.pop().unwrap() {
                    process_tokens(&mut tokens.iter(), data, vars, procs)?;
                }
            }

            Token::For => {
                let tokens = next!(tokens, "block");
                if let Some(VMType::Array(array)) = data.pop() {
                    for element in array {
                        data.push(element);
                        process_tokens(&mut tokens.iter(), data, vars, procs)?;
                    }
                }
            }

            Token::Let => {
                vars.insert(next!(tokens, "identifier"), data.pop().unwrap());
            }

            Token::If => {
                let tokens = next!(tokens, "block");
                if let Some(VMType::Bool(true)) = data.pop() {
                    process_tokens(&mut tokens.iter(), data, vars, procs)?;
                }
            }

            Token::IStart => {
                let (start, end) = match (tokens.next(), tokens.next()) {
                    (Some(Token::Integer(m)), Some(Token::Integer(n))) => (m, n),

                    _ => panic!(),
                };

                let Some(Token::IEnd) = tokens.next() else {
                    panic!()
                };

                data.push(VMType::Array(
                    (*start..*end).map(|i| VMType::Integer(i)).collect(),
                ));
            }

            Token::ArrayStart => {
                let mut array = Vec::new();

                loop {
                    match tokens.next().unwrap().clone() {
                        Token::ArrayEnd => break,
                        Token::ArrayStart => panic!(),
                        Token::Str(value) => array.push(VMType::Str(value)),
                        Token::Integer(value) => array.push(VMType::Integer(value)),
                        Token::Float(value) => array.push(VMType::Float(value)),
                        Token::Bool(value) => array.push(VMType::Bool(value)),

                        Token::Identifier(identifier) => match vars.get(&identifier) {
                            Some(value) => array.push(value.clone()),
                            None => panic!(),
                        },

                        Token::Block(expr) => {
                            let gen_expr = process_tokens(
                                &mut next!(tokens, "block").iter(),
                                data,
                                vars,
                                procs,
                            )?
                            .pop();

                            match gen_expr {
                                Some(VMType::Array(a)) => {
                                    for e in a {
                                        let mut tmp_data = vec![e];

                                        let output = process_tokens(
                                            &mut expr.iter(),
                                            &mut tmp_data,
                                            vars,
                                            procs,
                                        )?
                                        .pop();

                                        array.push(output.unwrap());
                                    }
                                }
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                data.push(VMType::Array(array))
            }

            Token::Block(tokens) => {
                process_tokens(&mut tokens.iter(), data, vars, procs)?;
            }

            Token::Str(content) => data.push(VMType::Str(content.clone())),
            Token::Bool(content) => data.push(VMType::Bool(*content)),
            Token::Float(content) => data.push(VMType::Float(*content)),
            Token::Integer(content) => data.push(VMType::Integer(*content)),

            Token::Pop => {
                data.pop();
            }

            Token::Rot => {
                let (top, over_0, over_1) = (
                    data.pop().unwrap(),
                    data.pop().unwrap(),
                    data.pop().unwrap(),
                );

                data.push(over_0);
                data.push(top);
                data.push(over_1)
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

            Token::Not => {
                let p = data.pop().unwrap();
                data.push(p.not())
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
                    "print" => {
                        print!("{}", data.last().unwrap());
                    }
                    identifier => match vars.get(identifier) {
                        Some(value) => data.push(value.clone()),
                        None => {
                            process_tokens(
                                &mut procs.get(identifier).unwrap().clone().iter(),
                                data,
                                vars,
                                procs,
                            )?;
                        }
                    },
                };
            }
        };
    }

    Ok(data)
}
