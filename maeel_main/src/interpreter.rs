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
        let (a, b) = ($data.pop().unwrap(), $data.pop().unwrap());
        $data.push(b $operator a)
    }};
    ($data:expr, $operator:tt, $vmtype:expr) => {{
        let (a, b) = ($data.pop().unwrap(), $data.pop().unwrap());
        $data.push($vmtype(b $operator a))
    }};
}

pub fn process_tokens<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Vec<VMType>,
    globals: &'a mut HashMap<String, VMType>,
    procs: &'a mut HashMap<String, Vec<Token>>,
) -> Result<&'a mut Vec<VMType>> {
    // Specific to current code block (won't be given to the next/previous code block)
    let mut locals = HashMap::new();

    while let Some(token) = tokens.next() {
        match token {
            Token::BlockStart | Token::BlockEnd | Token::ArrayEnd | Token::IEnd => panic!(),

            // Parse a new procedure
            Token::ProcStart => {
                // Procedure name
                let name = next!(tokens, "identifier");

                // We expect a list of names
                let Some(Token::IStart) = tokens.next() else {
                    panic!()
                };

                // Procedure block
                let mut block = Vec::default();

                loop {
                    let token = tokens.next();

                    match token {
                        Some(Token::Identifier(_)) => {
                            // Append variable definition to procedure block
                            block.append(&mut vec![Token::Let, token.unwrap().clone()])
                        }

                        // List end
                        Some(Token::IEnd) => break,

                        // We want only identifiers
                        _ => panic!(),
                    }
                }

                // Finally append real procedure tokens
                block.append(&mut next!(tokens, "block"));

                procs.insert(name, block);
            }

            Token::While => {
                // Code block to execute while P(x) is true
                let tokens = next!(tokens, "block");

                // This is why we need to push P(x) at the end of the code block
                while let VMType::Bool(true) = data.pop().unwrap() {
                    process_tokens(&mut tokens.iter(), data, globals, procs)?;
                }
            }

            Token::For => {
                // Code block to execute for each value of L
                let tokens = next!(tokens, "block");

                if let Some(VMType::Array(array)) = data.pop() {
                    for element in array {
                        data.push(element);
                        process_tokens(&mut tokens.iter(), data, globals, procs)?;
                    }
                } else {
                    panic!() // An array must be on the stack's top
                }
            }

            Token::Let => {
                // Variable name
                let name = next!(tokens, "identifier");

                // Variable privateness/publicness depends of the name
                match name.chars().collect::<Vec<char>>().get(0) {
                    Some('_') => locals.insert(name, data.pop().unwrap()),
                    Some(_) => globals.insert(name, data.pop().unwrap()),
                    None => panic!(),
                };
            }

            Token::If => {
                // Code block to execute if, and only if P(x) is true
                let tokens = next!(tokens, "block");

                if let Some(VMType::Bool(true)) = data.pop() {
                    process_tokens(&mut tokens.iter(), data, globals, procs)?;
                }
            }

            Token::IStart => {
                let (Some(Token::Integer(start)), Some(Token::Integer(end))) = (tokens.next(), tokens.next()) else {
                    panic!()
                };

                let Some(Token::IEnd) = tokens.next() else {
                    panic!()
                };

                data.push(VMType::Array((*start..*end).map(VMType::Integer).collect()));
            }

            Token::ArrayStart => {
                let mut array = Vec::default();

                loop {
                    match tokens.next().unwrap().clone() {
                        Token::ArrayEnd => break,

                        Token::ArrayStart => panic!(),

                        Token::Str(value) => array.push(VMType::Str(value)),

                        Token::Integer(value) => array.push(VMType::Integer(value)),

                        Token::Float(value) => array.push(VMType::Float(value)),

                        Token::Bool(value) => array.push(VMType::Bool(value)),

                        Token::Identifier(identifier) => match globals.get(&identifier) {
                            Some(value) => array.push(value.clone()),

                            None => panic!(),
                        },

                        Token::Block(expr) => {
                            let generator = process_tokens(
                                &mut next!(tokens, "block").iter(),
                                data,
                                globals,
                                procs,
                            )?
                            .pop();

                            let Some(VMType::Array(target)) = generator else {
                                panic!()
                            };

                            for element in target {
                                let mut tmp_data = vec![element];

                                let output = process_tokens(
                                    &mut expr.iter(),
                                    &mut tmp_data,
                                    globals,
                                    procs,
                                )?
                                .pop();

                                array.push(output.unwrap());
                            }
                        }

                        _ => panic!(),
                    }
                }

                data.push(VMType::Array(array))
            }

            Token::Block(tokens) => {
                process_tokens(&mut tokens.iter(), data, globals, procs)?;
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

                    identifier => match globals.get(identifier) {
                        Some(value) => data.push(value.clone()),
                        None => match locals.get(identifier) {
                            Some(value) => data.push(value.clone()),
                            None => {
                                process_tokens(
                                    &mut procs.get(identifier).unwrap().clone().iter(),
                                    data,
                                    globals,
                                    procs,
                                )?;
                            }
                        },
                    },
                };
            }
        };
    }

    Ok(data)
}
