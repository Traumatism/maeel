use maeel_common::tokens::Token;
use maeel_common::vmtypes::VMType;

use std::collections::HashMap;
use std::io::Result;
use std::ops::Not;
use std::slice::Iter;

#[macro_export]
macro_rules! next {
    ($tokens:expr, "identifier") => {{
        let next = $tokens.next().unwrap();

        match next {
            Token::Identifier(value) => value.clone(),
            token => panic!("Expected identifier, got {token:?}"),
        }
    }};

    ($tokens:expr, "block") => {{
        let next = $tokens.next().unwrap();
        match next {
            Token::Block(block) => block.to_vec(),
            a => panic!("Expected block, got {:?}", a),
        }
    }};
}

#[macro_export]
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
pub fn process_tokens<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Vec<VMType>,
    vars: &'a mut HashMap<String, VMType>,
    procs: &'a mut HashMap<String, Vec<Token>>,
) -> Result<(
    &'a mut Vec<VMType>,
    &'a mut HashMap<String, VMType>,
    &'a mut HashMap<String, Vec<Token>>,
)> {
    while let Some(token) = tokens.next() {
        match token {
            // TODO: fix this performance disaster
            Token::Equiv => {
                let (mut tokens_f1, mut tokens_f2) = match (data.pop(), data.pop()) {
                    (Some(VMType::Str(f1)), Some(VMType::Str(f2))) => (
                        procs.get(&f1).unwrap().iter(),
                        procs.get(&f2).unwrap().iter(),
                    ),
                    _ => panic!(),
                };

                let mut ok = true;

                for mut tmp_data in [
                    vec![VMType::Bool(false), VMType::Bool(false)],
                    vec![VMType::Bool(true), VMType::Bool(false)],
                    vec![VMType::Bool(false), VMType::Bool(true)],
                    vec![VMType::Bool(true), VMType::Bool(true)],
                ] {
                    if ok == false {
                        break;
                    }

                    let mut tmp_data_clone = tmp_data.clone();

                    let mut procs_clone = HashMap::new();
                    let mut vars_clone = HashMap::new();

                    let (t1, _, _) = process_tokens(
                        &mut tokens_f1,
                        &mut tmp_data_clone,
                        &mut vars_clone,
                        &mut procs_clone,
                    )?;

                    let mut procs_clone = HashMap::new();
                    let mut vars_clone = HashMap::new();

                    let (t2, _, _) = process_tokens(
                        &mut tokens_f2,
                        &mut tmp_data,
                        &mut vars_clone,
                        &mut procs_clone,
                    )?;

                    if t1 != t2 {
                        ok = false
                    }
                }

                data.push(VMType::Bool(ok))
            }

            // Push a new procedure
            Token::ProcStart => {
                procs.insert(next!(tokens, "identifier"), next!(tokens, "block"));
            }

            // While loop
            Token::While => {
                let tokens = next!(tokens, "block");

                while let VMType::Bool(true) = data.pop().unwrap() {
                    process_tokens(&mut tokens.iter(), data, vars, procs)?;
                }
            }

            // For loop
            Token::For => {
                let tokens = next!(tokens, "block");

                if let Some(VMType::Array(array)) = data.pop() {
                    for element in array {
                        data.push(element);
                        process_tokens(&mut tokens.iter(), data, vars, procs)?;
                    }
                }
            }

            // Alias a name to a VM type
            Token::Let => {
                vars.insert(next!(tokens, "identifier"), data.pop().unwrap());
            }

            // If condition
            Token::If => {
                let tokens = next!(tokens, "block");

                if let Some(VMType::Bool(true)) = data.pop() {
                    process_tokens(&mut tokens.iter(), data, vars, procs)?;
                }
            }

            // Parse integer intervals
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

            // Parse an array of elements
            Token::ArrayStart => {
                let mut array = Vec::new();

                loop {
                    let next_token = tokens.next().unwrap().clone();

                    match next_token {
                        Token::ArrayEnd => break,

                        Token::ArrayStart => panic!(),

                        Token::Str(value) => array.push(VMType::Str(value)),

                        Token::Integer(value) => array.push(VMType::Integer(value)),

                        Token::Float(value) => array.push(VMType::Float(value)),

                        Token::Bool(value) => array.push(VMType::Bool(value)),

                        Token::Block(expr) => {
                            let tokens = next!(tokens, "block");

                            let gen_expr = {
                                let mut iterator = tokens.iter();

                                let (tmp_data, _, _) =
                                    process_tokens(&mut iterator, data, vars, procs)?;

                                tmp_data.pop().unwrap()
                            };

                            match gen_expr {
                                VMType::Array(a) => {
                                    for e in a {
                                        let mut tmp_data = vec![e];
                                        let mut iterator = expr.iter();

                                        let (tmp_data, _, _) = process_tokens(
                                            &mut iterator,
                                            &mut tmp_data,
                                            vars,
                                            procs,
                                        )?;

                                        array.push(tmp_data.pop().unwrap());
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

            // These tokens shouldn't be alone here
            Token::BlockStart | Token::BlockEnd | Token::ArrayEnd | Token::IEnd => panic!(),

            // Handle new code blocks
            Token::Block(tokens) => {
                process_tokens(&mut tokens.iter(), data, vars, procs)?;
            }

            // Push a string
            Token::Str(content) => data.push(VMType::Str(content.clone())),

            // Push a boolean
            Token::Bool(content) => data.push(VMType::Bool(*content)),

            // Push a float
            Token::Float(content) => data.push(VMType::Float(*content)),

            // Push an integer
            Token::Integer(content) => data.push(VMType::Integer(*content)),

            // Pop from the stack
            Token::Pop => {
                data.pop();
            }

            // Rotate the three top elements
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

            // Swap the two stack top elements
            Token::Swap => {
                let (top, over) = (data.pop().unwrap(), data.pop().unwrap());

                data.push(top);
                data.push(over)
            }

            // Push the element on the stack top on the stack top
            Token::Dup => data.push(data.last().cloned().unwrap()),

            // Push the element over the stack top on the stack top
            Token::Over => data.push(data[data.len() - 2].to_owned()),

            // Clear the stack
            Token::Clear => data.clear(),

            // (a > b)?
            Token::Gt => binary_op!(data, >, VMType::Bool),

            // (a < b)?
            Token::Lt => binary_op!(data, <, VMType::Bool),

            // (a = b)?
            Token::Eq => binary_op!(data, ==, VMType::Bool),

            Token::Sub => binary_op!(data, -),

            Token::Add => binary_op!(data, +),

            Token::Mul => binary_op!(data, *),

            Token::Div => binary_op!(data, /),

            Token::Mod => binary_op!(data, %),

            // Invert the stack head
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
                    "argv" => data.push(VMType::Array(
                        std::env::args().map(|arg| VMType::Str(arg)).collect(),
                    )),

                    "print" => {
                        print!("{}", data.last().unwrap());
                    }

                    identifier => match vars.get(identifier) {
                        Some(value) => data.push(value.clone()),

                        None => {
                            let tokens = procs.get(identifier).unwrap().clone();
                            process_tokens(&mut tokens.iter(), data, vars, procs)?;
                        }
                    },
                };
            }
        };
    }

    Ok((data, vars, procs))
}
