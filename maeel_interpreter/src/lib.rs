mod parsing;

use maeel_common::{
    maeel_std::{
        MAEEL_STD_CONTENT,
        MAEEL_STD_MATHS_CONTENT,
    },
    tokens::Token,
    vmtypes::VMType,
};

use std::collections::HashMap;
use std::io::Result;
use std::ops::Not;
use std::iter::zip;
use std::fs::read_to_string;
use std::slice::Iter;

type Stack = Vec<VMType>;
type VariablesRegistery = HashMap<String, VMType>;
type ProceduresRegistery = HashMap<String, Vec<Token>>;
type StructuresRegistery = HashMap<String, Vec<String>>;

#[macro_export]
macro_rules! next {
    ($tokens:expr,"identifier") => {{
        match $tokens.next().unwrap() {
            Token::Identifier(value) => value.clone(),
            token => panic!("{token:?}"),
        }
    }};

    ($tokens:expr,"block") => {{
        match $tokens.next().unwrap() {
            Token::Block(block) => block.to_vec(),
            token => panic!("{token:?}"),
        }
    }};
}

/// Perform a binary operation between two `VMType`s
macro_rules! perform_binary_op {
    ($data:expr, $operator:tt) => {{
        let (a, b) = ($data.pop().unwrap(), $data.pop().unwrap());
        $data.push(b $operator a)
    }};

    ($data:expr, $operator:tt, $vmtype:expr) => {{
        let (a, b) = ($data.pop().unwrap(), $data.pop().unwrap());
        $data.push($vmtype(b $operator a))
    }};
}

#[macro_export]
macro_rules! parse_identifiers_list {
    ($tokens:expr) => {{
        let mut identifiers = Vec::default();

        loop {
            let token = $tokens.next();

            match token {
                Some(Token::Identifier(field)) => {
                    identifiers.push(field.clone())
                }

                // List end
                Some(Token::IEnd) => break,

                // We want only identifiers
                _ => panic!(),
            }
        }

        identifiers
    }};
}

/// The `process_tokens` function processes a sequence of tokens and executes the corresponding
/// operations on a stack of values, global variables, and procedures.
pub fn process_tokens<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Stack,
    globals: &'a mut VariablesRegistery,
    procs: &'a mut ProceduresRegistery,
    structs: &'a mut StructuresRegistery,
) -> Result<&'a mut Stack>
{
    // Specific to current code block (won't be given to the next/previous code block)
    let mut locals = HashMap::new();

    while let Some(token) = tokens.next() {
        match token {
            Token::ProcStart => {
                parsing::procedures::parse_proc(tokens, procs);
            }

            Token::While => {
                parsing::loops::parse_while(
                    tokens, data, globals, procs, structs,
                );
            }

            Token::For => {
                parsing::loops::parse_for(
                    tokens, data, globals, procs, structs,
                );
            }

            Token::If => {
                parsing::conditions::parse_if(
                    tokens, data, globals, procs, structs,
                );
            }

            Token::IStart => {
                parsing::iterables::parse_interval(tokens, data);
            }

            Token::ArrayStart => {
                parsing::iterables::parse_array(
                    tokens, data, globals, procs, structs,
                );
            }

            Token::At => {
                parsing::structures::parse_struct(
                    tokens, data, structs,
                );
            }

            Token::Let => {
                parsing::assignments::parse_assignment(
                    tokens,
                    data,
                    &mut locals,
                    globals,
                );
            }

            Token::Gt => {
                perform_binary_op!(data, >, VMType::Bool)
            }

            Token::Lt => {
                perform_binary_op!(data, <, VMType::Bool)
            }

            Token::Eq => {
                perform_binary_op!(data, ==, VMType::Bool)
            }

            Token::Add => {
                perform_binary_op!(data, +)
            }

            Token::Mul => {
                perform_binary_op!(data, *)
            }

            Token::Div => {
                perform_binary_op!(data, /)
            }

            Token::Mod => {
                perform_binary_op!(data, %)
            }

            Token::Str(content) => {
                data.push(VMType::Str(content.clone()));
            }

            Token::Bool(content) => {
                data.push(VMType::Bool(*content));
            }

            Token::Float(content) => {
                data.push(VMType::Float(*content));
            }

            Token::Integer(content) => {
                data.push(VMType::Integer(*content));
            }

            // Execute next block
            Token::Block(tokens) => {
                process_tokens(
                    &mut tokens.iter(),
                    data,
                    globals,
                    procs,
                    structs,
                )?;
            }

            // Pop value from the stack
            Token::Pop => {
                data.pop();
            }

            Token::Rot => {
                let (a, b, c) = (
                    data.pop().unwrap(),
                    data.pop().unwrap(),
                    data.pop().unwrap(),
                );

                data.push(b);
                data.push(a);
                data.push(c);
            }

            Token::Swap => {
                let (top, over) =
                    (data.pop().unwrap(), data.pop().unwrap());

                data.push(top);
                data.push(over)
            }

            Token::Clear => data.clear(),

            Token::Dup => data.push(data.last().cloned().unwrap()),

            Token::Over => data.push(data[data.len() - 2].to_owned()),

            Token::Not => {
                let p = data.pop().unwrap();

                data.push(p.not())
            }

            Token::Get => {
                handle_get(data);
            }
            Token::Take => {
                handle_take(data);
            }

            Token::Identifier(identifier) => {
                match identifier.as_str() {
                    "print" => print!("{}", data.last().unwrap()),

                    "include" => {
                        let Some(Token::Str(target)) = tokens.next() else {
                            panic!()
                        };

                        let content = match target.as_str() {
                            "std" => MAEEL_STD_CONTENT.to_string(),

                            "maths" => {
                                MAEEL_STD_MATHS_CONTENT.to_string()
                            }

                            _ => {
                                let file_name = format!(
                                    "{}.maeel",
                                    target.replace('.', "/")
                                );

                                read_to_string(file_name)
                                    .expect("Failed to include file")
                            }
                        };

                        process_tokens(
                            &mut maeel_lexer::lex_into_tokens(
                                &content,
                            )
                            .iter(),
                            &mut Vec::default(),
                            globals,
                            procs,
                            structs,
                        )?;
                    }

                    identifier => {
                        match (
                            globals.get(identifier),
                            locals.get(identifier),
                        ) {
                            (None, Some(value)) => {
                                data.push(value.clone());
                                continue
                            }
                            (Some(value), None) => {
                                data.push(value.clone());
                                continue
                            }
                            (Some(_), Some(_)) => {
                                panic!()
                            }
                            (..) => {}
                        }

                        if let Some(fields_names) = structs
                            .get(identifier)
                            .cloned()
                        {
                            let Some(VMType::Array(fields_values)) = data.pop() else {
                                panic!()
                            };

                            let mut struct_kv = HashMap::new();

                            zip(fields_names, fields_values)
                                .for_each(|(k, v)| {
                                    struct_kv.insert(k, v);
                                });

                            data.push(VMType::Struct((
                                identifier.to_string(),
                                struct_kv,
                            )));

                            continue
                        }

                        process_tokens(
                            &mut procs
                                .get(identifier)
                                .expect(identifier)
                                .clone()
                                .iter(),
                            data,
                            globals,
                            procs,
                            structs,
                        )?;
                    }
                }
            }

            Token::BlockStart | Token::ArrayEnd | Token::IEnd => {
                panic!()
            }

            Token::BlockEnd => {}
        };
    }

    Ok(data)
}

/// Parses and executes a code block if a certain condition is true.
fn handle_take(data: &mut Stack)
{
    match data.pop() {
        // 'take' expect an array on top of the stack
        Some(VMType::Integer(max_index)) => {
            let mut array = (0..max_index)
                .map(|_| data.pop().unwrap())
                .collect::<Stack>();

            array.reverse();

            data.push(VMType::Array(array));
        }
        _ => panic!(),
    }
}

/// Parses and executes a code block if a certain condition is true.
fn handle_get(data: &mut Stack)
{
    match (data.pop(), data.pop()) {
        (
            Some(VMType::Integer(index)),
            Some(VMType::Array(array)),
        ) => {
            data.push(
                array
                    .get(index as usize)
                    .unwrap()
                    .clone(),
            );
        }
        _ => panic!(),
    }
}
