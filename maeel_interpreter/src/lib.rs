use maeel_common::{
    maeel_std::{
        MAEEL_STD_CONTENT,
        MAEEL_STD_MATHS_CONTENT,
    },
    tokens::Token,
    vmtypes::VMType,
};

use std::{collections::HashMap,};
use std::io::Result;
use std::ops::Not;
use std::slice::Iter;

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

/// The `process_tokens` function processes a sequence of tokens and executes the corresponding
/// operations on a stack of values, global variables, and procedures.
pub fn process_tokens<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Vec<VMType>,
    globals: &'a mut HashMap<String, VMType>,
    procs: &'a mut HashMap<String, Vec<Token>>,
) -> Result<&'a mut Vec<VMType>>
{
    // Specific to current code block (won't be given to the next/previous code block)
    let mut locals = HashMap::new();

    while let Some(token) = tokens.next() {
        match token {
            Token::ProcStart => {
                parse_proc(tokens, procs);
            }

            Token::While => {
                parse_while(tokens, data, globals, procs);
            }

            Token::For => {
                parse_for(tokens, data, globals, procs);
            }

            Token::If => {
                parse_if(tokens, data, globals, procs);
            }

            Token::IStart => {
                parse_interval(tokens, data);
            }

            Token::ArrayStart => {
                parse_array(tokens, data, globals, procs);
            }

            Token::Let => {
                parse_assignement(tokens, data, &mut locals, globals);
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

                                std::fs::read_to_string(file_name)
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
                        )?;
                    }

                    identifier => {
                        if let Some(value) = globals.get(identifier) {
                            data.push(value.clone());

                            continue
                        }

                        if let Some(value) = locals.get(identifier) {
                            data.push(value.clone());

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

/// Parses a procedure definition.
fn parse_proc<'a>(
    tokens: &'a mut Iter<Token>,
    procs: &'a mut HashMap<String, Vec<Token>>,
)
{
    // Procedure name
    let name = next!(tokens, "identifier");

    assert_eq!(Some(&Token::IStart), tokens.next());

    let mut procedure_block = Vec::default();

    loop {
        let token = tokens.next();

        match token {
            Some(Token::Identifier(_)) => {
                // Append variable definition to procedure block
                procedure_block.append(&mut vec![
                    Token::Let,
                    token.unwrap().clone(),
                ])
            }

            // List end
            Some(Token::IEnd) => break,

            // We want only identifiers
            _ => panic!(),
        }
    }

    // Finally append real procedure tokens
    procedure_block.append(&mut next!(tokens, "block"));

    procs.insert(name, procedure_block);
}

/// Executes a code block repeatedly while a certain condition is true.
fn parse_while<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Vec<VMType>,
    globals: &'a mut HashMap<String, VMType>,
    procs: &'a mut HashMap<String, Vec<Token>>,
)
{
    // Code block to execute while P(x) is true
    let tokens = next!(tokens, "block");

    // This is why we need to push P(x) at the end of the code block
    while let Some(VMType::Bool(true)) = data.pop() {
        process_tokens(&mut tokens.iter(), data, globals, procs)
            .unwrap();
    }
}

/// Iterates through an array on the stack and executes a code block for each element.
fn parse_for<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Vec<VMType>,
    globals: &'a mut HashMap<String, VMType>,
    procs: &'a mut HashMap<String, Vec<Token>>,
)
{
    // Code block to execute for each value of L
    let tokens = next!(tokens, "block");

    if let Some(VMType::Array(array)) = data.pop() {
        for element in array {
            data.push(element);

            process_tokens(&mut tokens.iter(), data, globals, procs)
                .unwrap();
        }
    } else {
        panic!() // An array must be on the stack's top
    }
}

/// Parses an assignment statement and stores the assigned value in either a local or
/// global variable depending on the variable name.
fn parse_assignement<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Vec<VMType>,
    locals: &'a mut HashMap<String, VMType>,
    globals: &'a mut HashMap<String, VMType>,
)
{
    // Variable name
    let name = next!(tokens, "identifier");

    // Variable privateness/publicness depends of the name
    match name
        .chars()
        .collect::<Vec<char>>()
        .first()
    {
        Some('_') => locals.insert(name, data.pop().unwrap()),
        Some(_) => globals.insert(name, data.pop().unwrap()),
        None => panic!(),
    };
}

/// Parses and executes a code block if a certain condition is true.
fn parse_if<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Vec<VMType>,
    globals: &'a mut HashMap<String, VMType>,
    procs: &'a mut HashMap<String, Vec<Token>>,
)
{
    // Code block to execute if, and only if P(x) is true
    let tokens = next!(tokens, "block");

    if let Some(VMType::Bool(true)) = data.pop() {
        process_tokens(&mut tokens.iter(), data, globals, procs)
            .unwrap();
    }
}

fn parse_interval<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Vec<VMType>,
)
{
    let (
        Some(Token::Integer(start)),
        Some(Token::Integer(end))
    ) = (tokens.next(), tokens.next()) else {
        panic!()
    };

    assert_eq!(Some(&Token::IEnd), tokens.next());

    data.push(VMType::Array(
        (*start..*end)
            .map(VMType::Integer)
            .collect(),
    ));
}

/// Parses and executes a code block if a certain condition is true.
fn parse_array<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Vec<VMType>,
    globals: &'a mut HashMap<String, VMType>,
    procs: &'a mut HashMap<String, Vec<Token>>,
)
{
    let mut array = Vec::default();

    loop {
        match tokens.next().unwrap().clone() {
            Token::ArrayEnd => break,

            Token::ArrayStart => {
                panic!()
            }

            Token::Str(value) => array.push(VMType::Str(value)),

            Token::Integer(value) => {
                array.push(VMType::Integer(value))
            }

            Token::Float(value) => array.push(VMType::Float(value)),

            Token::Bool(value) => array.push(VMType::Bool(value)),

            Token::Identifier(identifier) => {
                match globals.get(&identifier) {
                    Some(value) => array.push(value.clone()),
                    None => panic!(),
                }
            }

            Token::Block(expr) => {
                let generator = process_tokens(
                    &mut next!(tokens, "block").iter(),
                    data,
                    globals,
                    procs,
                )
                .unwrap()
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
                    )
                    .unwrap()
                    .pop();

                    array.push(output.unwrap());
                }
            }

            _ => panic!(),
        }
    }

    data.push(VMType::Array(array))
}

/// Parses and executes a code block if a certain condition is true.
fn handle_take(data: &mut Vec<VMType>)
{
    match data.pop() {
        // 'take' expect an array on top of the stack
        Some(VMType::Integer(max_index)) => {
            let mut array = (0..max_index)
                .map(|_| data.pop().unwrap())
                .collect::<Vec<VMType>>();

            array.reverse();

            data.push(VMType::Array(array));
        }
        _ => panic!(),
    }
}

/// Parses and executes a code block if a certain condition is true.
fn handle_get(data: &mut Vec<VMType>)
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
