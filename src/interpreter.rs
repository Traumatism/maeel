use std::collections::hash_map::HashMap;
use std::io::Read;
use std::io::Result;

use crate::lexer::*;
use crate::vm::*;

macro_rules! next {
    // Grab the next token of the specified variant
    ($tokens:expr, $variant:ident) => {{
        match $tokens.next() {
            Some(Token::$variant(value)) => value.clone(),
            other => panic!("Expected {}, got {:?}", stringify!($variant), other),
        }
    }};
}

macro_rules! perform_binary_op {
    ($data:expr, $operator:tt) => {{
        let (a, b) = (
            $data.pop().expect("Binary operation expect 2 values on the stack"),
            $data.pop().expect("Binary operation expect 2 values on the stack")
        );

        $data.push(b $operator a)
    }};

    ($data:expr, $operator:tt, $vmtype:expr) => {{
        let (a, b) = (
            $data.pop().expect("Binary operation expect 2 values on the stack"),
            $data.pop().expect("Binary operation expect 2 values on the stack")
        );

        $data.push($vmtype((b $operator a) as i64))
    }};
}

fn parse_xs(
    tokens: &mut std::slice::Iter<Token>,
    data: &mut VMStack,
    globals: &mut HashMap<String, VMType>,
    functions: &mut HashMap<String, Vec<Token>>,
    locals: &mut HashMap<String, VMType>,
) {
    let mut xs = Vec::default();

    loop {
        match tokens.next() {
            Some(Token::ArrayEnd) => break,

            // Recursion for xs of xs
            Some(Token::ArrayStart) => {
                parse_xs(tokens, data, globals, functions, locals);
                xs.push(data.pop().unwrap())
            }

            Some(Token::String(value)) => xs.push(VMType::String(value.clone())),

            Some(Token::Float(value)) => xs.push(VMType::Float(*value)),

            Some(Token::Block(expr)) => xs.push(VMType::Function(expr.clone())),

            Some(Token::Integer(value)) => xs.push(VMType::Integer(*value)),

            Some(Token::Identifier(identifier)) => {
                match (globals.get(identifier), locals.get(identifier)) {
                    // Found in locals
                    (None, Some(value)) => {
                        xs.push(value.clone()); // Push the variable content
                        continue;
                    }

                    // Found in globals
                    (Some(value), None) => {
                        xs.push(value.clone()); // Push the variable content
                        continue;
                    }

                    // Both in locals and globals
                    (Some(_), Some(_)) => {
                        panic!("{identifier} is both in globals and locals, bruh!")
                    }

                    // Must be in functions
                    (..) => {}
                }

                xs.push(VMType::Function(
                    functions
                        .get(identifier)
                        .unwrap_or_else(|| panic!("{identifier} isn't in scope!"))
                        .clone(),
                ));
            }

            other => panic!("Found unexpected token while parsing array: {other:?}"),
        }
    }

    data.push(VMType::Array(xs))
}

pub fn process_tokens<'a>(
    tokens: &'a mut std::slice::Iter<Token>,  /* Program tokens */
    data: &'a mut VMStack,                    /* Program data stack */
    globals: &'a mut HashMap<String, VMType>, /* Global variables */
    functions: &'a mut HashMap<String, Vec<Token>>, /* Global functions */
) -> Result<&'a mut VMStack> {
    let mut locals = HashMap::new();

    while let Some(token) = tokens.next() {
        match token {
            // Call anonymous functions
            Token::Call => match data.pop() {
                Some(VMType::Function(block_tokens)) => {
                    process_tokens(
                        &mut block_tokens.iter(),
                        data,
                        &mut globals.clone(),
                        functions,
                    )?;
                }

                Some(other) => panic!("Cannot call {other}"),
                None => panic!("Nothing to call"),
            },
            Token::FunctionDefinition => {
                let mut parameters = Vec::default();
                let mut final_block = Vec::default();
                let mut function_block = Vec::default();

                let name = next!(tokens, Identifier);

                for next_token in tokens.by_ref() {
                    match next_token {
                        Token::Block(block) => {
                            function_block.extend(block.clone());
                            break;
                        }

                        Token::Identifier(_) => parameters.push(next_token),

                        _ => panic!(),
                    }
                }

                parameters.reverse();

                final_block.extend(
                    parameters
                        .iter()
                        .cloned()
                        .flat_map(|parameter| vec![Token::Assignment, parameter.clone()]),
                );

                final_block.extend(function_block);

                functions.insert(name, final_block);
            }

            // Parse while statement
            Token::While => {
                // While requires a code block to execute
                let tokens = next!(tokens, Block);

                while let Some(VMType::Integer(1)) = data.pop() {
                    process_tokens(&mut tokens.iter(), data, globals, functions)?;
                }
            }

            // Parse for statement
            Token::For => {
                // For requires a code block to execute
                let tokens = next!(tokens, Block);

                // For requires an indexable on the stack top
                match data.pop() {
                    Some(VMType::Array(xs)) => {
                        xs.iter().for_each(|x| {
                            data.push(x.clone());
                            process_tokens(&mut tokens.iter(), data, globals, functions).unwrap();
                        });
                    }

                    Some(VMType::String(string)) => {
                        string.chars().for_each(|x| {
                            data.push(VMType::String(String::from(x)));
                            process_tokens(&mut tokens.iter(), data, globals, functions).unwrap();
                        });
                    }

                    Some(other) => panic!("{other} is not indexable!"),

                    None => panic!("Nothing to index!"),
                }
            }

            // Parse if statement
            Token::Then => {
                // Then requires a code block to execute
                let tokens = next!(tokens, Block);

                // Check if stack top value is a TRUE value
                if let Some(VMType::Integer(1)) = data.pop() {
                    process_tokens(&mut tokens.iter(), data, globals, functions)?;
                }
            }

            // Parse an xs (recursive => separated function)
            Token::ArrayStart => parse_xs(tokens, data, globals, functions, &mut locals),

            // Assign the stack top value to the next token
            Token::Assignment => {
                let name = next!(tokens, Identifier);

                match name.chars().collect::<Vec<char>>().first() {
                    // Variable name does start with _ <=> Local variable
                    Some('_') => locals.insert(name.clone(), data.pop().unwrap()),

                    // Variable name does not start with _ <=> Global variable
                    Some(_) => globals.insert(name.clone(), data.pop().unwrap()),

                    // No variable name provided
                    None => panic!("Variable name is missing!"),
                };
            }

            // Process the 'is greater than' binary operation
            Token::GreaterThan => perform_binary_op!(data, >, VMType::Integer),

            // Process the 'is lower than' binary operation
            Token::LowerThan => perform_binary_op!(data, <, VMType::Integer),

            // Process the 'is equal to' binary operation
            Token::Equal => perform_binary_op!(data, ==, VMType::Integer),

            // Process the 'add' binary operation
            Token::Plus => perform_binary_op!(data, +),

            // Process the 'substract' binary operation
            Token::Minus => perform_binary_op!(data, -),

            // Process the 'multiply' binary operation
            Token::Times => perform_binary_op!(data, *),

            // Process the 'divide' binary operation
            Token::Divide => perform_binary_op!(data, /),

            // Process the 'modulo' binary operation
            Token::Modulo => perform_binary_op!(data, %),

            // Clear the data stack
            Token::Clear => data.clear(),

            // Push a string to the stack
            Token::String(content) => data.push(VMType::String(content.clone())),

            // Push a float to the stack
            Token::Float(content) => data.push(VMType::Float(*content)),

            // Push an integer to the stack
            Token::Integer(content) => data.push(VMType::Integer(*content)),

            // Push an anonymous function to the stack
            Token::Block(tokens) => data.push(VMType::Function(tokens.clone())),

            // Get the n'th element of an indexable
            Token::Get => match (data.pop(), data.pop()) {
                (Some(VMType::Integer(index)), Some(VMType::Array(xs))) => {
                    data.push(xs.get(index as usize).unwrap().clone());
                }

                (Some(VMType::Integer(index)), Some(VMType::String(string))) => {
                    data.push(VMType::String(
                        string.chars().nth(index as usize).unwrap().to_string(),
                    ));
                }

                (Some(other), Some(VMType::Integer(_))) => panic!("{other} is not indexable!"),

                (..) => panic!("Nothing to index!"),
            },

            Token::Identifier(identifier) => match identifier.as_str() {
                "print" => print!("{}", data.peek().unwrap()),

                "read" => {
                    let (Some(VMType::Integer(bytes)), Some(VMType::String(path))) = (data.pop(), data.pop()) else {
                        panic!()
                    };

                    assert!(bytes >= 0);

                    let mut buf = vec![0u8; bytes as usize];

                    std::fs::File::open(path)?.read_exact(&mut buf)?;

                    data.push(VMType::Array(
                        buf.iter()
                            .map(|byte| VMType::Integer(*byte as i64))
                            .collect(),
                    ));
                }

                "include" => {
                    // Include requires a string on the stack
                    let Some(VMType::String(target)) = data.pop() else {
                        panic!()
                    };

                    let content = match target.as_str() {
                        "std" => include_str!("../stdlib/std.maeel").to_string(),

                        // Read file to include
                        _ => std::fs::read_to_string(format!("{}.maeel", target.replace('.', "/")))
                            .expect("Failed to include file"),
                    };

                    process_tokens(
                        &mut lex_into_tokens(&content).iter(),
                        &mut VMStack::new(), // don't copy the data
                        globals,             // give a ref to the globals
                        functions,           // give a ref to the functions
                    )?;
                }

                identifier => {
                    match (globals.get(identifier), locals.get(identifier)) {
                        // Found in locals
                        (None, Some(value)) => {
                            data.push(value.clone()); // Push the variable content
                            continue;
                        }

                        // Found in globals
                        (Some(value), None) => {
                            data.push(value.clone()); // Push the variable content
                            continue;
                        }

                        // Both in locals and globals
                        (Some(_), Some(_)) => panic!("?????????????"),

                        // Must be in functions
                        (None, None) => {
                            // Execute the function
                            process_tokens(
                                /* Extract function tokens */
                                &mut functions.get(identifier).expect(identifier).clone().iter(),
                                data,                 // give a ref to the data
                                &mut globals.clone(), // copy the globals
                                functions,            // give a ref to the functions
                            )?;
                        }
                    }
                }
            },

            // This should not be here
            Token::BlockStart | Token::ArrayEnd => {
                panic!()
            }

            // uhm
            Token::BlockEnd => {}
        };
    }

    Ok(data)
}
