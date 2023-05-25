use std::io::Read;

use crate::lexer::*;
use crate::vm::*;

macro_rules! maeel_expect {
    ($tokens:expr, $variant:ident) => {{
        match $tokens.next() {
            Some(Token::$variant(value)) => value.clone(),
            other => panic!("Expected {}, got {:?}", stringify!($variant), other),
        }
    }};
}

macro_rules! maeelvm_expect {
    ($data:expr, $variant:ident) => {{
        match $data.pop() {
            Ok(VMType::$variant(value)) => value.clone(),
            other => panic!("Expected {}, got {}", stringify!($variant), other?),
        }
    }};
}

macro_rules! maeelvm_push {
    ($data:expr, $variant:ident, $value:expr) => {
        $data.push(VMType::$variant($value))
    };

    ($data:expr, $value:expr) => {
        $data.push($value)
    };
}

macro_rules! maeelvm_binop {
    ($data:expr, $operator:tt) => {{
        let (a, b) = ($data.pop()?, $data.pop()?);
        maeelvm_push!($data, b $operator a)
    }};

    ($data:expr, $operator:tt, $variant:ident) => {{
        let (a, b) = ($data.pop()?, $data.pop()?);
        maeelvm_push!($data, $variant, (b $operator a) as i64);
    }};
}

fn parse_xs(
    tokens: &mut std::slice::Iter<Token>,
    data: &mut VMStack,
    globals: &mut std::collections::HashMap<String, VMType>,
    functions: &mut std::collections::HashMap<String, Vec<Token>>,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut xs = Vec::default();

    loop {
        match tokens.next() {
            Some(Token::ArrayEnd) => break,

            // Recursion for xs of xs
            Some(Token::ArrayStart) => {
                parse_xs(tokens, data, globals, functions)?;
                maeelvm_push!(xs, data.pop()?);
            }

            Some(Token::String(value)) => maeelvm_push!(xs, String, value.clone()),

            Some(Token::Float(value)) => maeelvm_push!(xs, Float, *value),

            Some(Token::Integer(value)) => maeelvm_push!(xs, Integer, *value),

            Some(Token::Block(expr)) => maeelvm_push!(xs, Function, expr.clone()),

            Some(Token::Identifier(identifier)) => {
                match globals.get(identifier) {
                    Some(value) => {
                        maeelvm_push!(xs, value.clone()); // Push the variable content
                        continue;
                    }

                    // Must be in functions
                    None => {
                        maeelvm_push!(
                            xs,
                            Function,
                            functions
                                .get(identifier)
                                .unwrap_or_else(|| panic!("{identifier} isn't in scope!"))
                                .clone()
                        );
                    }
                }
            }

            other => panic!("Found unexpected token while parsing array: {other:?}"),
        }
    }

    maeelvm_push!(data, Array, xs);

    Ok(())
}

pub fn process_tokens<'a>(
    tokens: &'a mut std::slice::Iter<Token>, /* Program tokens */
    data: &'a mut VMStack,                   /* Program data stack */
    globals: &'a mut std::collections::HashMap<String, VMType>, /* Global variables */
    functions: &'a mut std::collections::HashMap<String, Vec<Token>>, /* Global functions */
) -> Result<&'a mut VMStack, Box<dyn std::error::Error>> {
    while let Some(token) = tokens.next() {
        match token {
            // Call anonymous functions
            Token::Call => {
                process_tokens(
                    &mut maeelvm_expect!(data, Function).iter(),
                    data,
                    &mut globals.clone(),
                    functions,
                )?;
            }

            Token::FunctionDefinition => {
                let name = maeel_expect!(tokens, Identifier);

                let mut function_block = Vec::default();

                for next_token in tokens.by_ref() {
                    match next_token {
                        Token::Block(block) => {
                            function_block.reverse();
                            function_block.extend(block.clone());
                            break;
                        }

                        Token::Identifier(_) => {
                            function_block.push(next_token.clone());
                            function_block.push(Token::Assignment);
                        }

                        _ => panic!(),
                    }
                }

                functions.insert(name, function_block);
            }

            // Parse while statement
            Token::While => {
                // While requires a code block to execute
                let tokens = maeel_expect!(tokens, Block);

                while maeelvm_expect!(data, Integer) == 1 {
                    process_tokens(&mut tokens.iter(), data, globals, functions)?;
                }
            }

            // Parse for statement
            Token::For => {
                // For requires a code block to execute
                let tokens = maeel_expect!(tokens, Block);

                // For requires an indexable on the stack top
                match data.pop() {
                    Ok(VMType::Array(xs)) => {
                        xs.iter().for_each(|x| {
                            maeelvm_push!(data, x.clone());
                            process_tokens(&mut tokens.iter(), data, globals, functions).unwrap();
                        });
                    }

                    Ok(VMType::String(string)) => {
                        string.chars().for_each(|x| {
                            maeelvm_push!(data, String, x.to_string());
                            process_tokens(&mut tokens.iter(), data, globals, functions).unwrap();
                        });
                    }

                    Ok(other) => panic!("{other} is not indexable!"),
                    Err(_) => panic!("Nothing to index!"),
                }
            }

            // Parse if statement
            Token::Then => {
                // Then requires a code block to execute
                let tokens = maeel_expect!(tokens, Block);

                // Check if stack top value is a TRUE value
                if maeelvm_expect!(data, Integer) == 1 {
                    process_tokens(&mut tokens.iter(), data, globals, functions)?;
                }
            }

            // Parse an xs (recursive => separated function)
            Token::ArrayStart => parse_xs(tokens, data, globals, functions)?,

            // Assign the stack top value to the next token
            Token::Assignment => {
                globals.insert(maeel_expect!(tokens, Identifier), data.pop()?);
            }

            // Process the 'is greater than' binary operation
            Token::GreaterThan => maeelvm_binop!(data, >, Integer),

            // Process the 'is lower than' binary operation
            Token::LowerThan => maeelvm_binop!(data, <, Integer),

            // Process the 'is equal to' binary operation
            Token::Equal => maeelvm_binop!(data, ==, Integer),

            // Process the 'add' binary operation
            Token::Plus => maeelvm_binop!(data, +),

            // Process the 'substract' binary operation
            Token::Minus => maeelvm_binop!(data, -),

            // Process the 'multiply' binary operation
            Token::Times => maeelvm_binop!(data, *),

            // Process the 'divide' binary operation
            Token::Divide => maeelvm_binop!(data, /),

            // Process the 'modulo' binary operation
            Token::Modulo => maeelvm_binop!(data, %),

            // Clear the data stack
            Token::Clear => data.clear(),

            // Push a string to the stack
            Token::String(content) => maeelvm_push!(data, String, content.clone()),

            // Push a float to the stack
            Token::Float(content) => maeelvm_push!(data, Float, *content),

            // Push an integer to the stack
            Token::Integer(content) => maeelvm_push!(data, Integer, *content),

            // Push an anonymous function to the stack
            Token::Block(tokens) => maeelvm_push!(data, Function, tokens.clone()),

            // Get the n'th element of an indexable
            Token::Get => {
                let index = maeelvm_expect!(data, Integer);

                match data.pop() {
                    Ok(VMType::Array(xs)) => {
                        maeelvm_push!(data, xs.get(index as usize).unwrap().clone())
                    }

                    Ok(VMType::String(string)) => {
                        maeelvm_push!(
                            data,
                            String,
                            string.chars().nth(index as usize).unwrap().to_string()
                        );
                    }

                    Ok(other) => panic!("{other} is not indexable!"),
                    _ => panic!("Nothing to index!"),
                }
            }

            Token::Identifier(identifier) => match identifier.as_str() {
                "print" => print!("{}", data.peek()?),

                "read" => {
                    let bytes = maeelvm_expect!(data, Integer);
                    let path = maeelvm_expect!(data, String);

                    assert!(bytes >= 0);

                    let mut buf = vec![0u8; bytes as usize];
                    std::fs::File::open(path)?.read_exact(&mut buf)?;

                    maeelvm_push!(
                        data,
                        Array,
                        buf.iter()
                            .map(|byte| VMType::Integer(*byte as i64))
                            .collect()
                    );
                }

                "include" => {
                    let target = maeelvm_expect!(data, String);

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
                    match globals.get(identifier) {
                        Some(value) => {
                            maeelvm_push!(data, value.clone()); // Push the variable content
                            continue;
                        }

                        // Must be in functions
                        None => {
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
