use std::error::Error;
use std::io::Read;
use std::slice::Iter;

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
    ($vm:expr, $variant:ident) => {{
        match $vm.pop() {
            Ok(VMType::$variant(value)) => value.clone(),
            other => panic!("Expected {}, got {}", stringify!($variant), other?),
        }
    }};

    ($vm:expr, $variant_a:ident, $variant_b:ident) => {{
        match $vm.pop() {
            Ok(VMType::$variant_a(value)) => VMType::$variant_a(value.clone()),
            Ok(VMType::$variant_b(value)) => VMType::$variant_b(value.clone()),
            other => panic!("Expected {}, got {}", stringify!($variant), other?),
        }
    }};
}

macro_rules! maeelvm_push {
    ($vm:expr, $variant:ident, $value:expr) => {
        $vm.push(VMType::$variant($value))
    };

    ($vm:expr, $value:expr) => {
        $vm.push($value)
    };
}

macro_rules! maeelvm_binop {
    ($vm:expr, $operator:tt) => {{
        let (a, b) = ($vm.pop()?, $vm.pop()?);
        maeelvm_push!($vm, b $operator a)
    }};

    ($vm:expr, $operator:tt, $variant:ident) => {{
        let (a, b) = ($vm.pop()?, $vm.pop()?);
        maeelvm_push!($vm, $variant, (b $operator a) as i64);
    }};
}

fn parse_xs(
    tokens: &mut Iter<Token>,
    vm: &mut VMStack,
    vars: &mut hashbrown::HashMap<String, VMType>,
    funs: &mut hashbrown::HashMap<String, Vec<Token>>,
) -> Result<(), Box<dyn Error>> {
    let mut xs = Vec::default();

    loop {
        match tokens.next() {
            Some(Token::ArrayEnd) => break,

            Some(Token::ArrayStart) => {
                parse_xs(tokens, vm, vars, funs)?;
                maeelvm_push!(xs, vm.pop()?);
            }

            Some(Token::String(value)) => maeelvm_push!(xs, String, value.clone()),

            Some(Token::Float(value)) => maeelvm_push!(xs, Float, *value),

            Some(Token::Integer(value)) => maeelvm_push!(xs, Integer, *value),

            Some(Token::Block(expr)) => maeelvm_push!(xs, Function, expr.clone()),

            Some(Token::Identifier(identifier)) => {
                match vars.get(identifier) {
                    Some(value) => {
                        maeelvm_push!(xs, value.clone()); // Push the variable content
                        continue;
                    }

                    // Must be in funs
                    None => {
                        maeelvm_push!(
                            xs,
                            Function,
                            funs.get(identifier)
                                .unwrap_or_else(|| panic!("{identifier} isn't in scope!"))
                                .clone()
                        );
                    }
                }
            }

            other => panic!("Found unexpected token while parsing array: {other:?}"),
        }
    }

    maeelvm_push!(vm, Array, xs);

    Ok(())
}

pub fn process_tokens<'a>(
    tokens: &'a mut Iter<Token>,                          /* Program tokens */
    vm: &'a mut VMStack,                                  /* Program vm stack */
    vars: &'a mut hashbrown::HashMap<String, VMType>,     /* Global vars */
    funs: &'a mut hashbrown::HashMap<String, Vec<Token>>, /* Global funs */
) -> Result<&'a mut VMStack, Box<dyn Error>> {
    while let Some(token) = tokens.next() {
        match token {
            // Call anonymous funs
            Token::Call => {
                process_tokens(
                    &mut maeelvm_expect!(vm, Function).iter(),
                    vm,
                    &mut vars.clone(),
                    funs,
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
                            function_block.extend(vec![next_token.clone(), Token::Assignment]);
                        }

                        _ => panic!(),
                    }
                }

                funs.insert(name, function_block);
            }

            // Parse while statement
            Token::While => {
                // While requires a code block to execute
                let tokens = maeel_expect!(tokens, Block);

                while maeelvm_expect!(vm, Integer) == 1 {
                    process_tokens(&mut tokens.iter(), vm, vars, funs)?;
                }
            }

            // Parse for statement
            Token::For => {
                // For requires a code block to execute
                let tokens = maeel_expect!(tokens, Block);

                // For requires an indexable on the stack top
                match maeelvm_expect!(vm, Array, String) {
                    VMType::Array(xs) => {
                        xs.iter().for_each(|x| {
                            maeelvm_push!(vm, x.clone());
                            process_tokens(&mut tokens.iter(), vm, vars, funs).unwrap();
                        });
                    }

                    VMType::String(string) => {
                        string.chars().for_each(|x| {
                            maeelvm_push!(vm, String, x.to_string());
                            process_tokens(&mut tokens.iter(), vm, vars, funs).unwrap();
                        });
                    }

                    other => panic!("{other} is not indexable!"),
                }
            }

            // Parse if statement
            Token::Then => {
                // Then requires a code block to execute
                let tokens = maeel_expect!(tokens, Block);

                // Check if stack top value is a TRUE value
                if maeelvm_expect!(vm, Integer) == 1 {
                    process_tokens(&mut tokens.iter(), vm, vars, funs)?;
                }
            }

            // Parse an xs (recursive => separated function)
            Token::ArrayStart => parse_xs(tokens, vm, vars, funs)?,

            // Assign the stack top value to the next token
            Token::Assignment => {
                vars.insert(maeel_expect!(tokens, Identifier), vm.pop()?);
            }

            // Process the 'is greater than' binary operation
            Token::GreaterThan => maeelvm_binop!(vm, >, Integer),

            // Process the 'is lower than' binary operation
            Token::LowerThan => maeelvm_binop!(vm, <, Integer),

            // Process the 'is equal to' binary operation
            Token::Equal => maeelvm_binop!(vm, ==, Integer),

            // Process the 'add' binary operation
            Token::Plus => maeelvm_binop!(vm, +),

            // Process the 'substract' binary operation
            Token::Minus => maeelvm_binop!(vm, -),

            // Process the 'multiply' binary operation
            Token::Times => maeelvm_binop!(vm, *),

            // Process the 'divide' binary operation
            Token::Divide => maeelvm_binop!(vm, /),

            // Process the 'modulo' binary operation
            Token::Modulo => maeelvm_binop!(vm, %),

            // Clear the vm stack
            Token::Clear => vm.clear(),

            // Push a string to the stack
            Token::String(content) => maeelvm_push!(vm, String, content.clone()),

            // Push a float to the stack
            Token::Float(content) => maeelvm_push!(vm, Float, *content),

            // Push an integer to the stack
            Token::Integer(content) => maeelvm_push!(vm, Integer, *content),

            // Push an anonymous function to the stack
            Token::Block(tokens) => maeelvm_push!(vm, Function, tokens.clone()),

            // Get the n'th element of an indexable
            Token::Get => {
                let index = maeelvm_expect!(vm, Integer) as usize;

                match vm.pop() {
                    Ok(VMType::Array(xs)) => {
                        maeelvm_push!(vm, xs.get(index).unwrap().clone())
                    }

                    Ok(VMType::String(string)) => {
                        maeelvm_push!(vm, String, string.chars().nth(index).unwrap().to_string());
                    }

                    Ok(other) => panic!("{other} is not indexable!"),
                    _ => panic!("Nothing to index!"),
                }
            }

            Token::Identifier(identifier) => match identifier.as_str() {
                "print" => print!("{}", vm.peek()?),

                "read" => {
                    let bytes = maeelvm_expect!(vm, Integer);
                    let path = maeelvm_expect!(vm, String);

                    assert!(bytes >= 0);

                    let mut buf = vec![0u8; bytes as usize];
                    std::fs::File::open(path)?.read_exact(&mut buf)?;

                    maeelvm_push!(
                        vm,
                        Array,
                        buf.iter()
                            .map(|byte| VMType::Integer(*byte as i64))
                            .collect()
                    );
                }

                "include" => {
                    let target = maeelvm_expect!(vm, String);

                    let content = match target.as_str() {
                        "std" => include_str!("../stdlib/std.maeel").to_string(),

                        // Read file to include
                        _ => std::fs::read_to_string(format!("{}.maeel", target.replace('.', "/")))
                            .expect("Failed to include file"),
                    };

                    process_tokens(
                        &mut lex_into_tokens(&content).iter(),
                        &mut VMStack::new(),            // don't copy the vm
                        &mut hashbrown::HashMap::new(), // give a ref to the vars
                        funs,                           // give a ref to the funs
                    )?;
                }

                identifier => {
                    match vars.get(identifier) {
                        Some(value) => {
                            maeelvm_push!(vm, value.clone()); // Push the variable content
                            continue;
                        }

                        // Must be in funs
                        None => {
                            // Execute the function
                            process_tokens(
                                /* Extract function tokens */
                                &mut funs
                                    .get(identifier)
                                    .unwrap_or_else(|| panic!("{identifier} isn't in scope!"))
                                    .clone()
                                    .iter(),
                                vm,                // give a ref to the vm
                                &mut vars.clone(), // copy the vars
                                funs,              // give a ref to the funs
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

    Ok(vm)
}
