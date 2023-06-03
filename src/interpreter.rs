use crate::lexer::*;
use crate::vm::*;

use std::error::Error;
use std::fs::read_to_string;
use std::io::Read;

macro_rules! maeel_expect {
    ($tokens:expr, $variant:ident) => {{
        match $tokens.pop() {
            Some(Token::$variant(value)) => value,
            other => panic!("Expected {}, got {:?}", stringify!($variant), other),
        }
    }};
}

macro_rules! maeelvm_expect {
    ($vm:expr, $variant:ident) => {{
        match $vm.pop() {
            Ok(VMType::$variant(value)) => value,
            other => panic!("Expected {}, got {}", stringify!($variant), other?),
        }
    }};

    ($vm:expr, $variant_a:ident, $variant_b:ident) => {{
        match $vm.pop() {
            Ok(VMType::$variant_a(value)) => VMType::$variant_a(value),
            Ok(VMType::$variant_b(value)) => VMType::$variant_b(value),
            other => panic!(
                "Expected {} or {}, got {}",
                stringify!($variant_a),
                stringify!($variant_b),
                other?
            ),
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

fn parse_array(
    tokens: &mut Vec<Token>,
    vm: &mut VM,
    vars: &mut hashbrown::HashMap<String, VMType>,
    funs: &mut hashbrown::HashMap<String, Vec<Token>>,
) -> Result<(), Box<dyn Error>> {
    let mut xs = Vec::default();

    while let Some(tmp_token) = tokens.pop() {
        match tmp_token {
            Token::ArrayEnd => break,

            Token::ArrayStart => {
                parse_array(tokens, vm, vars, funs)?;

                maeelvm_push!(xs, vm.pop()?);
            }

            Token::String(value) => maeelvm_push!(xs, String, value),

            Token::Float(value) => maeelvm_push!(xs, Float, value),

            Token::Integer(value) => maeelvm_push!(xs, Integer, value),

            Token::Block(tmp_tokens) => maeelvm_push!(xs, Function, tmp_tokens),

            Token::Identifier(identifier) => {
                if let Some(value) = vars.get(&identifier) {
                    maeelvm_push!(vm, value.clone()); // Push the variable content

                    if identifier.starts_with('_') {
                        vars.remove(&identifier);
                    }

                    continue;
                }

                maeelvm_push!(
                    xs,
                    Function,
                    funs.get(&identifier)
                        .cloned()
                        .unwrap_or_else(|| panic!("{identifier} isn't in scope!"))
                );
            }

            other => panic!("Found unexpected token while parsing array: {other:?}"),
        }
    }

    maeelvm_push!(vm, Array, xs);
    Ok(())
}

pub fn process_tokens<'a>(
    tokens_iter: &[Token],                                /* Program tokens */
    vm: &'a mut VM,                                       /* Program vm stack */
    vars: &'a mut hashbrown::HashMap<String, VMType>,     /* Global vars */
    funs: &'a mut hashbrown::HashMap<String, Vec<Token>>, /* Global funs */
) -> Result<&'a mut VM, Box<dyn Error>> {
    let mut tokens = tokens_iter.iter().rev().cloned().collect::<Vec<Token>>();

    while let Some(token) = tokens.pop() {
        match token {
            // Call anonymous funs
            Token::Call => {
                process_tokens(
                    &maeelvm_expect!(vm, Function),
                    vm,                // give a ref to the vm
                    &mut vars.clone(), // copy the vars
                    funs,              // give a ref to the funs
                )?;
            }

            Token::FunctionDefinition => {
                let fun_name = maeel_expect!(tokens, Identifier);

                let mut fun_tokens = Vec::default();

                while let Some(tmp_token) = tokens.pop() {
                    match tmp_token {
                        Token::Block(tmp_tokens) => {
                            fun_tokens.reverse();
                            fun_tokens.extend(tmp_tokens);

                            break;
                        }

                        Token::Identifier(value) => {
                            fun_tokens.extend([Token::Identifier(value), Token::Assignment]);
                        }

                        _ => panic!(),
                    }
                }

                fun_tokens.reverse();

                funs.insert(fun_name, fun_tokens);
            }

            // Parse while statement
            Token::While => {
                // While requires a code block to execute
                let tmp_tokens = maeel_expect!(tokens, Block);

                while maeelvm_expect!(vm, Integer) == 1 {
                    process_tokens(&tmp_tokens, vm, vars, funs)?;
                }
            }

            // Parse for statement
            Token::For => {
                // For requires a code block to execute
                let tmp_tokens = maeel_expect!(tokens, Block);

                // For requires an indexable on the stack top
                match maeelvm_expect!(vm, Array, String) {
                    VMType::Array(xs) => {
                        xs.iter().for_each(|x| {
                            maeelvm_push!(vm, x.clone());
                            process_tokens(&tmp_tokens, vm, vars, funs).unwrap();
                        });
                    }

                    VMType::String(string) => {
                        string.chars().for_each(|x| {
                            maeelvm_push!(vm, String, x.to_string());
                            process_tokens(&tmp_tokens, vm, vars, funs).unwrap();
                        });
                    }

                    other => panic!("{other} is not indexable!"),
                }
            }

            // Parse if statement
            Token::Then => {
                // Then requires a code block to execute
                let tmp_tokens = maeel_expect!(tokens, Block);

                // Check if stack top value is a TRUE value
                if maeelvm_expect!(vm, Integer) == 1 {
                    tmp_tokens
                        .iter()
                        .rev()
                        .cloned()
                        .for_each(|token| tokens.push(token));
                }
            }

            // Parse an xs (recursive => separated function)
            Token::ArrayStart => parse_array(&mut tokens, vm, vars, funs)?,

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
            Token::String(content) => maeelvm_push!(vm, String, content),

            // Push a float to the stack
            Token::Float(content) => maeelvm_push!(vm, Float, content),

            // Push an integer to the stack
            Token::Integer(content) => maeelvm_push!(vm, Integer, content),

            // Push an anonymous function to the stack
            Token::Block(tokens) => maeelvm_push!(vm, Function, tokens),

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

                "break" => break,

                "vmdrop" => {
                    vm.pop()?;
                }

                "vmdup" => vm.dup()?,
                "vmswap" => vm.swap()?,
                "vmover" => vm.over()?,
                "vmrot" => vm.rot()?,

                "vmtype" => match vm.peek()? {
                    VMType::Float(_) => maeelvm_push!(vm, VMType::String(String::from("float"))),
                    VMType::Integer(_) => {
                        maeelvm_push!(vm, VMType::String(String::from("integer")))
                    }
                    VMType::String(_) => maeelvm_push!(vm, VMType::String(String::from("string"))),
                    VMType::Array(_) => maeelvm_push!(vm, VMType::String(String::from("array"))),
                    VMType::Function(_) => {
                        maeelvm_push!(vm, VMType::String(String::from("function")))
                    }
                },

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
                        _ => read_to_string(target).expect("Failed to include file"),
                    };

                    lex_into_tokens(&content)
                        .iter()
                        .rev()
                        .cloned()
                        .for_each(|token| tokens.push(token))
                }

                identifier => {
                    if let Some(value) = vars.get(identifier) {
                        maeelvm_push!(vm, value.clone()); // Push the variable content

                        if identifier.starts_with('_') {
                            vars.remove(identifier);
                        }

                        continue;
                    }

                    funs.get(identifier)
                        .unwrap_or_else(|| panic!("{identifier} isn't in scope!"))
                        .iter()
                        .cloned()
                        .for_each(|token| tokens.push(token));
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
