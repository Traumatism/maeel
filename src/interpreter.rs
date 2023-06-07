use hashbrown::HashMap;

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
            Ok(MaeelType::$variant(value)) => value,

            other => panic!("Expected {}, got {}", stringify!($variant), other?),
        }
    }};

    ($vm:expr, $variant_a:ident, $variant_b:ident) => {{
        match $vm.pop() {
            Ok(MaeelType::$variant_a(value)) => MaeelType::$variant_a(value),

            Ok(MaeelType::$variant_b(value)) => MaeelType::$variant_b(value),

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
        $vm.push(MaeelType::$variant($value))
    };

    ($vm:expr, $value:expr) => {
        $vm.push($value)
    };
}

fn parse_array(
    tokens: &mut Vec<Token>,
    vm: &mut dyn MaeelVM<Data = MaeelType>,
    vars: &mut HashMap<String, MaeelType>,
    funs: &mut HashMap<String, Vec<Token>>,
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
                match vars.get(&identifier) {
                    Some(value) => {
                        maeelvm_push!(xs, value.clone()); // Push the variable content
                        continue;
                    }

                    // Must be in funs
                    None => {
                        maeelvm_push!(
                            xs,
                            Function,
                            funs.get(&identifier)
                                .cloned()
                                .unwrap_or_else(|| panic!("{identifier} isn't in scope!"))
                        );
                    }
                }
            }

            other => panic!("Found unexpected token while parsing array: {other:?}"),
        }
    }

    maeelvm_push!(vm, Array, xs)?;
    Ok(())
}

pub fn process_tokens(
    tokens_iter: &[Token],                  /* Program tokens */
    vm: &mut dyn MaeelVM<Data = MaeelType>, /* Program stack VM */
    vars: &mut HashMap<String, MaeelType>,  /* Global vars */
    funs: &mut HashMap<String, Vec<Token>>, /* Global funs */
) -> Result<(), Box<dyn Error>> {
    let mut tokens = tokens_iter.iter().rev().cloned().collect::<Vec<Token>>();

    while let Some(token) = tokens.pop() {
        match token {
            Token::Call => {
                process_tokens(&maeelvm_expect!(vm, Function), vm, &mut vars.clone(), funs)?;
            }

            Token::FunctionDefinition => {
                let fun_name = maeel_expect!(tokens, Identifier);

                let mut fun_tokens = Vec::default();

                while let Some(tmp_token) = tokens.pop() {
                    match tmp_token {
                        Token::Block(tmp_tokens) => {
                            fun_tokens.reverse();
                            fun_tokens.extend(tmp_tokens);
                            fun_tokens.reverse();

                            break;
                        }

                        Token::Identifier(_) => {
                            fun_tokens.push(tmp_token);
                            fun_tokens.push(Token::Assignment);
                        }

                        _ => panic!(),
                    }
                }

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
                    MaeelType::Array(xs) => {
                        xs.iter().for_each(|x| {
                            maeelvm_push!(vm, x.clone()).unwrap();
                            process_tokens(&tmp_tokens, vm, vars, funs).unwrap();
                        });
                    }

                    MaeelType::String(string) => {
                        string.chars().for_each(|x| {
                            maeelvm_push!(vm, String, x.to_string()).unwrap();
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

            Token::GreaterThan => vm.binary_op(|a, b| MaeelType::Integer((b > a) as i64))?, /* Process "greater than" binary operation */

            Token::LowerThan => vm.binary_op(|a, b| MaeelType::Integer((b < a) as i64))?, /* Process "lower than" binary operation */

            Token::Equal => vm.binary_op(|a, b| MaeelType::Integer((b == a) as i64))?, /* Process "equal" binary operation */

            Token::Plus => vm.binary_op(|a, b| b + a)?, /* Process "add" binary operation */

            Token::Minus => vm.binary_op(|a, b| b - a)?, /* Process "substract" binary operation */

            Token::Times => vm.binary_op(|a, b| b * a)?, /* Process "multiply" binary operation */

            Token::Divide => vm.binary_op(|a, b| b / a)?, /* Process "divide" binary operation */

            Token::Modulo => vm.binary_op(|a, b| b % a)?, /* Process "modulo" binary operation */

            Token::Clear => vm.clear()?, /* Clear the VM stack */

            Token::String(content) => maeelvm_push!(vm, String, content)?, /* Push a string */

            Token::Float(content) => maeelvm_push!(vm, Float, content)?, /* Push a float */

            Token::Integer(content) => maeelvm_push!(vm, Integer, content)?, /* Push an integer */

            Token::Block(tokens) => maeelvm_push!(vm, Function, tokens)?, /* Push an anonymous function */

            Token::Get => {
                let index = maeelvm_expect!(vm, Integer) as usize;

                match vm.pop() {
                    Ok(MaeelType::Array(xs)) => {
                        maeelvm_push!(vm, xs.get(index).unwrap().clone())
                    }

                    Ok(MaeelType::String(string)) => {
                        maeelvm_push!(vm, String, string.chars().nth(index).unwrap().to_string())
                    }

                    Ok(other) => panic!("{other} is not indexable!"),
                    _ => panic!("Nothing to index!"),
                }?
            }

            Token::Identifier(identifier) => match identifier.as_str() {
                "print" => print!("{}", vm.peek()?),

                "break" => break,

                "vmdrop" => vm.fastpop()?, /* Process "fastpop" VM operation */

                "vmdup" => vm.dup()?, /* Process "dup" VM operation */

                "vmswap" => vm.swap()?, /* Process "swap" VM operation */

                "vmover" => vm.over()?, /* Process "over" VM operation */

                "vmrot" => vm.rot()?, /* Process "rotate" VM operation */

                "vmtype" => match vm.peek()? {
                    MaeelType::Float(_) => {
                        maeelvm_push!(vm, MaeelType::String(String::from("float")))?
                    }

                    MaeelType::Integer(_) => {
                        maeelvm_push!(vm, MaeelType::String(String::from("integer")))?
                    }

                    MaeelType::String(_) => {
                        maeelvm_push!(vm, MaeelType::String(String::from("string")))?
                    }

                    MaeelType::Array(_) => {
                        maeelvm_push!(vm, MaeelType::String(String::from("array")))?
                    }

                    MaeelType::Function(_) => {
                        maeelvm_push!(vm, MaeelType::String(String::from("function")))?
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
                            .map(|byte| MaeelType::Integer(*byte as i64))
                            .collect()
                    )?
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
                        maeelvm_push!(vm, value.clone())?;
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

    Ok(())
}
