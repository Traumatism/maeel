use hashbrown::HashMap;

use crate::lexer::*;
use crate::vm::*;

use std::error::Error;
use std::fs::read_to_string;
use std::io::Read;

/// Representation of a function
#[derive(Clone)]
pub struct Function {
    /* Function tokens */
    tokens: Vec<Token>,

    /* If function is not inline, function tokens will be called
    recursively. Otherwise, it will push function tokens on the
    main tokens stack */
    inline: bool,
}

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

fn parse_array(
    tokens: &mut Vec<Token>,
    vm: &mut dyn MaeelVM<Data = MaeelType>,
    vars: &mut HashMap<String, MaeelType>,
    funs: &mut HashMap<String, Function>,
) -> Result<(), Box<dyn Error>> {
    let mut xs = Vec::default();

    while let Some(temporary_token) = tokens.pop() {
        match temporary_token {
            Token::ArrayEnd => break,

            Token::ArrayStart => {
                parse_array(tokens, vm, vars, funs)?;
                xs.push(vm.pop()?);
            }

            Token::String(value) => xs.push(MaeelType::String(value)),

            Token::Float(value) => xs.push(MaeelType::Float(value)),

            Token::Integer(value) => xs.push(MaeelType::Integer(value)),

            Token::Block(value) => xs.push(MaeelType::Function(value)),

            Token::Identifier(identifier) => match vars.get(&identifier) {
                Some(value) => {
                    xs.push(value.clone());
                    continue;
                }

                None => {
                    let fun = funs
                        .get(&identifier)
                        .unwrap_or_else(|| panic!("{identifier} isn't in scope!"));

                    let tokens = &fun.tokens;

                    xs.push(MaeelType::Function(tokens.clone()));
                }
            },

            other => panic!("Found unexpected token while parsing array: {other:?}"),
        }
    }

    vm.push(MaeelType::Array(xs))?;

    Ok(())
}

pub fn process_tokens<'a>(
    tokens: &'a [Token],                        /* Program tokens */
    vm: &'a mut dyn MaeelVM<Data = MaeelType>,  /* Program stack VM */
    vars: &'a mut HashMap<String, MaeelType>,   /* Global vars */
    funs: &'a mut HashMap<String, Function>,    /* Global funs */
    structs: &mut HashMap<String, Vec<String>>, /* Global structs */
) -> Result<(), Box<dyn Error>> {
    // Parse the tokens into a stack
    let mut tokens: Vec<Token> = tokens
        .iter()
        .rev() /* Reverse the tokens */
        .cloned() /* Clone the tokens */
        .collect(); /* Return it as a vector */

    while let Some(token) = tokens.pop() {
        match token {
            Token::At => match vm.pop() {
                Ok(MaeelType::Structure(structure)) => vm.push(
                    structure
                        .get(&maeel_expect!(tokens, Identifier))
                        .unwrap()
                        .clone(),
                )?,

                _ => panic!(),
            },

            Token::Call => {
                process_tokens(
                    &maeelvm_expect!(vm, Function),
                    vm,
                    &mut vars.clone(),
                    funs,
                    structs,
                )?;
            }

            Token::Then => {
                let temporary_token = tokens.pop().unwrap();

                match vm.pop() {
                    Ok(MaeelType::Integer(1)) => match temporary_token {
                        Token::Block(temporary_tokens) => temporary_tokens
                            .iter()
                            .rev()
                            .for_each(|token| tokens.push(token.clone())),

                        _ => tokens.push(temporary_token),
                    },

                    Ok(MaeelType::Integer(0)) => {}

                    _ => panic!(),
                }
            }

            Token::ArrayStart => parse_array(&mut tokens, vm, vars, funs)?,

            Token::Assignment => {
                vars.insert(maeel_expect!(tokens, Identifier), vm.pop()?);
            }

            Token::BinaryOP(app) => vm.binary_op(app)?, /* Perform a binary operation */

            Token::String(content) => vm.push(MaeelType::String(content))?, /* Push a string */

            Token::Float(content) => vm.push(MaeelType::Float(content))?, /* Push a float */

            Token::Integer(content) => vm.push(MaeelType::Integer(content))?, /* Push an integer */

            Token::Block(content) => vm.push(MaeelType::Function(content))?, /* Push an anonymous function */

            Token::Identifier(identifier) => match identifier.as_str() {
                "print" => print!("{}", vm.peek()?), /* Print the top token */

                "for" => {
                    let temporary_tokens = maeel_expect!(tokens, Block);

                    match maeelvm_expect!(vm, Array, String) {
                        MaeelType::Array(xs) => {
                            xs.iter().for_each(|x| {
                                vm.push(x.clone()).unwrap();
                                process_tokens(&temporary_tokens, vm, vars, funs, structs).unwrap();
                            });
                        }

                        MaeelType::String(string) => {
                            string.chars().for_each(|x| {
                                vm.push(MaeelType::String(x.to_string())).unwrap();
                                process_tokens(&temporary_tokens, vm, vars, funs, structs).unwrap();
                            });
                        }

                        other => panic!("{other} is not indexable!"),
                    }
                }

                "while" => {
                    let temporary_tokens = maeel_expect!(tokens, Block);

                    while match vm.pop() {
                        Ok(MaeelType::Integer(1)) => true,  /* Continue looping */
                        Ok(MaeelType::Integer(0)) => false, /* Stop looping */
                        _ => panic!(),                      /* No boolean on the stack */
                    } {
                        process_tokens(&temporary_tokens, vm, vars, funs, structs)?
                    }
                }

                "struct" => {
                    let struct_name = maeel_expect!(tokens, Identifier);
                    let mut struct_fields = Vec::default();

                    while let Some(temporary_tokens) = tokens.pop() {
                        match temporary_tokens {
                            Token::Dot => {
                                break;
                            }

                            Token::Identifier(identifier) => {
                                struct_fields.push(identifier);
                            }

                            _ => panic!(),
                        }
                    }

                    structs.insert(struct_name, struct_fields);
                }

                "fun" => {
                    let mut fun_name = maeel_expect!(tokens, Identifier);
                    let mut inline = false;

                    if fun_name == "inline" {
                        inline = true;
                        fun_name = maeel_expect!(tokens, Identifier)
                    }

                    let mut fun_tokens = Vec::default();

                    while let Some(temporary_token) = tokens.pop() {
                        match temporary_token {
                            Token::Block(temporary_tokens) => {
                                fun_tokens.reverse();
                                fun_tokens.extend(temporary_tokens);
                                fun_tokens.reverse();

                                break;
                            }

                            Token::Identifier(_) => {
                                fun_tokens.push(temporary_token);
                                fun_tokens.push(Token::Assignment);
                            }

                            _ => panic!(),
                        }
                    }

                    funs.insert(
                        fun_name.clone(),
                        Function {
                            tokens: fun_tokens,
                            inline,
                        },
                    );
                }

                "clear" => vm.clear()?,

                "get" => {
                    let index = maeelvm_expect!(vm, Integer) as usize;

                    match vm.pop() {
                        Ok(MaeelType::Array(xs)) => vm.push(xs.get(index).unwrap().clone()),

                        Ok(MaeelType::String(string)) => vm.push(MaeelType::String(
                            string.chars().nth(index).unwrap().to_string(),
                        )),

                        Ok(other) => panic!("{other} is not indexable!"),
                        _ => panic!("Nothing to index!"),
                    }?
                }

                "break" => break, /* Stop processing the tokens */

                "vmdrop" => vm.fastpop()?, /* Process "fastpop" VM operation */

                "vmdup" => vm.dup()?, /* Process "dup" VM operation */

                "vmswap" => vm.swap()?, /* Process "swap" VM operation */

                "vmover" => vm.over()?, /* Process "over" VM operation */

                "vmrot" => vm.rot()?, /* Process "rotate" VM operation */

                "read" => {
                    let bytes = maeelvm_expect!(vm, Integer);
                    let path = maeelvm_expect!(vm, String);

                    assert!(bytes >= 0);

                    let mut buf = vec![0u8; bytes as usize];

                    std::fs::File::open(path)?.read_exact(&mut buf)?;

                    vm.push(MaeelType::Array(
                        buf.iter()
                            .map(|byte| MaeelType::Integer(*byte as i64))
                            .collect(),
                    ))?
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
                        .for_each(|token| tokens.push(token.clone()))
                }

                identifier => {
                    if let Some(value) = vars.get(identifier) {
                        vm.push(value.clone())?;
                        continue;
                    } else if let Some(fun) = funs.get(identifier) {
                        if fun.inline {
                            fun.tokens
                                .iter()
                                .for_each(|token| tokens.push(token.clone()));

                            continue;
                        }

                        let mut fun_tokens = fun.tokens.clone();

                        fun_tokens.reverse();

                        process_tokens(&fun_tokens, vm, &mut vars.clone(), funs, structs)?;
                    } else if let Some(fields) = structs.get(identifier) {
                        let mut structure: HashMap<String, MaeelType> =
                            HashMap::with_capacity(fields.len());

                        fields.iter().rev().for_each(|key| {
                            structure.insert(key.clone(), vm.pop().unwrap());
                        });

                        vm.push(MaeelType::Structure(structure))?
                    } else {
                        panic!()
                    }
                }
            },

            Token::BlockEnd | Token::BlockStart | Token::ArrayEnd | Token::Dot => panic!(),
        };
    }

    Ok(())
}
