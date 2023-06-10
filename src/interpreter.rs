use hashbrown::HashMap;

use crate::lexer::*;
use crate::vm::*;

use std::error::Error;
use std::fs::read_to_string;
use std::io::Read;

#[derive(Clone)]
pub struct Function {
    tokens: Vec<Token>,
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
    tokens: &'a [Token],                       /* Program tokens */
    vm: &'a mut dyn MaeelVM<Data = MaeelType>, /* Program stack VM */
    vars: &'a mut HashMap<String, MaeelType>,  /* Global vars */
    funs: &'a mut HashMap<String, Function>,   /* Global funs */
) -> Result<(), Box<dyn Error>> {
    let mut tokens: Vec<Token> = tokens
        .iter()
        .rev() /* Reverse the tokens */
        .cloned() /* Clone the tokens */
        .collect(); /* Return it as a vector */

    while let Some(token) = tokens.pop() {
        match token {
            Token::Call => {
                process_tokens(&maeelvm_expect!(vm, Function), vm, &mut vars.clone(), funs)?;
            }

            Token::FunctionDefinition => {
                let mut fun_name = maeel_expect!(tokens, Identifier);
                let mut inline = false;

                if fun_name == "inline" {
                    inline = true;
                    fun_name = maeel_expect!(tokens, Identifier)
                }

                let mut fun_tokens = Vec::default();

                while let Some(temporary_tokens) = tokens.pop() {
                    match temporary_tokens {
                        Token::Block(temporary_tokens) => {
                            fun_tokens.reverse();
                            fun_tokens.extend(temporary_tokens);
                            fun_tokens.reverse();

                            break;
                        }

                        Token::Identifier(_) => {
                            fun_tokens.push(temporary_tokens);
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

            Token::While => {
                let temporary_tokens = maeel_expect!(tokens, Block);

                while maeelvm_expect!(vm, Integer) == 1 {
                    process_tokens(&temporary_tokens, vm, vars, funs)?;
                }
            }

            Token::For => {
                let temporary_tokens = maeel_expect!(tokens, Block);

                match maeelvm_expect!(vm, Array, String) {
                    MaeelType::Array(xs) => {
                        xs.iter().for_each(|x| {
                            vm.push(x.clone()).unwrap();
                            process_tokens(&temporary_tokens, vm, vars, funs).unwrap();
                        });
                    }

                    MaeelType::String(string) => {
                        string.chars().for_each(|x| {
                            vm.push(MaeelType::String(x.to_string())).unwrap();
                            process_tokens(&temporary_tokens, vm, vars, funs).unwrap();
                        });
                    }

                    other => panic!("{other} is not indexable!"),
                }
            }

            Token::Then => {
                let temporary_token = tokens.pop().unwrap();

                if maeelvm_expect!(vm, Integer) == 1 {
                    match temporary_token {
                        Token::Block(temporary_tokens) => temporary_tokens
                            .iter()
                            .rev()
                            .cloned()
                            .for_each(|token| tokens.push(token)),

                        _ => tokens.push(temporary_token),
                    }
                }
            }

            Token::ArrayStart => parse_array(&mut tokens, vm, vars, funs)?,

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

            Token::String(content) => vm.push(MaeelType::String(content))?, /* Push a string */

            Token::Float(content) => vm.push(MaeelType::Float(content))?, /* Push a float */

            Token::Integer(content) => vm.push(MaeelType::Integer(content))?, /* Push an integer */

            Token::Block(content) => vm.push(MaeelType::Function(content))?, /* Push an anonymous function */

            Token::Get => {
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

            Token::Identifier(identifier) => match identifier.as_str() {
                "print" => print!("{}", vm.peek()?), /* Print the top token */

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
                        .cloned()
                        .for_each(|token| tokens.push(token))
                }

                identifier => {
                    if let Some(value) = vars.get(identifier) {
                        vm.push(value.clone())?;
                        continue;
                    }

                    let fun = funs
                        .get(identifier)
                        .unwrap_or_else(|| panic!("{identifier} isn't in scope!"))
                        .clone();

                    if fun.inline {
                        fun.tokens
                            .iter()
                            .for_each(|token| tokens.push(token.clone()));
                        continue;
                    }

                    let mut fun_tokens = fun.tokens;

                    fun_tokens.reverse();
                    process_tokens(&fun_tokens, vm, &mut vars.clone(), funs)?;
                }
            },

            Token::BlockEnd | Token::BlockStart | Token::ArrayEnd => panic!(),
        };
    }

    Ok(())
}
