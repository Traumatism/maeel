use hashbrown::HashMap;

use crate::lexer::{lex_into_tokens, Token};
use crate::vm::MaeelType;

use std::error::Error;
use std::fs::read_to_string;
use std::fs::File;
use std::io::Read;

/* Function type */
pub type Fun = (Vec<Token>, bool);

/* Binary VM application */
pub type BinApp = fn(MaeelType, MaeelType) -> MaeelType;

/* Default VM function output */
pub type VMOutput<T> = Result<T, Box<dyn Error>>;

pub trait MaeelVM {
    fn push_variable(&mut self, name: String, value: MaeelType) -> VMOutput<()>;

    fn get_variable(&mut self, name: String) -> VMOutput<&MaeelType>;

    /// Get the top value from the stack (without dropping it)
    fn peek(&self) -> VMOutput<&MaeelType>;

    /// Push a value to the stack
    fn push(&mut self, value: MaeelType) -> VMOutput<()>;

    /// Get the top value from the stack (and drop it)
    fn pop(&mut self) -> VMOutput<MaeelType>;

    /// Drop the top value from the stack
    fn fastpop(&mut self) -> VMOutput<()>;

    /// Clear the stack
    fn clear(&mut self) -> VMOutput<()>;

    /// Duplicate the highest value of the stack
    fn dup(&mut self) -> VMOutput<()>;

    /// Exchange the two highests values of the stack
    fn swap(&mut self) -> VMOutput<()>;

    /// Duplicate the 2nd highest value of the stack
    fn over(&mut self) -> VMOutput<()>;

    /// Rotate the three highests values of the stack
    fn rot(&mut self) -> VMOutput<()>;

    /// Perform a binary operation
    fn binary_op(&mut self, app: BinApp) -> VMOutput<()> {
        let output = app(self.pop()?, self.pop()?);

        self.push(output)
    }

    fn parse_array(
        &mut self,
        tokens: &mut Vec<Token>,
        vars: &mut HashMap<String, MaeelType>,
    ) -> VMOutput<()> {
        let mut xs = Vec::default();

        while let Some(temporary_token) = tokens.pop() {
            match temporary_token {
                /* Stop parsing the array */
                Token::ArrayEnd => break,

                /* Parse an array inside an array */
                Token::ArrayStart => {
                    self.parse_array(tokens, vars)?;
                    xs.push(self.pop()?);
                }

                /* Push a string to the current array */
                Token::String(value) => xs.push(MaeelType::String(value)),

                /* Push a float to the current array */
                Token::Float(value) => xs.push(MaeelType::Float(value)),

                /* Push an integer to the current array */
                Token::Integer(value) => xs.push(MaeelType::Integer(value)),

                /* Push a code block to the current array */
                Token::Block(value) => xs.push(MaeelType::Function(value)),

                Token::Identifier(identifier) => match vars.get(&identifier) {
                    Some(value) => {
                        xs.push(value.clone());
                    }

                    _ => panic!(),
                },

                _ => panic!(),
            }
        }

        /* Finally, push the array on the stack */
        self.push(MaeelType::Array(xs))?;

        Ok(())
    }

    fn process_tokens<'a>(
        &mut self,
        tokens: &'a [Token],                           /* Program tokens */
        vars: &'a mut HashMap<String, MaeelType>,      /* Global vars */
        funs: &'a mut HashMap<String, Fun>,            /* Global funs */
        structs: &'a mut HashMap<String, Vec<String>>, /* Global structs */
    ) -> VMOutput<()> {
        // Parse the tokens into a stack
        let mut tokens = tokens
            .iter()
            .rev() /* Reverse the tokens */
            .cloned() /* Clone the tokens */
            .collect::<Vec<Token>>(); /* Return it as a vector */

        while let Some(token) = tokens.pop() {
            match token {
                Token::BlockEnd | Token::BlockStart | Token::ArrayEnd => panic!(),

                /* Parse arrays */
                Token::ArrayStart => self.parse_array(&mut tokens, vars)?,

                /* Perform a binary operation */
                Token::BinaryOP(app) => self.binary_op(app)?,

                /* Push a string */
                Token::String(content) => self.push(MaeelType::String(content))?,

                /* Push a float */
                Token::Float(content) => self.push(MaeelType::Float(content))?,

                /* Push an integer */
                Token::Integer(content) => self.push(MaeelType::Integer(content))?,

                /* Push an anonymous function */
                Token::Block(content) => self.push(MaeelType::Function(content))?,

                /* Access structures members */
                Token::Dot => match self.pop() {
                    Ok(MaeelType::Structure(structure)) => self.push(
                        structure
                            .get(&match tokens.pop() {
                                Some(Token::Identifier(value)) => value,

                                _ => panic!(),
                            })
                            .unwrap()
                            .clone(),
                    )?,

                    _ => panic!(),
                },

                /* Use functions as first class objects */
                Token::Colon => self.push(MaeelType::Function(
                    funs.get(&match tokens.pop() {
                        Some(Token::Identifier(value)) => value,
                        _ => panic!(),
                    })
                    .unwrap()
                    .0
                    .iter()
                    .rev()
                    .cloned()
                    .collect(),
                ))?,

                Token::Call => {
                    let fun = match self.pop() {
                        Ok(MaeelType::Function(value)) => value,

                        _ => panic!(),
                    };

                    self.process_tokens(&fun, &mut vars.clone(), funs, structs)?;
                }

                Token::Then => {
                    let temporary_token = tokens.pop();

                    match self.pop() {
                        Ok(MaeelType::Integer(1)) => match temporary_token {
                            Some(Token::Block(temporary_tokens)) => temporary_tokens
                                .iter()
                                .rev()
                                .for_each(|token| tokens.push(token.clone())),

                            Some(temporary_token) => tokens.push(temporary_token),

                            None => panic!(),
                        },

                        Ok(MaeelType::Integer(0)) => {}

                        _ => panic!(),
                    }
                }

                Token::Assignment => {
                    let name = match tokens.pop() {
                        Some(Token::Identifier(value)) => value,
                        _ => panic!(),
                    };

                    vars.insert(name, self.pop()?);
                }

                Token::Identifier(identifier) => match identifier.as_str() {
                    "print" => print!("{}", self.peek()?), /* Print the top token */

                    "for" => {
                        let temporary_tokens = match tokens.pop() {
                            Some(Token::Block(value)) => value,

                            _ => panic!(),
                        };

                        match self.pop() {
                            Ok(MaeelType::Array(xs)) => {
                                xs.iter().for_each(|x| {
                                    self.push(x.clone()).unwrap();

                                    self.process_tokens(&temporary_tokens, vars, funs, structs)
                                        .unwrap();
                                });
                            }

                            Ok(MaeelType::String(string)) => {
                                string.chars().for_each(|x| {
                                    self.push(MaeelType::String(x.to_string())).unwrap();

                                    self.process_tokens(&temporary_tokens, vars, funs, structs)
                                        .unwrap();
                                });
                            }

                            _ => panic!(),
                        }
                    }

                    "while" => {
                        let temporary_tokens = match tokens.pop() {
                            Some(Token::Block(value)) => value,

                            _ => panic!(),
                        };

                        while match self.pop() {
                            Ok(MaeelType::Integer(1)) => true,  /* Continue looping */
                            Ok(MaeelType::Integer(0)) => false, /* Stop looping */
                            _ => panic!(),                      /* No boolean on the stack */
                        } {
                            self.process_tokens(&temporary_tokens, vars, funs, structs)?
                        }
                    }

                    "struct" => {
                        let struct_name = match tokens.pop() {
                            Some(Token::Identifier(value)) => value,

                            _ => panic!(),
                        };

                        let mut struct_fields = Vec::default();

                        while let Some(temporary_tokens) = tokens.pop() {
                            match temporary_tokens {
                                Token::Dot =>
                                /* Stop parsing structure fields on '.' */
                                {
                                    break;
                                }

                                Token::Identifier(identifier) => {
                                    struct_fields.push(identifier);
                                }

                                _ => panic!(),
                            }
                        }

                        struct_fields.reverse();

                        structs.insert(struct_name, struct_fields);
                    }

                    "fun" => {
                        let mut fun_name = match tokens.pop() {
                            Some(Token::Identifier(value)) => value,

                            _ => panic!(),
                        };

                        let mut inline = false;

                        if fun_name == "inline" {
                            inline = true;

                            fun_name = match tokens.pop() {
                                Some(Token::Identifier(value)) => value,

                                _ => panic!(),
                            }
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

                        funs.insert(fun_name.clone(), (fun_tokens, inline));
                    }

                    "clear" => self.clear()?,

                    "get" => {
                        let index = match self.pop() {
                            Ok(MaeelType::Integer(value)) => value as usize,

                            _ => panic!(),
                        };

                        match self.pop() {
                            Ok(MaeelType::Array(xs)) => self.push(xs.get(index).unwrap().clone()),

                            Ok(MaeelType::String(string)) => self.push(MaeelType::String(
                                string.chars().nth(index).unwrap().to_string(),
                            )),

                            Ok(other) => panic!("{other} is not indexable!"),
                            _ => panic!("Nothing to index!"),
                        }?
                    }

                    /* Stop processing the tokens */
                    "break" => break,

                    /* Process "fastpop" VM operation */
                    "drop" => self.fastpop()?,

                    /* Process "dup" VM operation */
                    "dup" => self.dup()?,

                    /* Process "swap" VM operation */
                    "swap" => self.swap()?,

                    /* Process "over" VM operation */
                    "over" => self.over()?,

                    /* Process "rotate" VM operation */
                    "rot" => self.rot()?,

                    "read" => {
                        let bytes = match self.pop() {
                            Ok(MaeelType::Integer(value)) => value,

                            _ => panic!(),
                        };

                        let path = match self.pop() {
                            Ok(MaeelType::String(value)) => value,

                            _ => panic!(),
                        };

                        assert!(bytes >= 0);

                        let mut buf = vec![0u8; bytes as usize];

                        File::open(path)?.read_exact(&mut buf)?;

                        self.push(MaeelType::Array(
                            buf.iter()
                                .map(|byte| MaeelType::Integer(*byte as i64))
                                .collect(),
                        ))?
                    }

                    "include" => {
                        let target = match self.pop() {
                            Ok(MaeelType::String(value)) => value,

                            _ => panic!(),
                        };

                        let content = match target.as_str() {
                            "std" => include_str!("../../stdlib/std.maeel").to_string(),

                            _ => read_to_string(target).expect("Failed to include file"),
                        };

                        lex_into_tokens(&content)
                            .iter()
                            .rev()
                            .for_each(|token| tokens.push(token.clone()))
                    }

                    identifier => {
                        if let Some(value) = vars.get(identifier)
                        /* Identifier is a variable */
                        {
                            self.push(value.clone())?;

                            continue;
                        }

                        if let Some(fun) = funs.get(identifier)
                        /* Identifier is a function */
                        {
                            if fun.1
                            /* Inline function */
                            {
                                fun.0.iter().for_each(|token| tokens.push(token.clone()));
                                continue;
                            }

                            let mut fun_tokens = fun.0.clone();

                            fun_tokens.reverse();

                            self.process_tokens(&fun_tokens, &mut vars.clone(), funs, structs)?;

                            continue;
                        }

                        if let Some(fields) = structs.get(identifier)
                        /* Identifier is a structure */
                        {
                            /* Future structure */
                            let mut structure =
                                HashMap::<String, MaeelType>::with_capacity(fields.len());

                            /* Map each field to a value of the stack */
                            fields.iter().for_each(|key| {
                                structure.insert(key.clone(), self.pop().unwrap());
                            });

                            /* Finally, push the structure */
                            self.push(MaeelType::Structure(structure))?;

                            continue;
                        }

                        panic!()
                    }
                },
            };
        }

        Ok(())
    }
}
