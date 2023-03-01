use maeel_common::tokens::TokenData;

use maeel_common::tokens::Token;
use maeel_common::types::Type;
use maeel_lexer::extract_instructions;

use std::collections::HashMap;
use std::slice::Iter;

macro_rules! next {
    ($tokens:expr, "identifier") => {{
        let next = $tokens.next().unwrap();

        match &next.token {
            Token::Identifier(value) => value.clone(),
            token => panic!("Expected identifier, got {:?}", token),
        }
    }};

    ($tokens:expr, "block") => {{
        let next = $tokens.next().unwrap();

        match &next.token {
            Token::Block(block) => block.to_vec(),
            token => panic!("Expected block, got {:?}", token),
        }
    }};
}

macro_rules! pop {
    ($self:ident) => {
        $self.data.pop().unwrap()
    };

    ($self:ident, $no_unwrap:expr) => {
        $self.data.pop()
    };
}

macro_rules! push {
    ($self:ident, $content:expr) => {
        $self.data.push($content)
    };

    ($self:ident, $content:expr, $vmtype:expr) => {
        $self.data.push($vmtype($content))
    };
}

macro_rules! binary_op {
    ($self:ident, $operator:tt) => {{
        let a = pop!($self);
        let b = pop!($self);

        push!($self, b $operator a);
    }};
}

macro_rules! run_block {
    ($self:ident, $block:ident) => {
        for instruction in extract_instructions($block.clone()) {
            $self.handle_instruction(&mut instruction.iter())
        }
    };
}

#[derive(Default)]
pub struct TypingInterpreter {
    data: Vec<Type>,
    vars: HashMap<String, Type>,
    procs: HashMap<String, Vec<TokenData>>,
    stop_execution: bool,
}

impl TypingInterpreter {
    pub fn handle_instruction(&mut self, tokens: &mut Iter<TokenData>) {
        while let Some(token_data) = tokens.next() {
            if self.stop_execution {
                return;
            }

            let token = token_data.token.clone();

            match token.clone() {
                Token::Include => {}
                Token::Return => self.stop_execution = true,
                Token::Clear => self.data.clear(),
                Token::BlockStart | Token::BlockEnd => panic!(),

                Token::Str(_) => push!(self, Type::Str),
                Token::Bool(_) => push!(self, Type::Bool),
                Token::Float(_) => push!(self, Type::Float),
                Token::Integer(_) => push!(self, Type::Integer),

                Token::Dup => push!(self, self.data.last().cloned().unwrap()),

                Token::Rot => {
                    let third = pop!(self);
                    let second = pop!(self);
                    let first = pop!(self);

                    push!(self, second);
                    push!(self, third);
                    push!(self, first);
                }

                Token::Swap => {
                    let second = pop!(self);
                    let first = pop!(self);

                    push!(self, second);
                    push!(self, first);
                }

                Token::Over => {
                    push!(self, self.data[self.data.len() - 2].to_owned());
                }

                Token::Sub => binary_op!(self, -),
                Token::Add => binary_op!(self, +),
                Token::Mul => binary_op!(self, *),
                Token::Div => binary_op!(self, /),
                Token::Mod => binary_op!(self, %),

                Token::Gt => push!(self, Type::Bool),
                Token::Lt => push!(self, Type::Bool),
                Token::Eq => push!(self, Type::Bool),

                Token::Block(tokens) => run_block!(self, tokens),

                Token::Pop => {
                    pop!(self);
                }

                Token::Not => {
                    let p = pop!(self);
                    push!(self, !p);
                }

                Token::ProcStart => {
                    self.procs
                        .insert(next!(tokens, "identifier"), next!(tokens, "block"));
                }

                Token::Take => match pop!(self, 1) {
                    Some(Type::Integer) => {
                        push!(self, Type::Array)
                    }
                    _ => panic!(),
                },

                Token::Identifier(identifier) => {
                    match identifier.as_str() {
                        "ptr" => {
                            let element = pop!(self);

                            let new_element = match element {
                                Type::Str => Type::StrPointer,
                                Type::Integer => Type::IntPointer,
                                _ => panic!(),
                            };

                            push!(self, new_element);
                        }

                        "syscall" => {}

                        "println" | "print" => {}

                        identifier => {
                            if let Some(value) = self.vars.get(identifier) {
                                push!(self, value.clone());
                            } else {
                                let proc_tokens = self.procs.get(identifier).unwrap().clone();
                                run_block!(self, proc_tokens);
                            }
                        }
                    };
                }

                Token::Let => {
                    let name = next!(tokens, "identifier");
                    let next = tokens.next().unwrap();

                    let value = match &next.token {
                        Token::Str(_) => Type::Str,
                        Token::Float(_) => Type::Float,
                        Token::Bool(_) => Type::Bool,
                        Token::Integer(_) => Type::Integer,
                        Token::Over => self.data[self.data.len() - 2].clone(),
                        Token::Dup => self.data.last().cloned().unwrap(),
                        Token::Pop => pop!(self),
                        _ => panic!(),
                    };

                    self.vars.insert(name, value);
                }

                Token::While => {
                    let block = next!(tokens, "block");

                    let Some(Type::Bool) = pop!(self, 1) else {
                        panic!()
                    };

                    run_block!(self, block)
                }

                Token::For => {
                    let block = next!(tokens, "block");

                    let Some(Type::Array) = pop!(self, 1) else {
                        panic!()
                    };

                    run_block!(self, block);
                }

                Token::If => {
                    let block = next!(tokens, "block");

                    let Some(Type::Bool) = pop!(self, 1) else {
                        panic!()
                    };

                    run_block!(self, block);
                }
            }
        }
    }
}

pub fn check(tokens: Vec<Vec<TokenData>>) {
    let mut interpreter = TypingInterpreter::default();

    tokens
        .iter()
        .for_each(|instruction| interpreter.handle_instruction(&mut instruction.iter()));
}
