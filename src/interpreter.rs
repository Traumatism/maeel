use crate::lexer::extract_instructions;
use crate::token::Token;
use crate::vmtype::VMType;

use std::collections::HashMap;
use std::slice::Iter;

macro_rules! next {
    ($tokens:expr, "identifier") => {
        match $tokens.next() {
            Some(Token::Identifier(value)) => value.clone(),
            _ => panic!(),
        }
    };

    ($tokens:expr, "block") => {
        match $tokens.next() {
            Some(Token::Block(block)) => block.clone(),
            _ => panic!(),
        }
    };
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

    ($self:ident, $operator:tt, $vmtype:expr) => {{
        let a = pop!($self);
        let b = pop!($self);

        push!($self, b $operator a, $vmtype)
    }};
}

macro_rules! run_block {
    ($self:ident, $block:ident) => {
        for instruction in extract_instructions($block.clone()) {
            $self.handle_instruction(&mut instruction.iter())
        }
    };
}

/// Maeel interpreter
#[derive(Default)]
pub struct Interpreter {
    data: Vec<VMType>,
    vars: HashMap<String, VMType>,
    procs: HashMap<String, Vec<Token>>,
    stop_execution: bool,
}

impl Interpreter {
    pub fn handle_instruction(&mut self, tokens: &mut Iter<Token>) {
        while let Some(token) = tokens.next() {
            if self.stop_execution {
                return;
            }

            match token.clone() {
                Token::Return => self.stop_execution = true,
                Token::Clear => self.data.clear(),
                Token::BlockStart | Token::BlockEnd => panic!(),
                Token::Str(content) => push!(self, content, VMType::Str),
                Token::Bool(content) => push!(self, content, VMType::Bool),
                Token::Float(content) => push!(self, content, VMType::Float),
                Token::Integer(content) => push!(self, content, VMType::Integer),
                Token::Dup => push!(self, self.data.last().cloned().unwrap()),
                Token::Swap => {
                    let second = pop!(self);
                    let first = pop!(self);

                    push!(self, second);
                    push!(self, first);
                }
                Token::Over => {
                    push!(self, self.data[self.data.len() - 2].clone());
                }

                Token::Sub => binary_op!(self, -),
                Token::Add => binary_op!(self, +),
                Token::Mul => binary_op!(self, *),
                Token::Div => binary_op!(self, /),
                Token::Modulo => binary_op!(self, %),
                Token::Gt => binary_op!(self, >, VMType::Bool),
                Token::Lt => binary_op!(self, <, VMType::Bool),
                Token::Eq => binary_op!(self, ==, VMType::Bool),

                Token::Del => {
                    self.vars.remove(&next!(tokens, "identifier"));
                }

                Token::Pop => {
                    pop!(self);
                }

                Token::Block(tokens) => {
                    run_block!(self, tokens);
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
                    Some(VMType::Integer(n)) => {
                        let array = (0..n).map(|_| pop!(self)).collect();
                        push!(self, array, VMType::Array)
                    }
                    _ => panic!(),
                },

                Token::Identifier(identifier) => {
                    match identifier.as_str() {
                        "exit" => match pop!(self) {
                            VMType::Integer(status) => std::process::exit(status as i32),
                            _ => {
                                panic!()
                            }
                        },

                        "println" => {
                            let element = pop!(self);
                            println!("{}", element.to_string());
                            push!(self, element);
                        }

                        "print" => {
                            let element = pop!(self);
                            print!("{}", element.to_string());
                            push!(self, element);
                        }

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

                    let value = match tokens.next() {
                        Some(Token::Str(content)) => VMType::Str(content.clone()),
                        Some(Token::Integer(n)) => VMType::Integer(*n),
                        Some(Token::Float(x)) => VMType::Float(*x),
                        Some(Token::Bool(p)) => VMType::Bool(*p),
                        Some(Token::Over) => self.data[self.data.len() - 2].clone(),
                        Some(Token::Dup) => self.data.last().cloned().unwrap(),
                        Some(Token::Pop) => pop!(self),
                        _ => panic!(),
                    };

                    self.vars.insert(name, value);
                }

                Token::While => {
                    let while_block = next!(tokens, "block");

                    while let VMType::Bool(true) = pop!(self) {
                        run_block!(self, while_block);

                        if self.stop_execution {
                            break;
                        }
                    }
                }

                Token::For => {
                    let for_block = next!(tokens, "block");

                    let array = match pop!(self, 1) {
                        Some(VMType::Array(array)) => array,
                        _ => {
                            panic!()
                        }
                    };

                    for element in array {
                        push!(self, element);
                        run_block!(self, for_block);

                        if self.stop_execution {
                            break;
                        }
                    }
                }

                Token::If => {
                    let if_block = next!(tokens, "block");
                    if match pop!(self, 1) {
                        Some(VMType::Bool(e)) => e,
                        _ => panic!(),
                    } {
                        run_block!(self, if_block);
                    }
                }
            }
        }
    }
}
