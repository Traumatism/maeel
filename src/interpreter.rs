use crate::lexing::extract_instructions;
use crate::token::Block;
use crate::token::Token;
use crate::vmtype::VMType;

use std::collections::HashMap;
use std::slice::Iter;

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

/// Maeel interpreter
#[derive(Default)]
pub struct Interpreter {
    stop_execution: bool,
    vars: HashMap<String, VMType>,
    private_vars: HashMap<String, VMType>,
    procs: HashMap<String, Block>,
    data: Vec<VMType>,
}

impl Interpreter {
    /// Returns a new interpreter
    ///
    /// # Arguments
    ///
    /// * `procs` - A Hashmap that maps procedure names to procedure tokens
    /// * `vars` - A Hashmap thats maps variable names to values
    /// * `data` - A Vec of VMType that represent the current stack
    ///
    pub fn new(
        procs: HashMap<String, Block>,
        vars: HashMap<String, VMType>,
        data: Vec<VMType>,
    ) -> Self {
        Self {
            stop_execution: false,
            private_vars: HashMap::default(),
            procs,
            vars,
            data,
        }
    }

    /// Rotate the 3 top element
    fn rotate(&mut self) {
        if self.data.len() >= 3 {
            let third = pop!(self);
            let second = pop!(self);
            let first = pop!(self);

            push!(self, second);
            push!(self, third);
            push!(self, first);
        }
    }

    /// Duplicate the top element
    fn dup(&mut self) {
        if let Some(x) = self.data.last().cloned() {
            push!(self, x);
        }
    }

    /// Duplicate the element under the top element
    fn over(&mut self) {
        if self.data.len() >= 2 {
            let second = &self.data[self.data.len() - 2];
            push!(self, second.clone());
        }
    }

    /// Swap the 2 top elements
    fn swap(&mut self) {
        if self.data.len() >= 2 {
            let second = pop!(self);
            let first = pop!(self);

            push!(self, second);
            push!(self, first);
        }
    }

    /// Handle one instruction
    ///
    /// # Arguments
    /// `tokens` - The instruction
    ///
    pub fn handle_instruction(&mut self, tokens: &mut Iter<Token>) {
        while let Some(token) = tokens.next() {
            if self.stop_execution {
                return;
            }

            match token.clone() {
                Token::Str(content) => push!(self, content, VMType::Str),
                Token::Bool(content) => push!(self, content, VMType::Bool),
                Token::Float(content) => push!(self, content, VMType::Float),
                Token::Integer(content) => push!(self, content, VMType::Integer),
                Token::Dup => self.dup(),
                Token::Swap => self.swap(),
                Token::Over => self.over(),
                Token::Rotate => self.rotate(),
                Token::Clear => self.data.clear(),
                Token::Len => push!(self, self.data.len() as i64, VMType::Integer),
                Token::Or => binary_op!(self, |),
                Token::Sub => binary_op!(self, -),
                Token::Add => binary_op!(self, +),
                Token::Mul => binary_op!(self, *),
                Token::Div => binary_op!(self, /),
                Token::And => binary_op!(self, &),
                Token::Xor => binary_op!(self, ^),
                Token::Modulo => binary_op!(self, %),
                Token::Gt => binary_op!(self, >, VMType::Bool),
                Token::Lt => binary_op!(self, <, VMType::Bool),
                Token::Eq => binary_op!(self, ==, VMType::Bool),
                Token::Not => {
                    let p = pop!(self);
                    push!(self, !p)
                }

                Token::DivQ => {
                    let a = pop!(self);
                    let b = pop!(self);

                    let n = match (a, b) {
                        (VMType::Integer(a0), VMType::Integer(b0)) => b0 / a0,
                        _ => panic!(),
                    };

                    push!(self, n, VMType::Integer)
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

                        "print" | "println" => {
                            let element = pop!(self);

                            (match identifier.as_str() {
                                "println" => |content: String| println!("{content}"),
                                "print" => |content: String| print!("{content}"),
                                _ => panic!(),
                            })(element.to_string());

                            push!(self, element);
                        }

                        identifier => {
                            if let Some(value) = self.vars.get(identifier) {
                                push!(self, value.clone());
                            } else if let Some(value) = self.private_vars.get(identifier) {
                                push!(self, value.clone());
                            } else {
                                let proc_tokens = self.procs.get(identifier).unwrap().clone();
                                self.handle_block_execution(proc_tokens);
                            }
                        }
                    };
                }

                Token::Del => {
                    self.vars.remove(match tokens.next() {
                        Some(Token::Identifier(name)) => name,
                        _ => panic!(),
                    });
                }
                Token::Pop => {
                    pop!(self);
                }
                Token::BlockStart | Token::BlockEnd => panic!(),
                Token::Block(tokens) => self.handle_block_execution(tokens),
                Token::Let => {
                    let name = match tokens.next() {
                        Some(Token::Identifier(name)) => name.clone(),
                        _ => panic!(),
                    };

                    let value = match tokens.next() {
                        Some(Token::Str(content)) => VMType::Str(content.clone()),
                        Some(Token::Integer(n)) => VMType::Integer(*n),
                        Some(Token::Float(x)) => VMType::Float(*x),
                        Some(Token::Bool(p)) => VMType::Bool(*p),
                        Some(Token::Over) => {
                            self.over();
                            pop!(self)
                        }
                        Some(Token::Pop) => pop!(self),
                        Some(Token::Dup) => {
                            self.dup();
                            pop!(self)
                        }
                        _ => panic!(),
                    };

                    if name.starts_with('_') {
                        self.private_vars.insert(name, value);
                    } else {
                        self.vars.insert(name, value);
                    }
                }
                Token::ProcStart => {
                    let proc_name = match tokens.next().unwrap() {
                        Token::Identifier(name) => name,
                        _ => panic!(),
                    };

                    let proc_block = match tokens.next().unwrap() {
                        Token::Block(proc_tokens) => proc_tokens.clone(),
                        _ => {
                            panic!()
                        }
                    };

                    self.procs.insert(String::from(proc_name), proc_block);
                }

                Token::Return => self.stop_execution = true,

                Token::While => {
                    let while_block = match tokens.next() {
                        Some(Token::Block(while_tokens)) => while_tokens.clone(),
                        _ => panic!(),
                    };

                    while let Some(VMType::Bool(true)) = pop!(self, 1) {
                        self.handle_block_execution(while_block.clone());

                        if self.stop_execution {
                            break;
                        }
                    }
                }

                Token::For => {
                    let for_block = match tokens.next() {
                        Some(Token::Block(if_tokens)) => if_tokens.clone(),
                        _ => panic!(),
                    };

                    let array = match pop!(self, 1) {
                        Some(VMType::Array(array)) => array,
                        _ => {
                            panic!()
                        }
                    };

                    for element in array {
                        match element {
                            VMType::Str(content) => push!(self, content, VMType::Str),
                            VMType::Integer(content) => push!(self, content, VMType::Integer),
                            VMType::Float(content) => push!(self, content, VMType::Float),
                            VMType::Bool(content) => push!(self, content, VMType::Bool),
                            _ => panic!(),
                        }

                        self.handle_block_execution(for_block.clone());

                        if self.stop_execution {
                            break;
                        }
                    }
                }

                Token::If => {
                    let block = match tokens.next() {
                        Some(Token::Block(if_tokens)) => if_tokens.clone(),
                        _ => panic!(),
                    };

                    if match pop!(self, 1) {
                        Some(VMType::Bool(e)) => e,
                        _ => panic!(),
                    } {
                        self.handle_block_execution(block)
                    }
                }
            }
        }
    }

    pub fn handle_block_execution(&mut self, block: Block) {
        let mut proc_parser = Self::new(self.procs.clone(), self.vars.clone(), self.data.clone());

        for instruction in extract_instructions(block) {
            proc_parser.handle_instruction(&mut instruction.iter())
        }

        self.procs = proc_parser.procs;
        self.data = proc_parser.data;
        self.vars = proc_parser.vars;
    }
}
