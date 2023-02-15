use crate::lexing::extract_instructions;
use crate::token::Block;
use crate::token::Token;
use crate::vmtype::VMType;

use std::collections::HashMap;
use std::io::Write;
use std::slice::Iter;

macro_rules! push {
    ($self:ident, $vmtype:expr, $content:expr) => {
        $self.data.push($vmtype($content))
    };
}

macro_rules! binary_op {
    ($self:ident, $operator:tt) => {{
        let a = $self.data.pop().unwrap();
        let b = $self.data.pop().unwrap();

        $self.data.push(b $operator a);
    }};

    ($self:ident, $operator:tt, $vmtype:expr) => {{
        let a = $self.data.pop().unwrap();
        let b = $self.data.pop().unwrap();

        $self.data.push($vmtype(b $operator a))
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
            let third = self.data.pop().unwrap();
            let second = self.data.pop().unwrap();
            let first = self.data.pop().unwrap();

            self.data.push(second);
            self.data.push(third);
            self.data.push(first);
        }
    }

    /// Duplicate the top element
    fn dup(&mut self) {
        if let Some(x) = self.data.last() {
            self.data.push(x.clone());
        }
    }

    /// Duplicate the element under the top element
    fn over(&mut self) {
        if self.data.len() >= 2 {
            let second = &self.data[self.data.len() - 2];
            self.data.push(second.clone());
        }
    }

    /// Swap the 2 top elements
    fn swap(&mut self) {
        if self.data.len() >= 2 {
            let second = self.data.pop().unwrap();
            let first = self.data.pop().unwrap();
            self.data.push(second);
            self.data.push(first);
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
                Token::Dup(_) => self.dup(),
                Token::Swap(_) => self.swap(),
                Token::Over(_) => self.over(),
                Token::Rotate(_) => self.rotate(),
                Token::Clear(_) => self.data.clear(),

                Token::Len(_) => push!(self, VMType::Integer, self.data.len() as i64),
                Token::Str(content, _) => push!(self, VMType::Str, content),
                Token::Bool(content, _) => push!(self, VMType::Bool, content),
                Token::Float(content, _) => push!(self, VMType::Float, content),
                Token::Integer(content, _) => push!(self, VMType::Integer, content),

                Token::Or(_) => binary_op!(self, |),
                Token::Sub(_) => binary_op!(self, -),
                Token::Add(_) => binary_op!(self, +),
                Token::Mul(_) => binary_op!(self, *),
                Token::Div(_) => binary_op!(self, /),
                Token::And(_) => binary_op!(self, &),
                Token::Xor(_) => binary_op!(self, ^),
                Token::Gt(_) => binary_op!(self, >, VMType::Bool),
                Token::Lt(_) => binary_op!(self, <, VMType::Bool),
                Token::Eq(_) => binary_op!(self, ==, VMType::Bool),
                Token::Modulo(_) => binary_op!(self, %),

                Token::Not(_) => {
                    let p = self.data.pop().unwrap();

                    self.data.push(!p)
                }
                Token::DivQ(_) => {
                    let a = self.data.pop().unwrap();
                    let b = self.data.pop().unwrap();

                    let n = match (a, b) {
                        (VMType::Integer(a0), VMType::Integer(b0)) => b0 / a0,
                        _ => panic!(),
                    };

                    push!(self, VMType::Integer, n)
                }
                Token::Take(line) => match self.data.pop() {
                    Some(VMType::Integer(n)) => {
                        let array = (0..n).map(|_| self.data.pop().unwrap()).collect();
                        push!(self, VMType::Array, array)
                    }
                    _ => panic!("line {line}: `take` requires an array on the take."),
                },
                Token::Identifier(identifier, line) => {
                    match identifier.as_str() {
                        "exit" => match self.data.pop().unwrap() {
                            VMType::Integer(status) => std::process::exit(status as i32),
                            _ => {
                                panic!("line {line}: exit requires an integer (the exit status) on the stack.")
                            }
                        },

                        "print" | "println" => {
                            let e = self.data.pop().unwrap_or_else(|| panic!());

                            (match identifier.as_str() {
                                "println" => |content: String| {
                                    std::io::stdout()
                                        .write_all((content + "\n").as_bytes())
                                        .unwrap()
                                },
                                "print" => |content: String| {
                                    std::io::stdout().write_all(content.as_bytes()).unwrap()
                                },

                                _ => panic!(),
                            })(e.to_string());

                            self.data.push(e);
                        }

                        identifier => {
                            if let Some(value) = self.vars.get(identifier) {
                                return self.data.push(value.clone());
                            }

                            if let Some(value) = self.private_vars.get(identifier) {
                                return self.data.push(value.clone());
                            }

                            let proc_tokens = self.procs.get(identifier).expect(identifier).clone();
                            self.handle_block_execution(proc_tokens, line);
                        }
                    };
                }

                Token::Del(line) => {
                    self.vars.remove(match tokens.next() {
                        Some(Token::Identifier(name, _)) => name,
                        _ => panic!("line {line}: `del` requires a value on the stack."),
                    });
                }
                Token::Pop(_) => {
                    self.data.pop().unwrap();
                }
                Token::BlockStart(_) | Token::BlockEnd(_) => panic!(),
                Token::Block(tokens, line) => self.handle_block_execution(tokens, line),
                Token::Let(line) => {
                    let name = match tokens.next() {
                        Some(Token::Identifier(name, _)) => name.clone(),
                        _ => panic!("line {line}: `let` must be followed by a variable name."),
                    };

                    let value = match tokens.next() {
                            Some(Token::Str(content, _)) => VMType::Str(content.clone()),
                            Some(Token::Integer(n, _)) => VMType::Integer(*n),
                            Some(Token::Float(x, _)) => VMType::Float(*x),
                            Some(Token::Bool(p, _)) => VMType::Bool(*p),
                            Some(Token::Over(_)) => {
                                self.over();
                                self.data.pop().unwrap()
                            },
                            Some(Token::Pop(_)) => self.data.pop().unwrap(),
                            Some(Token::Dup(_)) => {
                                self.dup();
                                self.data.pop().unwrap()
                            }
                            _ => panic!("line {line}: `let name` must be followed by a variable value (str, integer, float, bool, `dup`, `over`, `pop`)"),
                        };

                    if name.starts_with('_') {
                        self.private_vars.insert(name, value);
                    } else {
                        self.vars.insert(name, value);
                    }
                }
                Token::ProcStart(line) => {
                    let proc_name = match tokens.next().unwrap() {
                        Token::Identifier(name, _) => name,
                        _ => panic!("line {line}: `proc` must be followed by the procedure name."),
                    };

                    let proc_block = match tokens.next().unwrap() {
                        Token::Block(proc_tokens, _) => proc_tokens.clone(),
                        _ => {
                            panic!("line {line}: `proc name` must be followed by a code block.")
                        }
                    };

                    self.procs.insert(String::from(proc_name), proc_block);
                }
                Token::Return(_) => self.stop_execution = true,
                Token::While(line) => {
                    let while_block = match tokens.next() {
                        Some(Token::Block(if_tokens, _)) => if_tokens.clone(),
                        _ => panic!("line {line}: `while` must be followed by a code block."),
                    };

                    while let Some(VMType::Bool(true)) = self.data.pop() {
                        self.handle_block_execution(while_block.clone(), line);

                        if self.stop_execution {
                            break;
                        }
                    }
                }
                Token::For(line) => {
                    let for_block = match tokens.next() {
                        Some(Token::Block(if_tokens, _)) => if_tokens.clone(),
                        _ => panic!("line {line}: `for` must be followed by a code block."),
                    };

                    let array = match self.data.pop() {
                        Some(VMType::Array(array)) => array,
                        _ => {
                            panic!("line {line}: `for` requires an array on the stack.")
                        }
                    };

                    for element in array {
                        match element {
                            VMType::Str(content) => push!(self, VMType::Str, content),
                            VMType::Integer(content) => push!(self, VMType::Integer, content),
                            VMType::Float(content) => push!(self, VMType::Float, content),
                            VMType::Bool(content) => push!(self, VMType::Bool, content),
                            _ => panic!(
                                "line {line}: `for` array content can be: (str, int, float, bool)."
                            ),
                        }

                        self.handle_block_execution(for_block.clone(), line);

                        if self.stop_execution {
                            break;
                        }
                    }
                }

                Token::If(line) => {
                    let block = match tokens.next() {
                        Some(Token::Block(if_tokens, _)) => if_tokens.clone(),
                        _ => panic!("line {line}: `if` must be followed by a code block."),
                    };

                    if match self.data.pop() {
                        Some(VMType::Bool(e)) => e,
                        _ => panic!("line {line}: `if` requires a boolean value on the stack."),
                    } {
                        self.handle_block_execution(block, line)
                    }
                }
            }
        }
    }

    pub fn handle_block_execution(&mut self, block: Block, _line: u16) {
        let mut proc_parser = Self::new(self.procs.clone(), self.vars.clone(), self.data.clone());

        for instruction in extract_instructions(block) {
            proc_parser.handle_instruction(&mut instruction.iter())
        }

        self.procs = proc_parser.procs;
        self.data = proc_parser.data;
        self.vars = proc_parser.vars;
    }
}
