use std::collections::BTreeMap;
use std::io::Write;
use std::slice::Iter;

use crate::enums::token::Token;
use crate::enums::vmtype::VMType;
use crate::lexer::extract_instructions;
use crate::structures::stack::Stack;

pub struct Interpreter {
    stop_execution: bool,
    procs: BTreeMap<String, Vec<Token>>,
    vars: BTreeMap<String, VMType>,
    stack: Stack,
}

#[allow(clippy::derivable_impls)]
impl Default for Interpreter {
    fn default() -> Self {
        Self {
            stop_execution: false,
            procs: BTreeMap::default(),
            vars: BTreeMap::default(),
            stack: Stack::default(),
        }
    }
}

impl Interpreter {
    pub fn new(
        procs: BTreeMap<String, Vec<Token>>,
        vars: BTreeMap<String, VMType>,
        stack: Stack,
    ) -> Self {
        Self {
            stop_execution: false,
            procs,
            vars,
            stack,
        }
    }

    /// Handle a single instruction
    pub fn handle_instruction(&mut self, tokens: &mut Iter<Token>) {
        while let Some(token) = tokens.next() {
            if self.stop_execution {
                return;
            }

            match token.clone() {
                Token::BlockStart | Token::BlockEnd => panic!(),
                Token::Block(tokens) => self.handle_block_execution(tokens),

                Token::Str(content) => self.handle_push_str(content),
                Token::Integer(content) => self.handle_push_int(content),
                Token::Float(content) => self.handle_push_float(content),
                Token::Bool(content) => self.handle_push_bool(content),
                Token::Add => self.handle_add(),
                Token::Sub => self.handle_sub(),
                Token::Mul => self.handle_mul(),
                Token::Div => self.handle_div(),
                Token::Modulo => self.handle_modulo(),
                Token::Or => self.handle_or(),
                Token::Xor => self.handle_xor(),
                Token::And => self.handle_and(),
                Token::Not => self.handle_not(),
                Token::Eq => self.handle_eq(),
                Token::Take => self.handle_take(),
                Token::Reverse => self.handle_reverse(),
                Token::Identifier(identifier) => self.handle_identifier(&identifier),
                Token::Dup => self.stack.dup(),
                Token::Swap => self.stack.swap(),
                Token::Clear => self.stack.clear(),
                Token::Pop => self.stack.pop_nr(),

                Token::Return => self.stop_execution = true,
                Token::Let => {
                    self.vars.insert(
                        match tokens.next() {
                            Some(Token::Identifier(name)) => name.clone(),
                            _ => panic!(),
                        },
                        match tokens.next() {
                            Some(Token::Str(content)) => VMType::Str(content.clone()),
                            Some(Token::Integer(n)) => VMType::Integer(*n),
                            Some(Token::Float(x)) => VMType::Float(*x),
                            Some(Token::Bool(p)) => VMType::Bool(*p),

                            Some(Token::Pop) => self.stack.pop().unwrap(),
                            Some(Token::Dup) => {
                                self.stack.dup();
                                self.stack.pop().unwrap()
                            }
                            _ => panic!(),
                        },
                    );
                }

                Token::For => {
                    let array = match self.stack.pop() {
                        Some(VMType::Array(array)) => array,
                        _ => {
                            panic!()
                        }
                    };

                    let for_block = match tokens.next() {
                        Some(Token::Block(if_tokens)) => if_tokens.clone(),
                        _ => panic!(),
                    };

                    for element in array {
                        match element {
                            VMType::Str(c) => self.handle_push_str(c),
                            VMType::Integer(n) => self.handle_push_int(n),
                            VMType::Float(x) => self.handle_push_float(x),
                            VMType::Bool(p) => self.handle_push_bool(p),
                            _ => panic!(),
                        }

                        self.handle_block_execution(for_block.clone())
                    }
                }

                Token::If => {
                    let block = match tokens.next() {
                        Some(Token::Block(if_tokens)) => if_tokens.clone(),
                        _ => panic!(),
                    };

                    if match self.stack.pop() {
                        Some(VMType::Bool(e)) => e,
                        _ => panic!(),
                    } {
                        self.handle_block_execution(block)
                    }
                }

                Token::Del => {
                    self.vars.remove(match tokens.next() {
                        Some(Token::Identifier(name)) => name,
                        _ => panic!(),
                    });
                }

                Token::ProcStart => {
                    self.procs.insert(
                        match tokens.next().unwrap() {
                            Token::Identifier(name) => name,
                            _ => panic!(),
                        }
                        .to_string(),
                        match tokens.next().unwrap() {
                            Token::Block(proc_tokens) => proc_tokens.clone(),
                            _ => panic!(),
                        },
                    );
                }
            }
        }
    }

    pub fn handle_push_str(&mut self, content: String) {
        self.stack.push(VMType::Str(content));
    }

    pub fn handle_push_int(&mut self, content: i64) {
        self.stack.push(VMType::Integer(content));
    }

    pub fn handle_push_float(&mut self, content: f64) {
        self.stack.push(VMType::Float(content));
    }

    pub fn handle_push_bool(&mut self, content: bool) {
        self.stack.push(VMType::Bool(content));
    }

    pub fn handle_take(&mut self) {
        match self.stack.pop() {
            Some(VMType::Integer(n)) => {
                let array = (0..n).map(|_| self.stack.pop().unwrap()).collect();
                self.stack.push(VMType::Array(array))
            }
            _ => panic!(),
        }
    }

    pub fn handle_reverse(&mut self) {
        match self.stack.pop() {
            Some(VMType::Array(mut array)) => {
                array.reverse();
                self.stack.push(VMType::Array(array))
            }
            _ => {
                panic!()
            }
        }
    }

    pub fn handle_block_execution(&mut self, block: Vec<Token>) {
        let mut proc_parser = Self::new(self.procs.clone(), self.vars.clone(), self.stack.clone());

        for instruction in extract_instructions(block) {
            proc_parser.handle_instruction(&mut instruction.iter())
        }

        self.stop_execution = proc_parser.stop_execution;
        self.procs = proc_parser.procs;
        self.stack = proc_parser.stack;
        self.vars = proc_parser.vars;
    }

    pub fn handle_identifier(&mut self, identifier: &str) {
        match identifier {
            "exit" => match self.stack.pop().unwrap() {
                VMType::Integer(status) => std::process::exit(status as i32),
                _ => panic!(),
            },

            // Parse a print/println instruction
            //
            // Print stack head to stdout
            "print" | "println" => {
                let e = self.stack.pop().unwrap_or_else(|| panic!());

                (match identifier {
                    "println" => |content: String| {
                        std::io::stdout()
                            .write_all((content + "\n").as_bytes())
                            .unwrap()
                    },
                    "print" => {
                        |content: String| std::io::stdout().write_all(content.as_bytes()).unwrap()
                    }
                    _ => panic!(),
                })(e.to_string());

                self.stack.push(e);
            }

            identifier => {
                let v = self.vars.get(identifier);

                if let Some(value) = v {
                    return self.stack.push(value.clone());
                }

                let proc_tokens = self.procs.get(identifier).expect(identifier).clone();
                self.handle_block_execution(proc_tokens);
            }
        }
    }

    pub fn handle_modulo(&mut self) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(b % a)
    }

    pub fn handle_add(&mut self) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(a + b)
    }

    pub fn handle_mul(&mut self) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(a * b)
    }

    pub fn handle_sub(&mut self) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(b - a)
    }

    pub fn handle_or(&mut self) {
        let p = self.stack.pop().unwrap();
        let q = self.stack.pop().unwrap();

        self.stack.push(p | q)
    }

    pub fn handle_xor(&mut self) {
        let p = self.stack.pop().unwrap();
        let q = self.stack.pop().unwrap();

        self.stack.push(p ^ q)
    }

    pub fn handle_and(&mut self) {
        let p = self.stack.pop().unwrap();
        let q = self.stack.pop().unwrap();

        self.stack.push(p & q)
    }

    pub fn handle_not(&mut self) {
        let p = self.stack.pop().unwrap();

        self.stack.push(!p)
    }

    pub fn handle_eq(&mut self) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(VMType::Bool(a == b))
    }

    pub fn handle_div(&mut self) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(b / a)
    }
}
