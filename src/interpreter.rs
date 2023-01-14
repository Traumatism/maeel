use std::collections::HashMap;
use std::io::Write;
use std::slice::Iter;

use crate::lexing::extract_instructions;
use crate::enums::token::Token;
use crate::enums::vmtype::VMType;
use crate::stack::Stack;


type Block = Vec<Token>;

#[derive(Default)]
pub struct Interpreter {
    stop_execution: bool,
    vars: HashMap<String, VMType>,
    private_vars: HashMap<String, VMType>,
    procs: HashMap<String, Block>,
    stack: Stack,
}


impl Interpreter {
    pub fn new(
        procs: HashMap<String, Block>,
        vars: HashMap<String, VMType>,
        stack: Stack,
    ) -> Self {
        Self {
            stop_execution: false,
            private_vars: HashMap::default(),
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
                Token::BlockStart(_) | Token::BlockEnd(_) => panic!(),
                Token::Block(tokens, line) => self.handle_block_execution(tokens, line),
                Token::Str(content, line) => self.handle_push_str(content, line),
                Token::Integer(content, line) => self.handle_push_int(content, line),
                Token::Float(content, line) => self.handle_push_float(content, line),
                Token::Bool(content, line) => self.handle_push_bool(content, line),
                Token::Add(line) => self.handle_add(line),
                Token::Sub(line) => self.handle_sub(line),
                Token::Mul(line) => self.handle_mul(line),
                Token::Div(line) => self.handle_div(line),
                Token::DivQ(line) => self.handle_divq(line),
                Token::Modulo(line) => self.handle_modulo(line),
                Token::Lt(line) => self.handle_lt(line),
                Token::Gt(line) => self.handle_gt(line),
                Token::Or(line) => self.handle_or(line),
                Token::Xor(line) => self.handle_xor(line),
                Token::And(line) => self.handle_and(line),
                Token::Not(line) => self.handle_not(line),
                Token::Eq(line) => self.handle_eq(line),
                Token::Take(line) => self.handle_take(line),
                Token::Rotate(line) => self.handle_rotate(line),
                Token::Len(line) => self.handle_len(line),
                Token::Identifier(identifier, line) => self.handle_identifier(&identifier, line),
                Token::Over(_) => self.stack.over(),
                Token::Dup(_) => self.stack.dup(),
                Token::Swap(_) => self.stack.swap(),
                Token::Clear(_) => self.stack.clear(),
                Token::Return(_) => self.stop_execution = true,
                Token::Pop(_) => {
                    self.stack.pop().unwrap();
                }
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
                                self.stack.over();
                                self.stack.pop().unwrap()
                            } 
                            Some(Token::Pop(_)) => self.stack.pop().unwrap(),
                            Some(Token::Dup(_)) => {
                                self.stack.dup();
                                self.stack.pop().unwrap()
                            }
                            _ => panic!("line {line}: `let name` must be followed by a variable value (str, integer, float, bool, `dup`, `over`, `pop`)"),
                        };

                    if name.starts_with('_') {
                        self.private_vars.insert(name, value);
                    } else {
                        self.vars.insert(name, value);
                    }
                }

                Token::While(line) => {
                    let while_block = match tokens.next() {
                        Some(Token::Block(if_tokens, _)) => if_tokens.clone(),
                        _ => panic!("line {line}: `while` must be followed by a code block."),
                    };

                    while let Some(VMType::Bool(true)) = self.stack.pop() {
                        self.handle_block_execution(while_block.clone(), line);

                        if self.stop_execution {
                            break
                        }
                    }
                }

                Token::For(line) => {
                    let for_block = match tokens.next() {
                        Some(Token::Block(if_tokens, _)) => if_tokens.clone(),
                        _ => panic!("line {line}: `for` must be followed by a code block."),
                    };

                    let array = match self.stack.pop() {
                        Some(VMType::Array(array)) => array,
                        _ => {
                            panic!("line {line}: `for` requires an array on the stack.")
                        }
                    };

                    for element in array {
                        match element {
                            VMType::Str(c) => self.handle_push_str(c, line),
                            VMType::Integer(n) => self.handle_push_int(n, line),
                            VMType::Float(x) => self.handle_push_float(x, line),
                            VMType::Bool(p) => self.handle_push_bool(p, line),
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

                    if match self.stack.pop() {
                        Some(VMType::Bool(e)) => e,
                        _ => panic!("line {line}: `if` requires a boolean value on the stack."),
                    } {
                        self.handle_block_execution(block, line)
                    }
                }

                Token::Del(line) => {
                    self.vars.remove(match tokens.next() {
                        Some(Token::Identifier(name, _)) => name,
                        _ => panic!("line {line}: `del` requires a value on the stack."),
                    });
                }

                Token::ProcStart(line) => {
                    let proc_name = match tokens.next().unwrap() {
                        Token::Identifier(name, _) => name,
                        _ => panic!(
                            "line {line}: `proc` must be followed by the procedure name."
                        ),
                    };

                    let proc_block = match tokens.next().unwrap() {
                        Token::Block(proc_tokens, _) => proc_tokens.clone(),
                        _ => {
                            panic!("line {line}: `proc name` must be followed by a code block.")
                        }
                    };

                    self.procs.insert(
                        String::from(proc_name), proc_block,
                    );
                }
            }
        }
    }

    pub fn handle_push_str(&mut self, content: String, _line: u16) {
        self.stack.push(VMType::Str(content));
    }

    pub fn handle_push_int(&mut self, content: i64, _line: u16) {
        self.stack.push(VMType::Integer(content));
    }

    pub fn handle_push_float(&mut self, content: f64, _line: u16) {
        self.stack.push(VMType::Float(content));
    }

    pub fn handle_push_bool(&mut self, content: bool, _line: u16) {
        self.stack.push(VMType::Bool(content));
    }

    pub fn handle_len(&mut self, _line: u16) {
        self.stack.push(VMType::Integer(self.stack.size() as i64))
    }

    pub fn handle_take(&mut self, line: u16) {
        match self.stack.pop() {
            Some(VMType::Integer(n)) => {
                let array = (0..n).map(|_| self.stack.pop().unwrap()).collect();
                self.stack.push(VMType::Array(array))
            }
            _ => panic!("line {line}: `take` requires an array on the take."),
        }
    }

    pub fn handle_rotate(&mut self, _line: u16) {
        self.stack.rotate()
    }

    pub fn handle_block_execution(&mut self, block: Block, _line: u16) {
        let mut proc_parser = Self::new(self.procs.clone(), self.vars.clone(), self.stack.clone());

        for instruction in extract_instructions(block) {
            proc_parser.handle_instruction(&mut instruction.iter())
        }

        self.procs = proc_parser.procs;
        self.stack = proc_parser.stack;
        self.vars = proc_parser.vars;
    }

    pub fn handle_identifier(&mut self, identifier: &str, line: u16) {
        match identifier {
            "exit" => match self.stack.pop().unwrap() {
                VMType::Integer(status) => std::process::exit(status as i32),
                _ => {
                    panic!("line {line}: exit requires an integer (the exit status) on the stack.")
                }
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

                if let Some(value) = self.vars.get(identifier) {
                    return self.stack.push(value.clone());
                }

                if let Some(value) = self.private_vars.get(identifier) {
                    return self.stack.push(value.clone());
                }

                let proc_tokens = self.procs.get(identifier).expect(identifier).clone();
                self.handle_block_execution(proc_tokens, line);
            }
        }
    }

    pub fn handle_modulo(&mut self, _line: u16) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(b % a)
    }

    pub fn handle_add(&mut self, _line: u16) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(a + b)
    }

    pub fn handle_gt(&mut self, _line: u16) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(VMType::Bool(b > a))
    }

    pub fn handle_lt(&mut self, _line: u16) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(VMType::Bool(b < a))
    }

    pub fn handle_mul(&mut self, _line: u16) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(a * b)
    }

    pub fn handle_sub(&mut self, _line: u16) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(b - a)
    }

    pub fn handle_or(&mut self, _line: u16) {
        let p = self.stack.pop().unwrap();
        let q = self.stack.pop().unwrap();

        self.stack.push(p | q)
    }

    pub fn handle_xor(&mut self, _line: u16) {
        let p = self.stack.pop().unwrap();
        let q = self.stack.pop().unwrap();

        self.stack.push(p ^ q)
    }

    pub fn handle_and(&mut self, _line: u16) {
        let p = self.stack.pop().unwrap();
        let q = self.stack.pop().unwrap();

        self.stack.push(p & q)
    }

    pub fn handle_not(&mut self, _line: u16) {
        let p = self.stack.pop().unwrap();

        self.stack.push(!p)
    }

    pub fn handle_eq(&mut self, _line: u16) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(VMType::Bool(a == b))
    }

    pub fn handle_divq(&mut self, _line: u16) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        let n = match (a, b) {
            (VMType::Integer(a0), VMType::Integer(b0)) => b0 / a0 ,
          _ => panic!()
        };

        self.stack.push(VMType::Integer(n))
    }

    pub fn handle_div(&mut self, _line: u16) {
        let a = self.stack.pop().unwrap();
        let b = self.stack.pop().unwrap();

        self.stack.push(b / a)
    }
}
