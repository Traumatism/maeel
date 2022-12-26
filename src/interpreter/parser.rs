use crate::enums::token::Token;
use crate::enums::vmtype::VMType;
use crate::structures::stack::Stack;

use std::collections::BTreeMap;
use std::process::exit;
use std::slice::Iter;

/// Evaluate the instructions (runtime)
#[derive(Default)]
pub struct Parser {
    stop_proc_execution: bool,
    in_proc: bool,
    procs: BTreeMap<String, Vec<Token>>,
    vars: BTreeMap<String, VMType>,
    stack: Stack,
}

impl Parser {
    pub fn new(
        in_proc: bool,
        procs: BTreeMap<String, Vec<Token>>,
        vars: BTreeMap<String, VMType>,
        stack: Stack,
    ) -> Self {
        Self {
            stop_proc_execution: false,
            in_proc,
            procs,
            vars,
            stack,
        }
    }

    /// Parse a single instruction
    pub fn parse_instruction(&mut self, tokens: &mut Iter<Token>) {
        while let Some(token) = tokens.next() {
            match token.clone() {
                Token::Separator | Token::ProcEnd => (),
                Token::Bool(p, _) => self.stack.push(VMType::Bool(p)),
                Token::Str(content, _) => self.stack.push(VMType::Str(content)),
                Token::Integer(n, _) => self.stack.push(VMType::Integer(n)),
                Token::Float(x, _) => self.stack.push(VMType::Float(x)),

                // Parse new procedure definition
                Token::ProcStart => {
                    let proc_name = match tokens.next().unwrap() {
                        Token::Identifier(name, _) => name,
                        _ => panic!(),
                    };

                    let mut proc_tokens = Vec::new();
                    let mut found_proc_end = false;

                    for token in tokens.by_ref() {
                        match token {
                            Token::ProcEnd => {
                                found_proc_end = true;
                                break;
                            }
                            _ => proc_tokens.push(token.clone()),
                        }
                    }

                    if !found_proc_end {
                        panic!()
                    }

                    self.procs.insert(proc_name.clone(), proc_tokens);
                }

                // Parse if statement
                Token::If(line) => {
                    match self.stack.pop() {
                        Some(VMType::Bool(true)) => (),
                        Some(VMType::Bool(false)) => return,
                        _ => panic!(
                            "line {line}: `if` requires a boolean value on the top of the stack!"
                        ),
                    };
                }

                // Parse an add instruction
                //
                // 1 + 1 => 2
                // 1.0 + 1.0 => 2.0
                // 1.0 + 1 => 2.0
                Token::Add(line) => {
                    let a = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `add` requires two values on the top of the stack!")
                    });

                    let b = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `add` requires two values on the top of the stack!")
                    });

                    self.stack.push(a + b)
                }

                // Parse a mul instruction
                //
                // 2 * 1 => 2
                // 2.0 * 1.0 => 2.0
                // 2.0 * 1 => 2.0
                Token::Mul(line) => {
                    let a = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `mul` requires two values on the top of the stack!")
                    });

                    let b = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `mul` requires two values on the top of the stack!")
                    });

                    self.stack.push(a * b)
                }

                Token::Let(line) => {
                    let name = match tokens.next() {
                        Some(Token::Identifier(name, _)) => name,
                        _ => panic!("line {line}: syntax: `let name value` with value of type int|float|string or a pop|dup instruction!"),
                    };

                    let value = match tokens.next() {
                        Some(Token::Str(content, _)) => VMType::Str(content.clone()),
                        Some(Token::Integer(n, _)) => VMType::Integer(*n),
                        Some(Token::Float(x, _)) => VMType::Float(*x),
                        Some(Token::Bool(p, _)) => VMType::Bool(*p),
                        Some(Token::Pop) => self.stack.pop().unwrap_or_else(|| panic!("line {line}: let name pop requires a value on the top of the stack")),
                        Some(Token::Dup) => { self.stack.dup(); self.stack.pop().unwrap_or_else(|| panic!("line {line}: let name pop requires a value on the top of the stack")) }
                        _ => panic!("line {line}: syntax: `let name value;` with value of type int|float|string or a pop|dup instruction! (2)"),
                    };

                    self.vars.insert(name.clone(), value);
                }

                // Parse a not instruction
                //
                // 0 => 1
                // 1 => 0
                Token::Not(line) => {
                    let p = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `not` requires one value on the top of the stack!")
                    });

                    self.stack.push(!p)
                }

                // Parse an and instruction
                //
                // 0 + 0 => 0
                // 0 + 1 => 0
                // 1 + 0 => 0
                // 1 + 1 => 1
                Token::And(line) => {
                    let p = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `and` requires two values on the top of the stack!")
                    });

                    let q = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `and` requires two values on the top of the stack!")
                    });

                    self.stack.push(p & q)
                }

                // Parse an or instruction
                //
                // 0 + 0 => 0
                // 0 + 1 => 1
                // 1 + 0 => 1
                // 1 + 1 => 1
                Token::Or(line) => {
                    let p = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `or` requires two values on the top of the stack!")
                    });

                    let q = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `or` requires two values on the top of the stack!")
                    });

                    self.stack.push(p | q)
                }

                // Parse a xor instruction
                //
                // 0 + 0 => 0
                // 0 + 1 => 1
                // 1 + 0 => 1
                // 1 + 1 => 1
                Token::Xor(line) => {
                    let p = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `xor` requires two values on the top of the stack!")
                    });

                    let q = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `xor` requires two values on the top of the stack!")
                    });

                    self.stack.push(p ^ q)
                }

                // Parse a dup operation
                Token::Dup => self.stack.dup(),

                // Parse a swap operation
                Token::Swap => self.stack.swap(),

                // Parse a clear operation
                Token::Clear => self.stack.clear(),

                // Parse a pop operation
                Token::Pop => {
                    self.stack.pop();
                }

                // Parse an equality instruction
                //
                // 0 + 0 => 1
                // 0 + 1 => 0
                // 1 + 0 => 0
                // 1 + 1 => 1
                Token::Eq(line) => {
                    let a = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `eq` requires two values on the top of the stack!")
                    });

                    let b = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `eq` requires two values on the top of the stack!")
                    });

                    self.stack.push(VMType::Bool(a == b))
                }

                // Parse a sub instruction
                //
                // 3 - 1 => 2
                // 3.0 - 1.0 => 2.0
                // 3.0 - 1 => 2.0
                Token::Sub(line) => {
                    let a = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `sub` requires two values on the top of the stack!")
                    });

                    let b = self.stack.pop().unwrap_or_else(|| {
                        panic!("line {line}: `sub` requires two values on the top of the stack!")
                    });

                    self.stack.push(b - a)
                }

                Token::Return(line) => {
                    if !self.in_proc {
                        panic!("line {line}: `return` used outside of a procedure!")
                    }

                    self.stop_proc_execution = true
                }

                Token::Del(line) => {
                    let name = match tokens.next() {
                        Some(Token::Identifier(name, _)) => name,
                        _ => panic!("line {line}: syntax: `del name`"),
                    };

                    self.vars.remove(name);
                }

                // Parse a reverse instruction
                //
                // [a, b, c] => [c, b, a]
                Token::Reverse(line) => match self.stack.pop() {
                    Some(VMType::Array(mut array)) => {
                        array.reverse();
                        self.stack.push(VMType::Array(array))
                    }
                    _ => {
                        panic!("line {line}: `reverse` expect an array on top of the stack")
                    }
                },

                // Parse a take instruction
                //
                // |a|b|c|3| => [a, b, c]
                // |a|b|c|2| => [b, c]
                Token::Take(line) => match self.stack.pop() {
                    Some(VMType::Integer(n)) => {
                        let array = (0..n)
                            .map(|_| {
                                self.stack.pop().unwrap_or_else(|| {
                                    panic!("line {line}: no more values on the stack!")
                                })
                            })
                            .collect();
                        self.stack.push(VMType::Array(array))
                    }
                    _ => panic!("line {line}: `take` requires an integer on the stack!"),
                },

                Token::Identifier(identifier, line) => {
                    match &*identifier {
                        "exit" => match self.stack.pop().unwrap() {
                            VMType::Integer(status) => exit(status as i32),
                            _ => panic!(),
                        },

                        // Parse a print/println instruction
                        //
                        // Print stack head to stdout
                        "print" | "println" => {
                            let e = self.stack.pop().unwrap_or_else(|| {
                                panic!("line {line}: `{identifier}` requires string|integer|float on the stack!")
                            });

                            (match &*identifier {
                                "println" => |content| println!("{content}"),
                                "print" => |content| print!("{content}"),
                                _ => panic!(),
                            })(e.to_string());

                            self.stack.push(e);
                        }

                        identifier => {
                            let v = self.vars.get(identifier);

                            if let Some(value) = v {
                                self.stack.push(value.clone());
                                continue;
                            }

                            let proc_tokens = self.procs.get(identifier).expect(identifier).iter();

                            let mut instructions = Vec::<Vec<Token>>::default();
                            let mut current_instruction = Vec::<Token>::default();

                            for next in proc_tokens {
                                match next.clone() {
                                    Token::ProcStart => panic!(),
                                    Token::Separator => {
                                        current_instruction.push(Token::Separator);
                                        instructions.push(current_instruction);
                                        current_instruction = Vec::default()
                                    }
                                    _ => current_instruction.push(next.clone()),
                                }
                            }

                            instructions.push(current_instruction);

                            let mut proc_parser = Parser::new(
                                true,
                                self.procs.clone(),
                                self.vars.clone(),
                                self.stack.clone(),
                            );

                            let instructions_iter = instructions.iter();

                            for instruction in instructions_iter {
                                if proc_parser.stop_proc_execution {
                                    break;
                                }

                                proc_parser.parse_instruction(&mut instruction.iter())
                            }

                            self.stack = proc_parser.stack;
                            self.vars = proc_parser.vars;
                        }
                    }
                }
            }
        }
    }
}
