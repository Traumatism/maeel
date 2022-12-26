use std::collections::BTreeMap;

use crate::enums::token::Token;
use crate::enums::vmtype::VMType;
use crate::{compiler::Compiler, structures::stack::Stack};

pub struct Interpreter {
    stop_proc_execution: bool,
    in_proc: bool,
    procs: BTreeMap<String, Vec<Token>>,
    vars: BTreeMap<String, VMType>,
    stack: Stack,
}

#[allow(clippy::derivable_impls)]
impl Default for Interpreter {
    fn default() -> Self {
        Self {
            stop_proc_execution: false,
            in_proc: false,
            procs: BTreeMap::default(),
            vars: BTreeMap::default(),
            stack: Stack::default(),
        }
    }
}

impl Interpreter {
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
}

impl Compiler for Interpreter {
    fn handle_push_str(&mut self, content: String) {
        self.stack.push(VMType::Str(content));
    }

    fn handle_push_int(&mut self, content: i64) {
        self.stack.push(VMType::Integer(content));
    }

    fn handle_push_float(&mut self, content: f64) {
        self.stack.push(VMType::Float(content));
    }

    fn handle_push_bool(&mut self, content: bool) {
        self.stack.push(VMType::Bool(content));
    }

    fn handle_pop(&mut self) -> Option<VMType> {
        self.stack.pop()
    }

    fn handle_dup(&mut self) {
        self.stack.dup()
    }

    fn handle_swap(&mut self) {
        self.stack.swap()
    }

    fn handle_clear(&mut self) {
        self.stack.clear()
    }

    fn handle_add(&mut self, line: u16) {
        let a = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `add` requires two values on the top of the stack!")
        });

        let b = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `add` requires two values on the top of the stack!")
        });

        self.stack.push(a + b)
    }

    fn handle_mul(&mut self, line: u16) {
        let a = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `mul` requires two values on the top of the stack!")
        });

        let b = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `mul` requires two values on the top of the stack!")
        });

        self.stack.push(a * b)
    }

    fn handle_sub(&mut self, line: u16) {
        let a = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `sub` requires two values on the top of the stack!")
        });

        let b = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `sub` requires two values on the top of the stack!")
        });

        self.stack.push(b - a)
    }

    fn handle_or(&mut self, line: u16) {
        let p = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `or` requires two values on the top of the stack!")
        });

        let q = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `or` requires two values on the top of the stack!")
        });

        self.stack.push(p | q)
    }

    fn handle_xor(&mut self, line: u16) {
        let p = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `xor` requires two values on the top of the stack!")
        });

        let q = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `xor` requires two values on the top of the stack!")
        });

        self.stack.push(p ^ q)
    }

    fn handle_and(&mut self, line: u16) {
        let p = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `and` requires two values on the top of the stack!")
        });

        let q = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `and` requires two values on the top of the stack!")
        });

        self.stack.push(p & q)
    }

    fn handle_not(&mut self, line: u16) {
        let p = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `not` requires one value on the top of the stack!")
        });

        self.stack.push(!p)
    }

    fn handle_eq(&mut self, line: u16) {
        let a = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `eq` requires two values on the top of the stack!")
        });

        let b = self.stack.pop().unwrap_or_else(|| {
            panic!("line {line}: `eq` requires two values on the top of the stack!")
        });

        self.stack.push(VMType::Bool(a == b))
    }

    fn handle_return(&mut self, line: u16) {
        if !self.in_proc {
            panic!("line {line}: `return` used outside of a procedure!")
        }

        self.stop_proc_execution = true
    }

    fn handle_take(&mut self, line: u16) {
        match self.stack.pop() {
            Some(VMType::Integer(n)) => {
                let array = (0..n)
                    .map(|_| {
                        self.stack
                            .pop()
                            .unwrap_or_else(|| panic!("line {line}: no more values on the stack!"))
                    })
                    .collect();
                self.stack.push(VMType::Array(array))
            }
            _ => panic!("line {line}: `take` requires an integer on the stack!"),
        }
    }

    fn handle_reverse(&mut self, line: u16) {
        match self.stack.pop() {
            Some(VMType::Array(mut array)) => {
                array.reverse();
                self.stack.push(VMType::Array(array))
            }
            _ => {
                panic!("line {line}: `reverse` expect an array on top of the stack")
            }
        }
    }

    fn handle_identifier(&mut self, identifier: &str, line: u16) {
        match identifier {
            "exit" => match self.stack.pop().unwrap() {
                VMType::Integer(status) => std::process::exit(status as i32),
                _ => panic!(),
            },

            // Parse a print/println instruction
            //
            // Print stack head to stdout
            "print" | "println" => {
                let e = self.stack.pop().unwrap_or_else(|| {
                    panic!(
                        "line {line}: `{identifier}` requires string|integer|float on the stack!"
                    )
                });

                (match identifier {
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
                    return;
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

                let mut proc_parser = Self::new(
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

                    proc_parser.handle_instruction(&mut instruction.iter())
                }

                self.stack = proc_parser.stack;
                self.vars = proc_parser.vars;
            }
        }
    }

    fn handle_var_add(&mut self, name: String, value: VMType) {
        self.vars.insert(name, value);
    }

    fn handle_var_del(&mut self, name: String) {
        self.vars.remove(&name).unwrap();
    }

    fn handle_proc_add(&mut self, name: String, tokens: Vec<Token>) {
        self.procs.insert(name, tokens);
    }
}
