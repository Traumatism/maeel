use maeel_common::tokens::{Token, TokenData};
use maeel_common::vmtypes::VMType;
use std::collections::HashMap;
use std::slice::Iter;

use core::arch::asm;

mod syscalls;
use syscalls::handle_syscall;

const DEBUG: bool = false;

macro_rules! debug {
    ($message:expr) => {
        if DEBUG {
            println!("{}", $message)
        }
    };
}

macro_rules! next {
    ($tokens:expr, "identifier") => {{
        debug!("Expecting an identifier...");

        let next = $tokens.next().unwrap();

        match &next.token {
            Token::Identifier(value) => value.clone(),
            token => panic!("Expected identifier, got {:?}", token),
        }
    }};

    ($tokens:expr, "block") => {{
        debug!("Expecting an block...");

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

        debug!(format!("Binary OP: {:?} {} {:?}", &a, stringify!($operator), &b));

        push!($self, b $operator a);
    }};

    ($self:ident, $operator:tt, $vmtype:expr) => {{
        let a = pop!($self);
        let b = pop!($self);

        debug!(format!("Binary OP: {:?} {} {:?}", &a, stringify!($operator), &b));

        push!($self, b $operator a, $vmtype)
    }};
}

/// Maeel interpreter
#[derive(Default)]
pub struct Interpreter {
    pub data: Vec<VMType>,
    pub vars: HashMap<String, VMType>,
    procs: HashMap<String, Vec<TokenData>>,
    stop_execution: bool,
}

impl Interpreter {
    pub fn handle_instruction(&mut self, tokens: &mut Iter<TokenData>) {
        while let Some(token_data) = tokens.next() {
            if self.stop_execution {
                return;
            }

            let token = token_data.token.clone();
            let line = token_data.line;

            match token.clone() {
                Token::Include => panic!(),

                Token::Return => self.stop_execution = true,

                Token::Clear => self.data.clear(),

                Token::BlockStart | Token::BlockEnd => panic!(),

                Token::Str(content) => push!(self, content, VMType::Str),

                Token::Bool(content) => push!(self, content, VMType::Bool),

                Token::Float(content) => push!(self, content, VMType::Float),

                Token::Integer(content) => push!(self, content, VMType::Integer),

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

                Token::Gt => binary_op!(self, >, VMType::Bool),

                Token::Lt => binary_op!(self, <, VMType::Bool),

                Token::Eq => binary_op!(self, ==, VMType::Bool),

                Token::Block(tokens) => self.handle_instruction(&mut tokens.iter()),

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
                    Some(VMType::Integer(n)) => {
                        let array = (0..n).map(|_| pop!(self)).collect();
                        push!(self, array, VMType::Array)
                    }
                    _ => panic!(),
                },

                Token::Identifier(identifier) => {
                    match identifier.as_str() {
                        "ptr" => {
                            let element = pop!(self);

                            let new_element = match element {
                                VMType::Str(content) => VMType::StrPointer(content),
                                VMType::Integer(content) => VMType::IntPointer(Box::new(content)),
                                _ => panic!(),
                            };

                            push!(self, new_element);
                        }

                        "syscall" => {
                            let mut syscalls_args = Vec::new();

                            let syscall_nr = match self.vars.get("syscall_number") {
                                Some(VMType::Integer(number)) => *number as usize,
                                _ => panic!(),
                            };

                            for name in self.vars.keys() {
                                if !name.starts_with("syscall_") {
                                    continue;
                                }

                                let name_cpy = name.clone();
                                let number_e = name_cpy[8..name_cpy.len()].parse::<u8>();

                                let number = if let Ok(n) = number_e {
                                    n
                                } else {
                                    continue;
                                };

                                syscalls_args.push((number, self.vars[name].clone()));
                            }

                            syscalls_args.sort_by_key(|(n, _)| *n);

                            handle_syscall(
                                syscall_nr,
                                &syscalls_args
                                    .into_iter()
                                    .map(|(_, arg)| arg)
                                    .collect::<Vec<VMType>>(),
                            );
                        }

                        "print" => {
                            let element = pop!(self);
                            let message = element.to_string();

                            #[cfg(not(any(
                                all(target_family = "unix", target_arch = "x86_64"),
                                all(target_os = "macos", target_arch = "aarch64")
                            )))]

                            print!("{}", message);

                            unsafe {
                                #[cfg(all(target_os = "macos", target_arch = "aarch64",))]
                                asm!(
                                    "svc #0",
                                    in("x0") 1,
                                    in("x1") message.as_ptr(),
                                    in("x2") message.len(),
                                    in("x16") 4,
                                );

                                #[cfg(all(target_family = "unix", target_arch = "x86_64"))]
                                asm!(
                                    "syscall",
                                    in("rax") 1,
                                    in("rdi") 1,
                                    in("rsi") message.as_ptr(),
                                    in("rdx") message.len(),
                                );
                            }

                            push!(self, element);
                        }

                        identifier => {
                            if let Some(value) = self.vars.get(identifier) {
                                push!(self, value.clone());
                            } else {
                                let tokens = self.procs.get(identifier).unwrap().clone();
                                self.handle_instruction(&mut tokens.iter());
                            }
                        }
                    };
                }

                Token::Let => {
                    let name = next!(tokens, "identifier");
                    let next = tokens.next().unwrap();

                    let value = match &next.token {
                        Token::Str(content) => VMType::Str(content.clone()),
                        Token::Float(x) => VMType::Float(*x),
                        Token::Bool(p) => VMType::Bool(*p),
                        Token::Integer(n) => VMType::Integer(*n),
                        Token::Over => self.data[self.data.len() - 2].clone(),
                        Token::Dup => self.data.last().cloned().unwrap(),
                        Token::Pop => pop!(self),
                        _ => panic!(),
                    };

                    self.vars.insert(name, value);
                }

                Token::While => {
                    let tokens = next!(tokens, "block");

                    debug!(format!("While block: {:?}", tokens));

                    while let VMType::Bool(true) = pop!(self) {
                        self.handle_instruction(&mut tokens.iter());

                        if self.stop_execution {
                            break;
                        }
                    }
                }

                Token::For => {
                    let tokens = next!(tokens, "block");

                    debug!(format!("For block: {:?}", tokens));

                    match pop!(self, 1) {
                        Some(VMType::Array(array)) => {
                            for element in array {
                                push!(self, element);
                                self.handle_instruction(&mut tokens.iter());

                                if self.stop_execution {
                                    break;
                                }
                            }
                        }
                        _ => panic!(),
                    }
                }

                Token::If => {
                    let tokens = next!(tokens, "block");

                    debug!(format!("If block: {:?}", tokens));

                    if let Some(VMType::Bool(e)) = pop!(self, 1) {
                        if e {
                            self.handle_instruction(&mut tokens.iter())
                        }
                    } else {
                        panic!("{line}");
                    }
                }
            }
        }
    }
}
