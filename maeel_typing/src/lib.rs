use maeel_lexer::{extract_blocks, extract_instructions, Token};

use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Not, Rem, Sub};
use std::slice::Iter;

#[derive(Clone)]
enum PseudoVMType {
    Float,
    Integer,
    IntPointer,
    Str,
    StrPointer,
    Bool,
    Array,
}

impl Sub for PseudoVMType {
    type Output = PseudoVMType;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (PseudoVMType::Integer, PseudoVMType::Integer) => PseudoVMType::Integer,
            (PseudoVMType::Float, PseudoVMType::Float)
            | (PseudoVMType::Integer, PseudoVMType::Float)
            | (PseudoVMType::Float, PseudoVMType::Integer) => PseudoVMType::Float,
            _ => panic!(),
        }
    }
}

impl Not for PseudoVMType {
    type Output = PseudoVMType;

    fn not(self) -> Self::Output {
        match self {
            PseudoVMType::Float => PseudoVMType::Float,
            PseudoVMType::Integer => PseudoVMType::Integer,
            PseudoVMType::Bool => PseudoVMType::Bool,
            _ => panic!(),
        }
    }
}

impl Mul for PseudoVMType {
    type Output = PseudoVMType;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (PseudoVMType::Integer, PseudoVMType::Integer) => PseudoVMType::Integer,
            (PseudoVMType::Bool, PseudoVMType::Bool) => PseudoVMType::Bool,

            (PseudoVMType::Float, PseudoVMType::Float)
            | (PseudoVMType::Integer, PseudoVMType::Float)
            | (PseudoVMType::Float, PseudoVMType::Integer) => PseudoVMType::Float,

            _ => panic!(),
        }
    }
}

impl Add for PseudoVMType {
    type Output = PseudoVMType;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (PseudoVMType::Float, PseudoVMType::Float)
            | (PseudoVMType::Integer, PseudoVMType::Float)
            | (PseudoVMType::Float, PseudoVMType::Integer) => PseudoVMType::Float,

            (PseudoVMType::Str, PseudoVMType::Str) => PseudoVMType::Str,
            (PseudoVMType::Bool, PseudoVMType::Bool) => PseudoVMType::Bool,
            (PseudoVMType::Integer, PseudoVMType::Integer) => PseudoVMType::Integer,
            (_, PseudoVMType::Array) | (PseudoVMType::Array, _) => PseudoVMType::Array,

            _ => panic!(),
        }
    }
}

impl Rem for PseudoVMType {
    type Output = PseudoVMType;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (PseudoVMType::Float, PseudoVMType::Float)
            | (PseudoVMType::Integer, PseudoVMType::Float)
            | (PseudoVMType::Float, PseudoVMType::Integer) => PseudoVMType::Float,

            (PseudoVMType::Integer, PseudoVMType::Integer) => PseudoVMType::Integer,
            _ => panic!(),
        }
    }
}

impl Div for PseudoVMType {
    type Output = PseudoVMType;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (PseudoVMType::Float, PseudoVMType::Float)
            | (PseudoVMType::Integer, PseudoVMType::Integer)
            | (PseudoVMType::Integer, PseudoVMType::Float)
            | (PseudoVMType::Float, PseudoVMType::Integer) => PseudoVMType::Float,
            _ => panic!(),
        }
    }
}

macro_rules! next {
    ($tokens:expr, "identifier") => {
        match $tokens.next() {
            Some(Token::Identifier(value)) => value.clone(),
            token => panic!("Expected identifier, got {:?}", token),
        }
    };

    ($tokens:expr, "block") => {
        match $tokens.next() {
            Some(Token::Block(block)) => block.to_vec(),
            token => panic!("Expected block, got {:?}", token),
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

    ($self:ident, $content:expr, $PseudoVMType:expr) => {
        $self.data.push($PseudoVMType)
    };
}

macro_rules! binary_op {
    ($self:ident, $operator:tt) => {{
        let a = pop!($self);
        let b = pop!($self);

        push!($self, b $operator a);
    }};

    ($self:ident, $operator:tt, $PseudoVMType:expr) => {{
        let a = pop!($self);
        let b = pop!($self);

        push!($self, b $operator a, $PseudoVMType)
    }};
}

macro_rules! run_block {
    ($self:ident, $block:ident) => {
        for instruction in extract_instructions($block.clone()) {
            $self.handle_instruction(&mut instruction.iter())
        }
    };
}

/// Maeel pseudo interpreter
#[derive(Default)]
pub struct PseudoInterpreter {
    data: Vec<PseudoVMType>,
    vars: HashMap<String, PseudoVMType>,
    procs: HashMap<String, Vec<Token>>,
    stop_execution: bool,
}

impl PseudoInterpreter {
    pub fn handle_instruction(&mut self, tokens: &mut Iter<Token>) {
        while let Some(token) = tokens.next() {
            if self.stop_execution {
                return;
            }

            match token.clone() {
                Token::Include => panic!(),
                Token::Return => self.stop_execution = true,
                Token::Clear => self.data.clear(),
                Token::BlockStart | Token::BlockEnd => panic!(),

                Token::Str(_) => push!(self, content, PseudoVMType::Str),
                Token::Bool(_) => push!(self, content, PseudoVMType::Bool),
                Token::Float(_) => push!(self, content, PseudoVMType::Float),
                Token::Integer(_) => push!(self, content, PseudoVMType::Integer),

                Token::Dup => {
                    if self.data.len() < 1 {
                        panic!("Warning: found dup, but stack is empty")
                    }

                    push!(self, self.data.last().cloned().unwrap())
                }

                Token::Rot => {
                    if self.data.len() < 3 {
                        panic!("Warning: found rot, but stack size is <3")
                    }

                    let third = pop!(self);
                    let second = pop!(self);
                    let first = pop!(self);

                    push!(self, second);
                    push!(self, third);
                    push!(self, first);
                }

                Token::Swap => {
                    if self.data.len() < 2 {
                        panic!("Warning: found swap, but stack size is <2");
                    }

                    let second = pop!(self);
                    let first = pop!(self);

                    push!(self, second);
                    push!(self, first);
                }

                Token::Over => {
                    if self.data.len() < 1 {
                        panic!("Warning: found over, but stack size is <2");
                    }

                    push!(self, self.data[self.data.len() - 2].to_owned());
                }

                Token::Sub => binary_op!(self, -),
                Token::Add => binary_op!(self, +),
                Token::Mul => binary_op!(self, *),
                Token::Div => binary_op!(self, /),
                Token::Mod => binary_op!(self, %),

                Token::Gt => {}
                Token::Lt => {}
                Token::Eq => {}

                Token::Block(tokens) => run_block!(self, tokens),

                Token::Pop => {
                    if self.data.len() < 1 {
                        panic!("Warning: found pop, but stack size is <1");
                    }

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
                    Some(PseudoVMType::Integer) => {
                        push!(self, array, PseudoVMType::Array)
                    }
                    _ => panic!(),
                },

                Token::Identifier(identifier) => {
                    match identifier.as_str() {
                        "syscall" => {}

                        "ptr" => {
                            let element = pop!(self);

                            let new_element = match element {
                                PseudoVMType::Str => PseudoVMType::StrPointer,
                                PseudoVMType::Integer => PseudoVMType::IntPointer,
                                _ => panic!(),
                            };

                            push!(self, new_element);
                        }

                        "println" | "print" => {
                            let element = pop!(self);
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
                        Some(Token::Str(_)) => PseudoVMType::Str,
                        Some(Token::Integer(_)) => PseudoVMType::Integer,
                        Some(Token::Float(_)) => PseudoVMType::Float,
                        Some(Token::Bool(_)) => PseudoVMType::Bool,
                        Some(Token::Over) => self.data[self.data.len() - 2].clone(),
                        Some(Token::Dup) => self.data.last().cloned().unwrap(),
                        Some(Token::Pop) => pop!(self),
                        _ => panic!(),
                    };

                    self.vars.insert(name, value);
                }

                Token::While => {
                    let while_block = next!(tokens, "block");

                    while let PseudoVMType::Bool = pop!(self) {
                        run_block!(self, while_block);

                        if self.stop_execution {
                            break;
                        }
                    }
                }

                Token::For => {
                    let _for_block = next!(tokens, "block");

                    match pop!(self, 1) {
                        Some(PseudoVMType::Array) => {}
                        _ => panic!(),
                    }
                }

                Token::If => {
                    let _if_block = next!(tokens, "block");

                    if let Some(PseudoVMType::Bool) = pop!(self, 1) {
                    } else {
                        panic!();
                    }
                }
            }
        }
    }
}

pub fn analyze(tokens: Vec<Token>) {
    let mut tokens_backup = Vec::new();
    let mut tokens_iter = tokens.iter();
    let mut interpreter = PseudoInterpreter::default();

    while let Some(token) = tokens_iter.next() {
        match token {
            Token::Include => {
                let next_token = tokens_iter.next();

                match next_token {
                    Some(Token::Str(path)) => path,
                    _ => panic!(),
                };
            }
            value => tokens_backup.push(value.clone()),
        }
    }

    extract_instructions(extract_blocks(&tokens_backup))
        .iter()
        .for_each(|instruction| interpreter.handle_instruction(&mut instruction.iter()));
}
