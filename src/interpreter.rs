use crate::lexer::{extract_instructions, Token};

use std::collections::HashMap;
use std::slice::Iter;

use std::ops::{Add, Div, Mul, Not, Rem, Sub};

/// The `VMType` enum stores all data types that the
/// interpreter can work with
///
/// The `Clone` and `Debug` traits are implemented, which allows
/// the enum members to be easily copied, cloned, and printed.
#[derive(Debug, Clone)]
pub enum VMType {
    Float(f64),
    Integer(i64),
    Str(String),
    Bool(bool),
    Array(Vec<VMType>),
}

impl PartialOrd for VMType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (VMType::Integer(a), VMType::Integer(b)) => Some(a.cmp(b)),
            (VMType::Float(a), VMType::Float(b)) => Some(a.total_cmp(b)),
            (VMType::Integer(a), VMType::Float(b)) | (VMType::Float(b), VMType::Integer(a)) => {
                Some(b.total_cmp(&(*a as f64)))
            }
            (a, b) => panic!("can't compare {a:?} and {b:?}"),
        }
    }
}

impl PartialEq for VMType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VMType::Str(a), VMType::Str(b)) => a == b,
            (VMType::Array(a), VMType::Array(b)) => a == b,
            (VMType::Float(a), VMType::Integer(b)) => *a == (*b as f64),
            (VMType::Integer(a), VMType::Float(b)) => (*a as f64) == *b,
            (VMType::Integer(a), VMType::Integer(b)) => a == b,
            (VMType::Float(a), VMType::Float(b)) => a == b,
            (VMType::Bool(a), VMType::Bool(b)) => a == b,
            _ => false,
        }
    }
}

impl ToString for VMType {
    fn to_string(&self) -> String {
        match self {
            VMType::Float(a) => a.to_string(),
            VMType::Integer(a) => a.to_string(),
            VMType::Str(a) => a.to_string(),
            VMType::Bool(a) => a.to_string(),
            VMType::Array(a) => format!("{a:?}"),
        }
    }
}

impl Sub for VMType {
    type Output = VMType;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Integer(a - b),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a - b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 - b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a - b as f64),
            (a, b) => panic!("can't sub {a:?} and {b:?}"),
        }
    }
}

impl Not for VMType {
    type Output = VMType;

    fn not(self) -> Self::Output {
        match self {
            VMType::Float(a) => VMType::Float(a * -1.),
            VMType::Integer(a) => VMType::Integer(-a),
            VMType::Bool(true) => VMType::Bool(false),
            VMType::Bool(false) => VMType::Bool(true),
            a => panic!("can't invert {a:?}"),
        }
    }
}

impl Mul for VMType {
    type Output = VMType;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Integer(a * b),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a * b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 * b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a * b as f64),
            (VMType::Bool(false), VMType::Bool(_)) => VMType::Bool(false),
            (VMType::Bool(_), VMType::Bool(false)) => VMType::Bool(false),
            (VMType::Bool(true), VMType::Bool(true)) => VMType::Bool(true),

            (a, b) => panic!("can't mul {a:?} and {b:?}"),
        }
    }
}

impl Add for VMType {
    type Output = VMType;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Str(a), VMType::Str(b)) => VMType::Str(a + &b),
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Integer(a + b),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a + b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 + b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a + b as f64),
            (VMType::Bool(true), VMType::Bool(_)) => VMType::Bool(true),
            (VMType::Bool(_), VMType::Bool(true)) => VMType::Bool(true),
            (VMType::Bool(false), VMType::Bool(false)) => VMType::Bool(false),
            (other, VMType::Array(mut array)) | (VMType::Array(mut array), other) => {
                array.push(other);
                VMType::Array(array)
            }
            (a, b) => panic!("can't add {a:?} and {b:?}"),
        }
    }
}

impl Rem for VMType {
    type Output = VMType;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Integer(a % b),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a % b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 % b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a % b as f64),
            (a, b) => panic!("can't modulo {a:?} and {b:?}"),
        }
    }
}

impl Div for VMType {
    type Output = VMType;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(a), VMType::Integer(b)) => VMType::Float(a as f64 / b as f64),
            (VMType::Float(a), VMType::Float(b)) => VMType::Float(a / b),
            (VMType::Integer(a), VMType::Float(b)) => VMType::Float(a as f64 / b),
            (VMType::Float(a), VMType::Integer(b)) => VMType::Float(a / b as f64),
            (a, b) => panic!("can't divide {a:?} and {b:?}"),
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
    pub data: Vec<VMType>,
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
                            _ => panic!(),
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
