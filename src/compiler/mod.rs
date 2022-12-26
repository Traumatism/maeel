use crate::enums::{token::Token, vmtype::VMType};
use std::slice::Iter;

pub trait Compiler {
    fn handle_instruction(&mut self, tokens: &mut Iter<Token>) {
        while let Some(token) = tokens.next() {
            match token.clone() {
                Token::Str(content, _) => self.handle_push_str(content),
                Token::Integer(content, _) => self.handle_push_int(content),
                Token::Float(content, _) => self.handle_push_float(content),
                Token::Bool(content, _) => self.handle_push_bool(content),
                Token::Pop => {
                    self.handle_pop();
                }
                Token::Dup => self.handle_dup(),
                Token::Swap => self.handle_swap(),
                Token::Clear => self.handle_clear(),
                Token::Add(line) => self.handle_add(line),
                Token::Mul(line) => self.handle_mul(line),
                Token::Sub(line) => self.handle_sub(line),
                Token::Or(line) => self.handle_or(line),
                Token::Xor(line) => self.handle_xor(line),
                Token::And(line) => self.handle_and(line),
                Token::Not(line) => self.handle_not(line),
                Token::Eq(line) => self.handle_eq(line),
                Token::Return(line) => self.handle_return(line),
                Token::Take(line) => self.handle_take(line),
                Token::Reverse(line) => self.handle_reverse(line),
                Token::Identifier(identifier, line) => self.handle_identifier(&identifier, line),
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
                        Some(Token::Pop) => self.handle_pop().unwrap(),
                        Some(Token::Dup) => { self.handle_dup(); self.handle_pop().unwrap() }
                        _ => panic!("line {line}: syntax: `let name value;` with value of type int|float|string or a pop|dup instruction! (2)"),
                    };

                    self.handle_var_add(name.to_string(), value)
                }

                Token::If(line) => {
                    match self.handle_pop() {
                        Some(VMType::Bool(true)) => (),
                        Some(VMType::Bool(false)) => return,
                        _ => panic!(
                            "line {line}: `if` requires a boolean value on the top of the stack!"
                        ),
                    };
                }
                Token::Del(line) => {
                    let name = match tokens.next() {
                        Some(Token::Identifier(name, _)) => name,
                        _ => panic!("line {line}: syntax: `del name`"),
                    };

                    self.handle_var_del(name.to_string())
                }

                Token::Separator => (),
                Token::ProcEnd => panic!(),

                Token::ProcStart => {
                    let proc_name = match tokens.next().unwrap() {
                        Token::Identifier(name, _) => name,
                        _ => panic!(),
                    };

                    let mut proc_tokens = Vec::new();
                    let mut found_proc_end = false;

                    #[allow(clippy::while_let_on_iterator)]
                    while let Some(token) = tokens.next() {
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

                    self.handle_proc_add(proc_name.to_string(), proc_tokens)
                }
            }
        }
    }

    /// Global variable insertion
    fn handle_var_add(&mut self, name: String, value: VMType);

    /// Global variable removal
    fn handle_var_del(&mut self, name: String);

    /// Global procedure insertion
    fn handle_proc_add(&mut self, name: String, tokens: Vec<Token>);

    /// Pushing strings
    fn handle_push_str(&mut self, content: String);

    /// Pushing integers
    fn handle_push_int(&mut self, content: i64);

    /// Pushing floats
    fn handle_push_float(&mut self, content: f64);

    /// Pushing booleans
    fn handle_push_bool(&mut self, content: bool);

    /// Poping from the stack
    fn handle_pop(&mut self) -> Option<VMType>;

    /// Duping the head value
    fn handle_dup(&mut self);

    /// Swaping the two topmost values
    fn handle_swap(&mut self);

    /// Clearing the stack
    fn handle_clear(&mut self);

    /// Pushing the sum of the two topmost values
    fn handle_add(&mut self, line: u16);

    /// Pushing the product of the two topmost values
    fn handle_mul(&mut self, line: u16);

    /// Pushing the difference of the two topmost values
    fn handle_sub(&mut self, line: u16);

    /// Pushing the OR result of the two topmost values
    fn handle_or(&mut self, line: u16);

    /// Pushing the XOR result of the two topmost values
    fn handle_xor(&mut self, line: u16);

    /// Pushing the AND result of the two topmost values
    fn handle_and(&mut self, line: u16);

    /// Pushing the NOT result of the head value
    fn handle_not(&mut self, line: u16);

    /// Pushing the EQ result of the two topmost values
    fn handle_eq(&mut self, line: u16);

    /// Stoping the execution of the current procedure
    fn handle_return(&mut self, line: u16);

    /// Taking n elements and pushing them as an array
    fn handle_take(&mut self, line: u16);

    /// Reversing the head array
    fn handle_reverse(&mut self, line: u16);

    /// Handling identifiers
    fn handle_identifier(&mut self, identifier: &str, line: u16);
}
