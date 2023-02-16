use std::env::args;
use std::fs::read_to_string;

use crate::interpreter::Interpreter;
use crate::lexing::{extract_instructions, lex_into_tokens};

mod interpreter;
mod lexing;
mod token;
mod vmtype;

fn main() {
    let content = read_to_string(args().collect::<Vec<String>>().get(1).unwrap())
        .expect("Failed to open file");

    let mut interpreter = Interpreter::default();

    extract_instructions(lex_into_tokens(&content))
        .iter()
        .for_each(|instruction| interpreter.handle_instruction(&mut instruction.iter()));
}
