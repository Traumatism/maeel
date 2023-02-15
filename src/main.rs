use std::env::args;
use std::fs::read_to_string;

use crate::interpreter::Interpreter;
use crate::lexing::{extract_blocks, extract_instructions, lex_into_tokens};

mod interpreter;
mod lexing;
mod token;
mod vmtype;

fn main() {
    let args = args().collect::<Vec<String>>();
    let content = read_to_string(args.get(1).unwrap()).expect("Failed to open file");
    let tokens = lex_into_tokens(&content);

    let mut interpreter = Interpreter::default();

    extract_instructions(extract_blocks(tokens))
        .iter()
        .for_each(|instruction| interpreter.handle_instruction(&mut instruction.iter()));
}
