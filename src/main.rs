use std::env::args;
use std::fs::read_to_string;

mod lexing;
use lexing::{extract_blocks, extract_instructions, lex_into_tokens};

mod interpreter;
use interpreter::Interpreter;

mod enums;
mod stack;

fn main() {
    let args = args().collect::<Vec<String>>();
    let content = read_to_string(args.get(1).unwrap()).expect("Failed to open file");

    let mut interpreter = Interpreter::default();

    extract_instructions(extract_blocks(lex_into_tokens(&content)))
        .iter()
        .for_each(|instruction| interpreter.handle_instruction(&mut instruction.iter()));
}
