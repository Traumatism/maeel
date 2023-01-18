use std::env::args;
use std::fs::read_to_string;

mod lexing;
use lexing::{extract_blocks, extract_instructions, lex_into_tokens};

mod interpreter;
use interpreter::Interpreter;

mod enums;

fn main() {
    let args = args().collect::<Vec<String>>();
    let content = read_to_string(args.get(1).unwrap()).expect("Failed to open file");

    let mut interpreter = Interpreter::default();

    let tokens = lex_into_tokens(&content);
    let blocks = extract_blocks(tokens);

    extract_instructions(blocks)
        .iter()
        .for_each(|instruction| interpreter.handle_instruction(&mut instruction.iter()));
}
