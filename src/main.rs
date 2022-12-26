use std::env::args;
use std::fs::read_to_string;

mod lexer;
use compiler::interpreter::Interpreter;
use compiler::Compiler;
use lexer::lex_into_tokens;

mod compiler;
mod structures;

mod enums;
use enums::token::Token;

fn main() {
    let args = args().collect::<Vec<String>>();
    let content = read_to_string(args.get(1).unwrap()).expect("Failed to open file");

    let binding = lex_into_tokens(&content);
    let mut tokens = binding.iter();

    let mut instructions = Vec::<Vec<Token>>::default();
    let mut current_instruction = Vec::<Token>::default();

    while let Some(next) = tokens.next() {
        match next.clone() {
            Token::ProcStart => {
                current_instruction.push(next.clone());

                for next_p in tokens.by_ref() {
                    match next_p {
                        Token::ProcEnd => break,
                        _ => current_instruction.push(next_p.clone()),
                    }
                }

                current_instruction.push(Token::ProcEnd);
            }
            Token::Separator => {
                current_instruction.push(Token::Separator);
                instructions.push(current_instruction);
                current_instruction = Vec::default()
            }
            _ => current_instruction.push(next.clone()),
        }
    }

    instructions.push(current_instruction);

    let mut parser = Interpreter::default();

    for instruction in instructions {
        parser.handle_instruction(&mut instruction.iter())
    }
}
