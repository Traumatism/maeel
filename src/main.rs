use std::env::args;
use std::fs::read_to_string;

use crate::formatter::format;
use crate::interpreter::Interpreter;
use crate::lexer::{extract_blocks, extract_instructions, lex_into_tokens};

mod formatter;
mod interpreter;
mod lexer;
mod token;
mod vmtype;

macro_rules! usage {
    () => {
        println!(
            r#"
Maeel interpreter usage
=======================

    maeel lex <file>    <> Turn file into tokens
    maeel run <file>    <> Execute a maeel program
    maeel fmt <file>    <> Format a maeel program

            "#
        )
    };
}

fn main() {
    let args = args().collect::<Vec<String>>();

    let subcommand = args.get(1);

    match subcommand {
        Some(subcommand_unwrapped) => match subcommand_unwrapped.as_str() {
            "run" => {
                let content = read_to_string(args.get(2).unwrap()).expect("Failed to open file");

                let mut interpreter = Interpreter::default();

                extract_instructions(extract_blocks(&lex_into_tokens(&content)))
                    .iter()
                    .for_each(|instruction| {
                        interpreter.handle_instruction(&mut instruction.iter())
                    });
            }

            "lex" => {
                let content = read_to_string(args.get(2).unwrap()).expect("Failed to open file");

                println!("{:#?}", lex_into_tokens(&content));
            }

            "fmt" => {
                let content = read_to_string(args.get(2).unwrap()).expect("Failed to open file");
                let tokens = lex_into_tokens(&content);

                println!("{}", format(tokens));
            }

            _ => usage!(),
        },

        None => usage!(),
    }
}
