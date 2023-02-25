use std::env::args;
use std::fs::read_to_string;

use maeel_formatter::format;
use maeel_interpreter::Interpreter;
use maeel_lexer::{extract_blocks, extract_instructions, lex_into_tokens, Token};

macro_rules! usage {
    () => {
        println!(
            r#"
Maeel interpreter usage
=======================

maeel run <file>    <> Execute a maeel program
maeel fmt <file>    <> Format a maeel program
maeel lex <file>    <> Turn file into tokens
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
                let tokens = lex_into_tokens(&content);

                let mut interpreter = Interpreter::default();

                if !tokens.iter().any(|e| matches!(e, Token::Include)) {
                    extract_instructions(extract_blocks(&lex_into_tokens(&content)))
                        .iter()
                        .for_each(|instruction| {
                            interpreter.handle_instruction(&mut instruction.iter())
                        });
                } else {
                    let mut tokens_backup = Vec::new();
                    let mut tokens_iter = tokens.iter();

                    while let Some(token) = tokens_iter.next() {
                        match token {
                            Token::Include => {
                                let next_token = tokens_iter.next();

                                let path = match next_token {
                                    Some(Token::Str(path)) => path,
                                    _ => panic!(),
                                };

                                let include_content = read_to_string(path).unwrap();
                                tokens_backup.append(&mut lex_into_tokens(&include_content))
                            }
                            value => tokens_backup.push(value.clone()),
                        }
                    }
                    extract_instructions(extract_blocks(&tokens_backup))
                        .iter()
                        .for_each(|instruction| {
                            interpreter.handle_instruction(&mut instruction.iter())
                        });
                }
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
