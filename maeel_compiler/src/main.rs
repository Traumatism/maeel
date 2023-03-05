use std::fs::read_to_string;
use std::{collections::HashMap, env::args};

use maeel_common::tokens::Token;
use maeel_interpreter::process_tokens;
use maeel_lexer::{extract_blocks, lex_into_tokens};

macro_rules! usage {
    () => {
        println!(
            r#"
Maeel interpreter usage
=======================

maeel run <file>        <> Execute a maeel program
maeel lex <file>        <> Turn file into tokens
maeel check <file>      <> Check program for typing errors
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

                if !tokens.iter().any(|e| matches!(e.token, Token::Include)) {
                    process_tokens(
                        &mut extract_blocks(&lex_into_tokens(&content)).iter(),
                        Vec::new().as_mut(),
                        &mut HashMap::new(),
                        &mut HashMap::new(),
                    );
                } else {
                    let mut tokens_backup = Vec::new();
                    let mut tokens_iter = tokens.iter();

                    while let Some(token_data) = tokens_iter.next() {
                        let token = token_data.token.clone();

                        match token {
                            Token::Include => {
                                let next_token = tokens_iter.next().unwrap();

                                let path = match &next_token.token {
                                    Token::Str(path) => path,
                                    _ => panic!(),
                                };

                                let include_content = read_to_string(path).unwrap();
                                tokens_backup.append(&mut lex_into_tokens(&include_content))
                            }
                            _ => tokens_backup.push(token_data.clone()),
                        }
                    }

                    process_tokens(
                        &mut extract_blocks(&lex_into_tokens(&content)).iter(),
                        Vec::new().as_mut(),
                        &mut HashMap::new(),
                        &mut HashMap::new(),
                    );
                }
            }

            "lex" => {
                let content = read_to_string(args.get(2).unwrap()).expect("Failed to open file");
                extract_blocks(&lex_into_tokens(&content))
                    .iter()
                    .for_each(|instruction| println!("{:?}\n\n", instruction));
            }

            _ => usage!(),
        },

        None => usage!(),
    }
}
