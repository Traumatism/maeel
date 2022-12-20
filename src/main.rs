use std::io::{stdin, stdout, Write};

mod enums;
mod parse;
mod playground;
mod tokenize;
mod vm;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    if args.len() == 1 {
        let mut playground = playground::Playground::default();

        loop {
            let mut user_input = String::new();

            stdout().write_all(">>> ".as_bytes()).unwrap();
            stdout().flush().unwrap();
            stdin().read_line(&mut user_input).unwrap();

            playground.evaluate_expression(user_input);

            println!("\nCurrent stack: {:?}", playground.get_output())
        }
    }

    let content = std::fs::read_to_string(args.get(1).unwrap()).expect("Failed to open file");

    let mut vm = vm::Stack::<enums::VMType>::default();

    let mut instructions =
        tokenize::parse_into_instructions(&mut tokenize::lex_into_tokens(&content));

    while let Some(mut instruction) = instructions.pop() {
        parse::parse(&mut instruction, &mut vm)
    }
}
