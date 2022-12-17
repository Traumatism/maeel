use tokenize::{lex_into_tokens, parse_into_instructions};

mod parse;
mod standard;
mod tokenize;
mod utils;
mod vm;

fn main() {
    let content = std::fs::read_to_string("hello.mae").unwrap();

    println!("performing lexical analysis...");
    let mut tokens = lex_into_tokens(&content);

    println!("parsing into instructions...  ");

    let mut instructions = parse_into_instructions(&mut tokens);
    let mut vm = vm::Stack::<vm::VMTypes>::default();

    while let Some(mut instruction) = instructions.pop() {
        parse::parse(&mut instruction, &mut vm)
    }
}
