mod enums;
mod parse;
mod tokenize;
mod utils;
mod vm;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    let content = std::fs::read_to_string(args.get(1).unwrap()).expect("Failed to open file");

    let mut tokens = tokenize::lex_into_tokens(&content);
    let mut instructions = tokenize::parse_into_instructions(&mut tokens);
    let mut vm = vm::Stack::<enums::VMType>::default();

    while let Some(mut instruction) = instructions.pop() {
        parse::parse(&mut instruction, &mut vm)
    }
}
