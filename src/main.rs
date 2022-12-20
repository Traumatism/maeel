mod enums;
mod optimization;
mod parse;
mod tokenize;
mod utils;
mod vm;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let content = std::fs::read_to_string(args.get(1).unwrap()).expect("Failed to open file");

    let mut vm = vm::Stack::<enums::VMType>::default();
    let mut instructions =
        tokenize::parse_into_instructions(&mut tokenize::lex_into_tokens(&content));
    while let Some(mut instruction) = instructions.pop() {
        parse::parse(&mut instruction, &mut vm)
    }
}
