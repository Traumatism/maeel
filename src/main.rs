mod enums;
mod optimization;
mod parse;
mod playground;
mod tokenize;
mod utils;
mod vm;

fn _playground() {
    let mut pg = playground::Playground::default();

    pg.evaluate_expression("push 1 2 3");
    println!("{:?}", pg.get_output());

    pg.evaluate_expression("pop");
    println!("{:?}", pg.get_output());
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let first = args.get(1).unwrap();

    let content = std::fs::read_to_string(first).expect("Failed to open file");

    let mut vm = vm::Stack::<enums::VMType>::default();

    let mut instructions =
        tokenize::parse_into_instructions(&mut tokenize::lex_into_tokens(&content));

    while let Some(mut instruction) = instructions.pop() {
        parse::parse(&mut instruction, &mut vm)
    }
}
