mod enums;
mod parse;
mod tokenize;
mod vm;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let content = std::fs::read_to_string(args.get(1).unwrap()).expect("Failed to open file");

    parse::Parser::new(
        tokenize::parse_into_instructions(&mut tokenize::lex_into_tokens(&content)),
        vm::VM::default(),
    )
    .parse();
}
