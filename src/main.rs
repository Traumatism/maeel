use hashbrown::HashMap;

mod lexer;
mod vm;
use lexer::lex_into_tokens;

use std::env::args;
use std::fs::read_to_string;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut vm = vm::BocchiVM::default();

    vm.process_tokens(
        &mut lex_into_tokens(&read_to_string(args().nth(1).unwrap())?),
        &mut HashMap::default(),
        &mut HashMap::default(),
        &mut HashMap::default(),
    )
}
