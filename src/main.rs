use hashbrown::HashMap;

mod vm;

mod lexer;
use lexer::*;

mod interpreter;
use interpreter::*;
use vm::MaeelType;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Read program content
    let content = std::fs::read_to_string(
        std::env::args() // Shell args
            .collect::<Vec<String>>()
            .get(1)
            .expect("Please provide a file"),
    )
    .expect("Failed to open file");

    process_tokens(
        &lex_into_tokens(&content),
        &mut vm::BocchiVM::<MaeelType>::default(),
        &mut HashMap::default(),
        &mut HashMap::default(),
    )?;

    Ok(())
}
