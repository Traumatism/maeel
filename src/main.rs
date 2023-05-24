use std::collections::HashMap;

mod vm;
use vm::*;

mod lexer;
use lexer::*;

mod interpreter;
use interpreter::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Read program content
    let content = std::fs::read_to_string(
        std::env::args() // Shell args
            .collect::<Vec<String>>()
            .get(1)
            .expect("Please provide a file"),
    )
    .expect("Failed to open file");

    // Initial run
    process_tokens(
        &mut lex_into_tokens(&content).iter(),
        &mut VMStack::new(),     // data stack
        &mut HashMap::default(), // globals (variables)
        &mut HashMap::default(), // functions
    )?;

    Ok(())
}
