mod vm;

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

    // let mut vm = vm::IkuyoVM::<vm::MaeelType, 1>::default();
    let mut vm = vm::BocchiVM::default();

    // Initial run
    process_tokens(
        &lex_into_tokens(&content),
        &mut vm,                            // data stack
        &mut hashbrown::HashMap::default(), // globals (variables)
        &mut hashbrown::HashMap::default(), // functions
    )?;

    Ok(())
}
