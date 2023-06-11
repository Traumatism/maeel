use hashbrown::HashMap;

mod vm;

mod lexer;
use lexer::*;

mod interpreter;
use interpreter::*;
use vm::MaeelType;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content =
        std::fs::read_to_string(std::env::args().collect::<Vec<String>>().get(1).unwrap())?;

    process_tokens(
        &lex_into_tokens(&content),
        &mut vm::IkuyoVM::<MaeelType, 512>::default(),
        &mut HashMap::default(),
        &mut HashMap::default(),
        &mut HashMap::default(),
    )?;

    Ok(())
}
