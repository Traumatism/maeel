pub mod interpreter;
pub mod lexer;

use std::collections::HashMap;
use std::env::args;
use std::fs::read_to_string;
use std::io::Result;

fn main() -> Result<()> {
    let args = args().collect::<Vec<String>>();

    let content =
        read_to_string(args.get(1).expect("Please provide a file")).expect("Failed to open file");

    interpreter::process_tokens(
        args.get(1).unwrap(),
        &mut lexer::extract_blocks(&lexer::lex_into_tokens(&content)).iter(),
        Vec::new().as_mut(),
        &mut HashMap::new(),
        &mut HashMap::new(),
    )?;

    Ok(())
}
