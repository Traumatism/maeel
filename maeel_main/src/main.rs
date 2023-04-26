pub mod interpreter;
pub mod lexer;

use interpreter::process_tokens;
use lexer::{extract_blocks, lex_into_tokens};

use std::collections::HashMap;
use std::env::args;
use std::fs::read_to_string;
use std::io::Result;

fn main() -> Result<()> {
    let content = read_to_string(
        args()
            .collect::<Vec<String>>()
            .get(1)
            .expect("Please provide a file"),
    )
    .expect("Failed to open file");

    process_tokens(
        &mut extract_blocks(&lex_into_tokens(&content)).iter(),
        &mut Vec::new(),
        &mut HashMap::new(),
        &mut HashMap::new(),
    )?;

    Ok(())
}
