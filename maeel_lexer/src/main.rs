use maeel_lexer::lex_into_tokens;

use std::env::args;

use std::fs::read_to_string;

use std::io::Result;

fn main() -> Result<()>
{

    let content = read_to_string(
        args()
            .collect::<Vec<String>>()
            .get(1)
            .expect("Please provide a file"),
    )
    .expect("Failed to open file");

    println!("{:?}", lex_into_tokens(&content));

    Ok(())
}
