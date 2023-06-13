use hashbrown::HashMap;

mod vm;
use vm::*;

mod lexer;
use lexer::*;

use std::env::args;
use std::fs::read_to_string;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    #[cfg(any(not(maeelvm = "ikuyo"), maeelvm = "bocchi"))]
    let mut vm = vm::BocchiVM::default();

    #[cfg(all(maeelvm = "ikuyo"))]
    let mut vm = vm::IkuyoVM::<32>::default();

    vm.process_tokens(
        &lex_into_tokens(&read_to_string(args().nth(1).unwrap())?),
        /* Variables */
        &mut HashMap::default(),
        /* Functions */
        &mut HashMap::default(),
        /* Structures */
        &mut HashMap::default(),
    )
}
