use std::io::{stdin, stdout, Write};

mod enums;
mod parse;
mod playground;
mod tokenize;
mod vm;

fn main() {
    let mut playground = playground::Playground::default();

    loop {
        let mut user_input = String::new();

        stdout().write_all("-> ".as_bytes()).unwrap();
        stdout().flush().unwrap();
        stdin().read_line(&mut user_input).unwrap();

        playground.evaluate_expression(user_input);

        println!("\n<- {:?}", playground.get_output())
    }
}
