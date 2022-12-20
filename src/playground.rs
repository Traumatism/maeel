use crate::{
    enums::VMType,
    parse::parse,
    tokenize::{lex_into_tokens, parse_into_instructions},
    vm::Stack,
};

#[derive(Default)]
pub struct Playground {
    vm: Stack<VMType>,
}

impl Playground {
    pub fn evaluate_expression(&mut self, expression: impl Into<String>) {
        let mut instructions = parse_into_instructions(&mut lex_into_tokens(&expression.into()));

        while let Some(mut instruction) = instructions.pop() {
            parse(&mut instruction, &mut self.vm)
        }
    }

    pub fn get_output(&mut self) -> Vec<VMType> {
        let mut output = Vec::new();

        while let Some(next) = self.vm.pop() {
            output.push(next)
        }

        output.reverse();

        for value in &output {
            self.vm.push(value.clone())
        }

        output
    }
}
