use crate::enums;
use crate::parse;
use crate::tokenize;
use crate::vm;

#[derive(Default)]
pub struct Playground {
    vm: vm::Stack<enums::VMType>,
}

impl Playground {
    #[allow(dead_code, unused)]
    pub fn evaluate_expression(&mut self, expression: impl Into<String>) {
        let mut instructions =
            tokenize::parse_into_instructions(&mut tokenize::lex_into_tokens(&expression.into()));

        while let Some(mut instruction) = instructions.pop() {
            parse::parse(&mut instruction, &mut self.vm)
        }
    }

    #[allow(dead_code, unused)]
    pub fn get_output(&mut self) -> Vec<enums::VMType> {
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
