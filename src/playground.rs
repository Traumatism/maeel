use crate::enums;
use crate::tokenize;
use crate::vm;

#[derive(Default)]
pub struct Playground {
    vm: vm::Stack<enums::VMType>,
}

impl Playground {
    #[allow(unused)]
    pub fn evaluate_expression(&mut self, expression: impl Into<String>) {
        todo!();

        let instructions =
            tokenize::parse_into_instructions(&mut tokenize::lex_into_tokens(&expression.into()));

        // Parser::new(instructions, self.vm).parse();
    }

    pub fn get_output(&mut self) -> Vec<enums::VMType> {
        let mut output = Vec::new();

        while let Some(next) = self.vm.pop() {
            output.push(next)
        }

        output.reverse();

        output
            .iter()
            .for_each(|value| self.vm.push(value.to_owned()));

        output
    }
}
