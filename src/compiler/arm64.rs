use super::Compiler;

#[allow(non_camel_case_types)]
pub struct Arm64 {
    output: String,
}

#[allow(clippy::derivable_impls)]
impl Default for Arm64 {
    fn default() -> Self {
        let mut output = String::new();

        output.push_str(".section	__TEXT,__text,regular,pure_instructions\n");
        output.push_str(".globl	_main\n");
        output.push_str("_main:");

        Self { output }
    }
}

impl Compiler for Arm64 {
    fn handle_var_add(&mut self, _name: String, _value: crate::enums::vmtype::VMType) {
        todo!()
    }

    fn handle_var_del(&mut self, _name: String) {
        todo!()
    }

    fn handle_proc_add(&mut self, _name: String, _tokens: Vec<crate::enums::token::Token>) {
        todo!()
    }

    fn handle_push_str(&mut self, _content: String) {
        todo!()
    }

    fn handle_push_int(&mut self, _content: i64) {
        todo!()
    }

    fn handle_push_float(&mut self, _content: f64) {
        todo!()
    }

    fn handle_push_bool(&mut self, _content: bool) {
        todo!()
    }

    fn handle_pop(&mut self) -> Option<crate::enums::vmtype::VMType> {
        todo!()
    }

    fn handle_dup(&mut self) {
        todo!()
    }

    fn handle_swap(&mut self) {
        todo!()
    }

    fn handle_clear(&mut self) {
        todo!()
    }

    fn handle_add(&mut self, _line: u16) {
        todo!()
    }

    fn handle_mul(&mut self, _line: u16) {
        todo!()
    }

    fn handle_sub(&mut self, _line: u16) {
        todo!()
    }

    fn handle_or(&mut self, _line: u16) {
        todo!()
    }

    fn handle_xor(&mut self, _line: u16) {
        todo!()
    }

    fn handle_and(&mut self, _line: u16) {
        todo!()
    }

    fn handle_not(&mut self, _line: u16) {
        todo!()
    }

    fn handle_eq(&mut self, _line: u16) {
        todo!()
    }

    fn handle_return(&mut self, _line: u16) {
        todo!()
    }

    fn handle_take(&mut self, _line: u16) {
        todo!()
    }

    fn handle_reverse(&mut self, _line: u16) {
        todo!()
    }

    fn handle_identifier(&mut self, _identifier: &str, _line: u16) {
        todo!()
    }

    fn handle_modulo(&mut self, line: u16) {
        todo!()
    }

    fn handle_div(&mut self, line: u16) {
        todo!()
    }
}
