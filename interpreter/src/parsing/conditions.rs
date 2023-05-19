use super::super::{next, process_tokens, ProceduresRegistry, Stack, VariablesRegistry};

use common::{tokens::Token, vmtypes::VMType};

use std::slice::Iter;

/// Parses and executes a code block if a certain condition is true.
pub fn parse_if<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Stack,
    globals: &'a mut VariablesRegistry,
    procs: &'a mut ProceduresRegistry,
) {
    // Code block to execute if, and only if P(x) is true
    let tokens = next!(tokens, "block");

    if let Some(VMType::Bool(true)) = data.pop() {
        process_tokens("if", &mut tokens.iter(), data, globals, procs).unwrap();
    }
}
