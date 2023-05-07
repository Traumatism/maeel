use super::super::{
    process_tokens,
    next,
    ProceduresRegistry,
    Stack,
    VariablesRegistry,
    StructuresRegistry,
};

use maeel_common::{
    tokens::Token,
    vmtypes::VMType,
};

use std::slice::Iter;

/// Parses and executes a code block if a certain condition is true.
pub fn parse_if<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Stack,
    globals: &'a mut VariablesRegistry,
    procs: &'a mut ProceduresRegistry,
    structs: &'a mut StructuresRegistry,
)
{
    // Code block to execute if, and only if P(x) is true
    let tokens = next!(tokens, "block");

    if let Some(VMType::Bool(true)) = data.pop() {
        process_tokens(
            &mut tokens.iter(),
            data,
            globals,
            procs,
            structs,
        )
        .unwrap();
    }
}
