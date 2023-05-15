use super::super::{
    process_tokens,
    next,
    Stack,
    ProceduresRegistry,
    VariablesRegistry,
};

use maeel_common::{
    tokens::Token,
    vmtypes::VMType,
};

use std::slice::Iter;

/// Executes a code block repeatedly while a certain condition is true.
pub fn parse_while<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Stack,
    globals: &'a mut VariablesRegistry,
    procs: &'a mut ProceduresRegistry,
)
{
    // Code block to execute while P(x) is true
    let tokens = next!(tokens, "block");

    // This is why we need to push P(x) at the end of the code block
    while let Some(VMType::Bool(true)) = data.pop() {
        process_tokens(
            "while_loop",
            &mut tokens.iter(),
            data,
            globals,
            procs,
        )
        .unwrap();
    }
}

/// Iterates through an array on the stack and executes a code block for each element.
pub fn parse_for<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Stack,
    globals: &'a mut VariablesRegistry,
    procs: &'a mut ProceduresRegistry,
)
{
    // Code block to execute for each value of L
    let tokens = next!(tokens, "block");

    match data.pop() {
        Some(VMType::Array(xs)) => {
            for element in xs {
                data.push(element);

                process_tokens(
                    "for_loop",
                    &mut tokens.iter(),
                    data,
                    globals,
                    procs,
                )
                .unwrap();
            }
        }

        Some(VMType::Str(string)) => {
            let xs = string
                .chars()
                .map(String::from)
                .collect::<Vec<String>>();

            for element in xs {
                data.push(VMType::Str(element));

                process_tokens(
                    "for_loop",
                    &mut tokens.iter(),
                    data,
                    globals,
                    procs,
                )
                .unwrap();
            }
        }

        data => panic!("{data:?}"),
    }
}
