use super::super::{
    process_tokens,
    next,
};

use maeel_common::{
    tokens::Token,
    vmtypes::VMType,
};

use std::collections::HashMap;
use std::slice::Iter;

type Stack = Vec<VMType>;
type VariablesRegistery = HashMap<String, VMType>;
type ProceduresRegistery = HashMap<String, Vec<Token>>;
type StructuresRegistery = HashMap<String, Vec<String>>;

pub fn parse_interval<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Stack,
)
{
    let (
        Some(Token::Integer(start)),
        Some(Token::Integer(end))
    ) = (tokens.next(), tokens.next()) else {
        panic!()
    };

    assert_eq!(Some(&Token::IEnd), tokens.next());

    data.push(VMType::Array(
        (*start..*end)
            .map(VMType::Integer)
            .collect(),
    ));
}

/// Parses and executes a code block if a certain condition is true.
pub fn parse_array<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Stack,
    globals: &'a mut VariablesRegistery,
    procs: &'a mut ProceduresRegistery,
    structs: &'a mut StructuresRegistery,
)
{
    let mut array = Vec::default();

    loop {
        match tokens.next().unwrap().clone() {
            Token::ArrayEnd => break,

            Token::ArrayStart => {
                panic!()
            }

            Token::Str(value) => array.push(VMType::Str(value)),

            Token::Integer(value) => {
                array.push(VMType::Integer(value))
            }

            Token::Float(value) => array.push(VMType::Float(value)),

            Token::Bool(value) => array.push(VMType::Bool(value)),

            Token::Identifier(identifier) => {
                match globals.get(&identifier) {
                    Some(value) => array.push(value.clone()),
                    None => panic!(),
                }
            }

            Token::Block(expr) => {
                let generator = process_tokens(
                    &mut next!(tokens, "block").iter(),
                    data,
                    globals,
                    procs,
                    structs,
                )
                .unwrap()
                .pop();

                let Some(VMType::Array(xs)) = generator else {
                    panic!()
                };

                for element in xs {
                    let mut tmp_data = vec![element];

                    let output = process_tokens(
                        &mut expr.iter(),
                        &mut tmp_data,
                        globals,
                        procs,
                        structs,
                    )
                    .unwrap()
                    .pop();

                    array.push(output.unwrap());
                }
            }

            _ => panic!(),
        }
    }

    data.push(VMType::Array(array))
}
