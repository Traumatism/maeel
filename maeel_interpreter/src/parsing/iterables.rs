use super::super::{
    Stack,
    VariablesRegistry,
};

use maeel_common::{
    tokens::Token,
    vmtypes::VMType,
};

use std::slice::Iter;

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
    globals: &'a mut VariablesRegistry,
)
{
    let mut array = Vec::default();

    loop {
        match tokens.next().unwrap().clone() {
            Token::ArrayEnd => break,

            Token::ArrayStart => {
                parse_array(tokens, data, globals);
                array.push(data.pop().unwrap())
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
                array.push(VMType::Procedure(expr));
            }

            _ => panic!(),
        }
    }

    data.push(VMType::Array(array))
}
