use common::{tokens::Token,};

use super::super::{
    Stack,
    VariablesRegistry,
};

use std::slice::Iter;

/// Parses an assignment statement and stores the assigned value in either a local or
/// global variable depending on the variable name.
pub fn parse_assignment<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Stack,
    locals: &'a mut VariablesRegistry,
    globals: &'a mut VariablesRegistry,
)
{
    match tokens.next() {
        Some(Token::Identifier(name)) => {
            match name
                .chars()
                .collect::<Vec<char>>()
                .first()
            {
                Some('_') => {
                    locals.insert(name.clone(), data.pop().unwrap())
                }

                Some(_) => {
                    globals.insert(name.clone(), data.pop().unwrap())
                }
                None => panic!(),
            };
        }

        _ => panic!(),
    }
}
