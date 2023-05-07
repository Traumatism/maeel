use maeel_common::{
    tokens::Token,
    vmtypes::VMType,
};

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

        Some(Token::At) => {
            match (tokens.next(), tokens.next()) {
                (
                    Some(Token::Identifier(struct_variable_name)),
                    Some(Token::Identifier(field_name)),
                ) => {
                    let (struct_name, mut struct_fields) =
                        match globals
                            .get(struct_variable_name)
                            .unwrap()
                        {
                            VMType::Struct((name, fields)) => {
                                (name, fields.clone())
                            }
                            _ => panic!(),
                        };

                    struct_fields.insert(
                        field_name.clone(),
                        data.pop().unwrap(),
                    );

                    globals.insert(
                        struct_variable_name.to_string(),
                        VMType::Struct((
                            struct_name.clone(),
                            struct_fields,
                        )),
                    );
                }
                _ => panic!(),
            }
        }
        _ => panic!(),
    }
}
