use super::super::parse_identifiers_list;

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

/// Parse a structure definition/call
pub fn parse_struct<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Stack,
    structs: &'a mut StructuresRegistery,
)
{
    match tokens.next() {
        Some(Token::Identifier(struct_field)) => {
            let Some(VMType::Struct((_, top_struct))) = data.pop() else {
                panic!()
            };

            data.push(
                top_struct
                    .get(struct_field)
                    .unwrap()
                    .clone(),
            )
        }

        Some(Token::At) => {
            let Some(Token::Identifier(struct_name)) = tokens.next() else {
                panic!()
            };

            assert_eq!(Some(&Token::IStart), tokens.next());

            let struct_fields = parse_identifiers_list!(tokens);

            structs.insert(struct_name.to_string(), struct_fields);
        }
        _ => panic!(),
    };
}
