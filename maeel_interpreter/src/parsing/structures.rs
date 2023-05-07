use super::super::{
    parse_identifiers_list,
    Stack,
    StructuresRegistry,
};

use maeel_common::{
    tokens::Token,
    vmtypes::VMType,
};

use std::slice::Iter;

/// Parse a structure definition/call
pub fn parse_struct<'a>(
    tokens: &'a mut Iter<Token>,
    data: &'a mut Stack,
    structs: &'a mut StructuresRegistry,
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
