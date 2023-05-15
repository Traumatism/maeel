use common::tokens::Token;
use super::super::{
    next,
    parse_identifiers_list,
    ProceduresRegistry,
};

use std::slice::Iter;

/// Parses a procedure definition.
pub fn parse_proc<'a>(
    tokens: &'a mut Iter<Token>,
    procs: &'a mut ProceduresRegistry,
)
{
    // Procedure name
    let name = next!(tokens, "identifier");

    assert_eq!(Some(&Token::IStart), tokens.next());

    // Procedure block
    let mut procedure_block = Vec::default();

    let mut id_list = parse_identifiers_list!(tokens);

    id_list.reverse();

    id_list
        .iter()
        .for_each(|identifier| {
            procedure_block.append(&mut vec![
                Token::Let,
                Token::Identifier(identifier.clone()),
            ])
        });

    // Finally append real procedure tokens
    procedure_block.append(&mut next!(tokens, "block"));

    procs.insert(name, procedure_block);
}
