use super::super::{
    next,
    parse_identifiers_list,
};

use maeel_common::{tokens::Token,};

use std::collections::HashMap;
use std::slice::Iter;

type ProceduresRegistery = HashMap<String, Vec<Token>>;

/// Parses a procedure definition.
pub fn parse_proc<'a>(
    tokens: &'a mut Iter<Token>,
    procs: &'a mut ProceduresRegistery,
)
{
    // Procedure name
    let name = next!(tokens, "identifier");

    assert_eq!(Some(&Token::IStart), tokens.next());

    let mut procedure_block = Vec::default();

    for identifier in parse_identifiers_list!(tokens) {
        procedure_block.append(&mut vec![
            Token::Let,
            Token::Identifier(identifier),
        ])
    }

    // Finally append real procedure tokens
    procedure_block.append(&mut next!(tokens, "block"));

    procs.insert(name, procedure_block);
}
