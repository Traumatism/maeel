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

    let procedure_block = match tokens.next() {
        Some(Token::Block(procedure_block)) => {
            procedure_block.clone()
        }
        Some(Token::IStart) => {
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

            procedure_block.append(&mut next!(tokens, "block"));
            procedure_block
        }
        _ => panic!(),
    };

    procs.insert(name, procedure_block);
}
