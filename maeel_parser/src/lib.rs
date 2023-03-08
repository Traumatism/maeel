use maeel_common::instructions::{Instruction, InstructionData};
use maeel_common::tokens::{Token, TokenData};
use maeel_common::vmtypes::VMType;

use std::slice::Iter;

/// Handle a bunch of tokens
pub fn parse_tokens<'a>(tokens: &'a mut Iter<TokenData>) -> Vec<InstructionData> {
    let mut instructions = vec![];

    while let Some(token_data) = tokens.next() {
        let token = token_data.token.clone();
        let line = token_data.line.clone();
        let pos = token_data.pos.clone();

        let i = match token.clone() {
            Token::BlockStart | Token::BlockEnd => panic!(),
            Token::Include => Instruction::Pass,

            Token::Take => Instruction::Take,
            Token::Get => Instruction::Get,

            Token::Block(tokens) => {
                instructions.append(&mut parse_tokens(&mut tokens.iter()));
                Instruction::Pass
            }
            Token::Str(content) => Instruction::Push(VMType::Str(content)),
            Token::Bool(content) => Instruction::Push(VMType::Bool(content)),
            Token::Float(content) => Instruction::Push(VMType::Float(content)),
            Token::Integer(content) => Instruction::Push(VMType::Integer(content)),

            Token::Rot => Instruction::Rot,
            Token::Swap => Instruction::Swap,
            Token::Dup => Instruction::Dup,
            Token::Over => Instruction::Over,
            Token::Clear => Instruction::Clear,

            Token::Gt => Instruction::Gt,
            Token::Lt => Instruction::Lt,
            Token::Eq => Instruction::Eq,

            Token::Sub => Instruction::Sub,
            Token::Mul => Instruction::Mul,
            Token::Div => Instruction::Div,
            Token::Mod => Instruction::Mod,
            Token::Add => Instruction::Add,
            Token::Pop => Instruction::Pop,

            Token::Not => Instruction::Not,

            Token::ProcStart => {
                let next = tokens.next().unwrap();

                let identifier = match &next.token {
                    Token::Identifier(id) => id.clone(),
                    token => panic!("Expected block, got {:?}", token),
                };

                let next = tokens.next().unwrap();

                let tokens_b = match &next.token {
                    Token::Block(block) => block.to_vec(),
                    token => panic!("Expected block, got {:?}", token),
                };

                let instructions_b = parse_tokens(&mut tokens_b.iter());

                Instruction::ProcDef(identifier, instructions_b.clone())
            }

            Token::Identifier(identifier) => match identifier.as_str() {
                "ptr" => Instruction::Ptr,
                "argv" => Instruction::Argv,
                "syscall" => Instruction::Syscall,
                "print" => Instruction::Print,

                identifier => Instruction::Identifier(String::from(identifier)),
            },

            Token::Let => {
                let next = tokens.next().unwrap();

                let identifier = match &next.token {
                    Token::Identifier(id) => id.clone(),
                    token => panic!("Expected block, got {:?}", token),
                };

                let next = tokens.next().unwrap();

                let value = match &next.token {
                    Token::Str(content) => Instruction::Push(VMType::Str(content.clone())),
                    Token::Bool(content) => Instruction::Push(VMType::Bool(*content)),
                    Token::Float(content) => Instruction::Push(VMType::Float(*content)),
                    Token::Integer(content) => Instruction::Push(VMType::Integer(*content)),
                    Token::Pop => Instruction::Pop,
                    Token::Over => Instruction::Over,
                    Token::Dup => Instruction::Dup,
                    _ => panic!(),
                };

                Instruction::VarDef(identifier, Box::new(InstructionData::new(value, line, pos)))
            }
            Token::While => {
                let next = tokens.next().unwrap();

                let tokens_b = match &next.token {
                    Token::Block(block) => block.to_vec(),
                    token => panic!("Expected block, got {:?}", token),
                };

                let instructions_b = parse_tokens(&mut tokens_b.iter());

                Instruction::While(instructions_b)
            }
            Token::For => {
                let next = tokens.next().unwrap();

                let tokens_b = match &next.token {
                    Token::Block(block) => block.to_vec(),
                    token => panic!("Expected block, got {:?}", token),
                };

                let instructions_b = parse_tokens(&mut tokens_b.iter());

                Instruction::For(instructions_b)
            }
            Token::If => {
                let next = tokens.next().unwrap();

                let tokens_b = match &next.token {
                    Token::Block(block) => block.to_vec(),
                    token => panic!("Expected block, got {:?}", token),
                };

                let instructions_b = parse_tokens(&mut tokens_b.iter());

                Instruction::If(instructions_b)
            }
        };

        instructions.push(InstructionData::new(i, line, pos))
    }

    instructions
}
