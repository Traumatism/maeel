use crate::token::Token;

const INDENT: &str = "    ";

pub fn format(tokens: Vec<Token>) -> String {
    let mut output = String::new();
    let mut indents = 0;

    let mut tokens_stack = tokens.into_iter().rev().collect::<Vec<Token>>();

    while let Some(token) = tokens_stack.pop() {
        match token {
            Token::Newline => (),

            Token::BlockStart => {
                if !output.ends_with('\n') {
                    output.push('\n');
                }

                output.push_str(&INDENT.repeat(indents));
                output.push_str("do");

                indents += 1;
            }

            Token::Block(tokens_d1) => {
                output.push_str(&format(tokens_d1.clone()));
            }

            Token::BlockEnd => {
                indents -= 1;
                if !output.ends_with('\n') {
                    output.push('\n');
                }

                output.push_str(&INDENT.repeat(indents));
                output.push_str("end\n")
            }

            Token::Str(content) => {
                if !output.ends_with('\n') {
                    output.push('\n');
                }

                output.push_str(&INDENT.repeat(indents));
                output.push_str(&format!("{:?}", content));
            }

            Token::Integer(content) => {
                if !output.ends_with('\n') {
                    output.push('\n');
                }

                output.push_str(&INDENT.repeat(indents));
                output.push_str(&format!("{}", content));
            }

            Token::Identifier(content) => {
                if !output.ends_with('\n') {
                    output.push('\n');
                }

                output.push_str(&INDENT.repeat(indents));
                output.push_str(&content);
            }

            Token::Float(content) => {
                if !output.ends_with('\n') {
                    output.push('\n');
                }

                output.push_str(&INDENT.repeat(indents));
                output.push_str(&format!("{}", content));
            }

            Token::Bool(content) => {
                if !output.ends_with('\n') {
                    output.push('\n');
                }

                output.push_str(&format!("{}", content));
                output.push_str(&INDENT.repeat(indents));
            }

            Token::ProcStart => {
                if !output.ends_with('\n') {
                    output.push('\n');
                }

                output.push_str(&INDENT.repeat(indents));
                output.push_str("proc ");
            }

            Token::Let => {
                if !output.ends_with('\n') {
                    output.push('\n');
                }

                let identifier = match tokens_stack.pop() {
                    Some(Token::Identifier(identifier)) => identifier,
                    _ => panic!(),
                };

                let value = match tokens_stack.pop() {
                    Some(Token::Pop) => String::from("pop"),
                    Some(Token::Dup) => String::from("dup"),
                    Some(Token::Over) => String::from("over"),
                    Some(Token::Str(content)) => format!("{:?}", content),
                    Some(Token::Integer(content)) => format!("{}", content),
                    Some(Token::Float(content)) => format!("{}", content),
                    Some(Token::Bool(content)) => format!("{}", content),
                    _ => panic!(),
                };

                output = match value.as_str() {
                    "pop" | "dup" | "value" => String::from(&output[0..output.len() - 1]) + " ",
                    _ => output,
                };

                output.push_str(&format!("let {} {}\n", identifier, value))
            }

            Token::Sub => output.push_str("- "),
            Token::Add => output.push_str("+ "),
            Token::Mul => output.push_str("* "),
            Token::Mod => output.push_str("% "),
            Token::Div => output.push_str("/ "),
            Token::Not => output.push_str("! "),
            Token::Eq => output.push_str("= "),
            Token::Gt => output.push_str("> "),
            Token::Lt => output.push_str("< "),

            Token::Clear => output.push_str(" clear "),
            Token::Over => output.push_str(" over "),
            Token::Take => output.push_str(" take "),
            Token::Swap => output.push_str(" swap "),
            Token::Del => output.push_str(" del "),
            Token::Dup => output.push_str(" dup "),
            Token::Pop => output.push_str(" pop "),
            Token::Return => output.push_str(" return "),

            Token::If => output.push_str(" if "),
            Token::For => output.push_str(" for "),
            Token::While => output.push_str(" while "),
        }
    }

    output
}
