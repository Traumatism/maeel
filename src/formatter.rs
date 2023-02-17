#![allow(clippy::single_char_add_str)]

use crate::token::Token;

const INDENT: &str = "    ";

macro_rules! rm_last_char {
    ($string:expr) => {
        String::from(&$string[0..$string.len() - 1])
    };
}

macro_rules! rm_if_present {
    ($string:expr, $chr:expr) => {
        if $string.ends_with($chr) {
            String::from(&$string[0..$string.len() - 1])
        } else {
            $string
        }
    };
}

macro_rules! add_if_missing {
    ($string:expr, $chr:expr) => {
        if !$string.ends_with($chr) {
            $string + $chr
        } else {
            $string
        }
    };
}

macro_rules! add_indents {
    ($string:expr, $indents:expr) => {
        $string + &INDENT.repeat($indents)
    };
}

pub fn format(tokens: Vec<Token>) -> String {
    let mut output = String::new();
    let mut indents = 0;

    let mut tokens_stack = tokens.into_iter().rev().collect::<Vec<Token>>();

    while let Some(token) = tokens_stack.pop() {
        match token {
            Token::BlockStart => {
                output = rm_if_present!(output, "\n");
                output = add_if_missing!(output, " ");

                output.push_str("do");

                indents += 1;
            }

            Token::Block(tokens_d1) => {
                output.push_str(&format(tokens_d1.clone()));
            }

            Token::BlockEnd => {
                indents -= 1;

                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("end\n")
            }

            Token::If => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("if")
            }

            Token::For => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("for")
            }

            Token::While => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("while")
            }

            Token::Let => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("let ");

                match tokens_stack.pop().unwrap() {
                    Token::Identifier(content) => output.push_str(&content),
                    _ => panic!(),
                }

                output.push(' ');

                match tokens_stack.pop().unwrap() {
                    Token::Dup => output.push_str("dup"),
                    Token::Over => output.push_str("over"),
                    Token::Pop => output.push_str("pop"),
                    Token::Str(content) => output.push_str(&format!("{:?}", content)),
                    Token::Integer(content) => output.push_str(&content.to_string()),
                    Token::Float(content) => output.push_str(&content.to_string()),
                    Token::Bool(content) => output.push_str(&content.to_string()),
                    token => panic!("{token:?}"),
                }

                output.push('\n');
            }

            Token::Str(content) => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str(&format!("{:?}", content));
                output.push('\n')
            }

            Token::Integer(content) => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str(&content.to_string());
                output.push('\n')
            }

            Token::Identifier(content) => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str(&content);
                output.push('\n')
            }

            Token::Float(content) => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str(&content.to_string());
                output.push('\n')
            }

            Token::Bool(content) => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str(&content.to_string());
                output.push('\n')
            }

            Token::Sub => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("-");
            }

            Token::Add => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("+");
            }

            Token::Mul => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("*");
            }

            Token::Mod => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("%");
            }

            Token::Div => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("/");
            }

            Token::Not => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("!");
            }

            Token::Eq => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("=");
            }

            Token::Gt => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str(">");
            }

            Token::Lt => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("<");
            }

            Token::Clear => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("clear");
            }

            Token::Over => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("over");
            }

            Token::Take => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("take");
            }

            Token::Swap => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("swap");
            }

            Token::Del => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("del");
            }

            Token::Dup => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("dup");
            }

            Token::Pop => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("pop");
            }

            Token::ProcStart => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("proc");
            }

            Token::Return => {
                output = add_if_missing!(output, "\n");
                output = add_indents!(output, indents);

                output.push_str("return");
            }
        }
    }

    output
}
