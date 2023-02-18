use crate::lexer::Token;

const INDENT: &str = "    ";

macro_rules! rm_last_char {
    ($string:expr) => {
        String::from(&$string[0..$string.len() - 1])
    };
}

macro_rules! rm_if_present {
    ($string:expr, $chr:expr) => {
        if $string.ends_with($chr) {
            rm_last_char!($string)
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

macro_rules! rm_line {
    ($string:expr) => {
        add_if_missing!(rm_if_present!($string.clone(), "\n"), " ")
    };
}

macro_rules! jmp_line {
    ($string:expr, $indents:expr) => {
        add_indents!(add_if_missing!($string.clone(), "\n"), $indents)
    };
}

pub fn format(tokens: Vec<Token>) -> String {
    let mut output = String::new();
    let mut indents = 0;

    let mut tokens_stack = tokens.into_iter().rev().collect::<Vec<Token>>();
    let mut last_tokens_stack = Vec::new();

    while let Some(token) = tokens_stack.pop() {
        last_tokens_stack.push(token.clone());

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
                let if_token = last_tokens_stack.pop().unwrap();
                let token_before_if = last_tokens_stack.pop();

                last_tokens_stack.push(if_token);

                output = if let Some(token_d1) = token_before_if {
                    last_tokens_stack.push(token_d1.clone());

                    match token_d1 {
                        Token::Sub
                        | Token::Add
                        | Token::Mul
                        | Token::Mod
                        | Token::Div
                        | Token::Not
                        | Token::Eq
                        | Token::Gt
                        | Token::Lt => rm_line!(output),
                        Token::BlockStart => jmp_line!(output, indents),
                        _ => output,
                    }
                } else {
                    jmp_line!(output, indents)
                };

                output.push_str("if")
            }

            Token::For => {
                let for_token = last_tokens_stack.pop().unwrap();
                let token_before_for = last_tokens_stack.pop();

                last_tokens_stack.push(for_token);

                output = if let Some(token_d1) = token_before_for {
                    last_tokens_stack.push(token_d1.clone());

                    match token_d1 {
                        Token::Sub
                        | Token::Add
                        | Token::Mul
                        | Token::Mod
                        | Token::Div
                        | Token::Not
                        | Token::Eq
                        | Token::Gt
                        | Token::Lt => rm_line!(output),
                        Token::BlockStart => jmp_line!(output, indents),
                        _ => output,
                    }
                } else {
                    jmp_line!(output, indents)
                };

                output.push_str("for")
            }

            Token::While => {
                let while_token = last_tokens_stack.pop().unwrap();
                let token_before_while = last_tokens_stack.pop();

                last_tokens_stack.push(while_token);

                output = if let Some(token_d1) = token_before_while {
                    last_tokens_stack.push(token_d1.clone());

                    match token_d1 {
                        Token::Sub
                        | Token::Add
                        | Token::Mul
                        | Token::Mod
                        | Token::Div
                        | Token::Not
                        | Token::Eq
                        | Token::Gt
                        | Token::Lt => rm_line!(output),
                        Token::BlockStart => jmp_line!(output, indents),
                        _ => output,
                    }
                } else {
                    jmp_line!(output, indents)
                };

                output.push_str("while")
            }

            Token::Let => {
                let let_token = last_tokens_stack.pop().unwrap();
                let token_before_let = last_tokens_stack.pop();

                last_tokens_stack.push(let_token);

                if let Some(token_d1) = token_before_let {
                    last_tokens_stack.push(token_d1.clone());

                    output = match token_d1 {
                        Token::Sub
                        | Token::Add
                        | Token::Mul
                        | Token::Mod
                        | Token::Div
                        | Token::Not
                        | Token::Eq
                        | Token::Gt
                        | Token::Lt => rm_line!(output),
                        Token::BlockStart => jmp_line!(output, indents),
                        _ => {
                            jmp_line!(output, indents)
                        }
                    }
                } else {
                    output = add_if_missing!(output, "\n");
                    output = add_indents!(output, indents);
                }

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
                let identifier_token = last_tokens_stack.pop().unwrap();
                let token_before_identifier = last_tokens_stack.pop();

                last_tokens_stack.push(identifier_token);

                if let Some(Token::ProcStart) = token_before_identifier {
                    last_tokens_stack.push(Token::ProcStart);

                    output = rm_if_present!(output, "\n");
                    output = add_if_missing!(output, " ");
                } else {
                    output = add_if_missing!(output, "\n");
                    output = add_indents!(output, indents);
                }

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
                let operator_token = last_tokens_stack.pop().unwrap();
                let token_before_operator = last_tokens_stack.pop();

                last_tokens_stack.push(operator_token);

                if let Some(token_d1) = token_before_operator {
                    last_tokens_stack.push(token_d1.clone());

                    match token_d1 {
                        Token::Str(_) => output = rm_line!(output),
                        Token::Integer(_) => output = rm_line!(output),
                        Token::Identifier(_) => output = rm_line!(output),
                        Token::Float(_) => output = rm_line!(output),
                        Token::Bool(_) => output = rm_line!(output),
                        _ => {
                            output = add_if_missing!(output, "\n");
                            output = add_indents!(output, indents);
                        }
                    }
                } else {
                    output = add_if_missing!(output, "\n");
                    output = add_indents!(output, indents);
                }

                output.push('-');
                output = add_if_missing!(output, " ")
            }

            Token::Add => {
                let operator_token = last_tokens_stack.pop().unwrap();
                let token_before_operator = last_tokens_stack.pop();

                last_tokens_stack.push(operator_token);

                output = if let Some(token_d1) = token_before_operator {
                    last_tokens_stack.push(token_d1.clone());

                    match token_d1 {
                        Token::Str(_) => rm_line!(output),
                        Token::Integer(_) => rm_line!(output),
                        Token::Identifier(_) => rm_line!(output),
                        Token::Float(_) => rm_line!(output),
                        Token::Bool(_) => rm_line!(output),
                        _ => jmp_line!(output, indents),
                    }
                } else {
                    jmp_line!(output, indents)
                };

                output.push('+');
                output = add_if_missing!(output, " ")
            }

            Token::Mul => {
                let operator_token = last_tokens_stack.pop().unwrap();
                let token_before_operator = last_tokens_stack.pop();

                last_tokens_stack.push(operator_token);

                output = if let Some(token_d1) = token_before_operator {
                    last_tokens_stack.push(token_d1.clone());

                    match token_d1 {
                        Token::Str(_) => rm_line!(output),
                        Token::Integer(_) => rm_line!(output),
                        Token::Identifier(_) => rm_line!(output),
                        Token::Float(_) => rm_line!(output),
                        Token::Bool(_) => rm_line!(output),
                        _ => jmp_line!(output, indents),
                    }
                } else {
                    jmp_line!(output, indents)
                };

                output.push('*');
                output = add_if_missing!(output, " ")
            }

            Token::Mod => {
                let operator_token = last_tokens_stack.pop().unwrap();
                let token_before_operator = last_tokens_stack.pop();

                last_tokens_stack.push(operator_token);

                output = if let Some(token_d1) = token_before_operator {
                    last_tokens_stack.push(token_d1.clone());

                    match token_d1 {
                        Token::Str(_) => rm_line!(output),
                        Token::Integer(_) => rm_line!(output),
                        Token::Identifier(_) => rm_line!(output),
                        Token::Float(_) => rm_line!(output),
                        Token::Bool(_) => rm_line!(output),
                        _ => jmp_line!(output, indents),
                    }
                } else {
                    jmp_line!(output, indents)
                };

                output.push('%');
                output = add_if_missing!(output, " ")
            }

            Token::Div => {
                let operator_token = last_tokens_stack.pop().unwrap();
                let token_before_operator = last_tokens_stack.pop();

                last_tokens_stack.push(operator_token);

                output = if let Some(token_d1) = token_before_operator {
                    last_tokens_stack.push(token_d1.clone());

                    match token_d1 {
                        Token::Str(_) => rm_line!(output),
                        Token::Integer(_) => rm_line!(output),
                        Token::Identifier(_) => rm_line!(output),
                        Token::Float(_) => rm_line!(output),
                        Token::Bool(_) => rm_line!(output),
                        _ => jmp_line!(output, indents),
                    }
                } else {
                    jmp_line!(output, indents)
                };

                output.push('/');
                output = add_if_missing!(output, " ")
            }

            Token::Not => {
                let operator_token = last_tokens_stack.pop().unwrap();
                let token_before_operator = last_tokens_stack.pop();

                last_tokens_stack.push(operator_token);

                output = if let Some(token_d1) = token_before_operator {
                    last_tokens_stack.push(token_d1.clone());

                    match token_d1 {
                        Token::Str(_) => rm_line!(output),
                        Token::Integer(_) => rm_line!(output),
                        Token::Identifier(_) => rm_line!(output),
                        Token::Float(_) => rm_line!(output),
                        Token::Bool(_) => rm_line!(output),
                        _ => jmp_line!(output, indents),
                    }
                } else {
                    jmp_line!(output, indents)
                };

                output.push('!');
                output = add_if_missing!(output, " ")
            }

            Token::Eq => {
                let operator_token = last_tokens_stack.pop().unwrap();
                let token_before_operator = last_tokens_stack.pop();

                last_tokens_stack.push(operator_token);

                output = if let Some(token_d1) = token_before_operator {
                    last_tokens_stack.push(token_d1.clone());

                    match token_d1 {
                        Token::Str(_) => rm_line!(output),
                        Token::Integer(_) => rm_line!(output),
                        Token::Identifier(_) => rm_line!(output),
                        Token::Float(_) => rm_line!(output),
                        Token::Bool(_) => rm_line!(output),
                        _ => jmp_line!(output, indents),
                    }
                } else {
                    jmp_line!(output, indents)
                };

                output.push('=');
                output = add_if_missing!(output, " ")
            }

            Token::Gt => {
                let operator_token = last_tokens_stack.pop().unwrap();
                let token_before_operator = last_tokens_stack.pop();

                last_tokens_stack.push(operator_token);

                output = if let Some(token_d1) = token_before_operator {
                    last_tokens_stack.push(token_d1.clone());

                    match token_d1 {
                        Token::Str(_) => rm_line!(output),
                        Token::Integer(_) => rm_line!(output),
                        Token::Identifier(_) => rm_line!(output),
                        Token::Float(_) => rm_line!(output),
                        Token::Bool(_) => rm_line!(output),
                        _ => jmp_line!(output, indents),
                    }
                } else {
                    jmp_line!(output, indents)
                };

                output.push('>');
                output = add_if_missing!(output, " ")
            }

            Token::Lt => {
                let operator_token = last_tokens_stack.pop().unwrap();
                let token_before_operator = last_tokens_stack.pop();

                last_tokens_stack.push(operator_token);

                output = if let Some(token_d1) = token_before_operator {
                    last_tokens_stack.push(token_d1.clone());

                    match token_d1 {
                        Token::Str(_) => rm_line!(output),
                        Token::Integer(_) => rm_line!(output),
                        Token::Identifier(_) => rm_line!(output),
                        Token::Float(_) => rm_line!(output),
                        Token::Bool(_) => rm_line!(output),
                        _ => jmp_line!(output, indents),
                    }
                } else {
                    jmp_line!(output, indents)
                };

                output.push('<');
                output = add_if_missing!(output, " ")
            }

            Token::Clear => {
                output = jmp_line!(output, indents);
                output.push_str("clear");
            }

            Token::Over => {
                output = jmp_line!(output, indents);
                output.push_str("over");
            }

            Token::Take => {
                output = jmp_line!(output, indents);
                output.push_str("take");
            }

            Token::Swap => {
                output = jmp_line!(output, indents);
                output.push_str("swap");
            }

            Token::Del => {
                output = jmp_line!(output, indents);
                output.push_str("del");
            }

            Token::Dup => {
                output = jmp_line!(output, indents);
                output.push_str("dup");
            }

            Token::Pop => {
                output = jmp_line!(output, indents);
                output.push_str("pop");
            }

            Token::ProcStart => {
                output = jmp_line!(output, indents);
                output.push_str("proc");
            }

            Token::Return => {
                output = jmp_line!(output, indents);
                output.push_str("return");
            }
        }
    }

    output
}
