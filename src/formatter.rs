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

macro_rules! one_arg_operator {
    ($operator:expr, $last_tokens_stack:expr, $output:expr, $indents:expr) => {{
        let operator_token = $last_tokens_stack.pop().unwrap();
        let token_before_operator = $last_tokens_stack.pop();

        $last_tokens_stack.push(operator_token);

        $output = if let Some(token_d1) = token_before_operator {
            $last_tokens_stack.push(token_d1.clone());

            match token_d1 {
                Token::Str(_) => rm_line!($output),
                Token::Integer(_) => rm_line!($output),
                Token::Identifier(_) => rm_line!($output),
                Token::Float(_) => rm_line!($output),
                Token::Bool(_) => rm_line!($output),
                _ => jmp_line!($output, $indents),
            }
        } else {
            jmp_line!($output, $indents)
        };

        $output.push($operator);
        $output = add_if_missing!($output, " ")
    }};
}

macro_rules! zero_arg_keyword {
    ($keyword:expr, $output:expr, $indents:expr) => {{
        $output = jmp_line!($output, $indents);
        $output.push_str($keyword);
    }};
}

macro_rules! one_arg_keyword {
    ($keyword:expr, $last_tokens_stack:expr, $output:expr, $indents:expr) => {{
        let keyword_token = $last_tokens_stack.pop().unwrap();
        let token_before_keyword = $last_tokens_stack.pop();

        $last_tokens_stack.push(keyword_token);

        $output = if let Some(token_d1) = token_before_keyword {
            $last_tokens_stack.push(token_d1.clone());

            match token_d1 {
                Token::Sub
                | Token::Add
                | Token::Mul
                | Token::Mod
                | Token::Div
                | Token::Not
                | Token::Eq
                | Token::Gt
                | Token::Lt => rm_line!($output),
                _ => jmp_line!($output, $indents),
            }
        } else {
            jmp_line!($output, $indents)
        };

        $output.push_str($keyword)
    }};
}

pub fn format(tokens: Vec<Token>) -> String {
    let mut output = String::new();
    let mut indents = 0;

    let mut tokens_stack = tokens.into_iter().rev().collect::<Vec<Token>>();
    let mut last_tokens_stack = Vec::new();

    while let Some(token) = tokens_stack.pop() {
        last_tokens_stack.push(token.clone());

        match token {
            Token::ProcStart => {
                output = jmp_line!(output, indents);
                output.push_str("proc");
            }

            Token::BlockStart => {
                output = rm_line!(output);
                output.push_str("do");
                indents += 1;
            }

            Token::Block(tokens_d1) => output.push_str(&format(tokens_d1.clone())),

            Token::BlockEnd => {
                indents -= 1;
                output = jmp_line!(output, indents);
                output.push_str("end\n")
            }

            Token::Pop => zero_arg_keyword!("pop", output, indents),
            Token::Dup => zero_arg_keyword!("dup", output, indents),
            Token::Over => zero_arg_keyword!("over", output, indents),
            Token::Take => zero_arg_keyword!("take", output, indents),
            Token::Swap => zero_arg_keyword!("swap", output, indents),
            Token::Clear => zero_arg_keyword!("clear", output, indents),
            Token::Return => zero_arg_keyword!("return", output, indents),

            Token::If => one_arg_keyword!("if", last_tokens_stack, output, indents),
            Token::For => one_arg_keyword!("for", last_tokens_stack, output, indents),
            Token::While => one_arg_keyword!("while", last_tokens_stack, output, indents),

            Token::Eq => one_arg_operator!('=', last_tokens_stack, output, indents),
            Token::Gt => one_arg_operator!('>', last_tokens_stack, output, indents),
            Token::Lt => one_arg_operator!('<', last_tokens_stack, output, indents),
            Token::Sub => one_arg_operator!('-', last_tokens_stack, output, indents),
            Token::Add => one_arg_operator!('+', last_tokens_stack, output, indents),
            Token::Mul => one_arg_operator!('*', last_tokens_stack, output, indents),
            Token::Mod => one_arg_operator!('%', last_tokens_stack, output, indents),
            Token::Div => one_arg_operator!('/', last_tokens_stack, output, indents),
            Token::Not => one_arg_operator!('!', last_tokens_stack, output, indents),

            Token::Let => {
                one_arg_keyword!("let", last_tokens_stack, output, indents);

                output.push(' ');

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

            Token::Float(content) => {
                output = jmp_line!(output, indents);
                output.push_str(&content.to_string());
                output.push('\n')
            }

            Token::Bool(content) => {
                output = jmp_line!(output, indents);
                output.push_str(&content.to_string());
                output.push('\n')
            }

            Token::Str(content) => {
                output = jmp_line!(output, indents);
                output.push_str(&format!("{:?}", content));
                output.push('\n')
            }

            Token::Integer(content) => {
                output = jmp_line!(output, indents);
                output.push_str(&content.to_string());
                output.push('\n')
            }

            Token::Identifier(content) => {
                let identifier_token = last_tokens_stack.pop().unwrap();
                let token_before_identifier = last_tokens_stack.pop();

                last_tokens_stack.push(identifier_token);

                if let Some(Token::ProcStart) = token_before_identifier {
                    last_tokens_stack.push(Token::ProcStart);
                    output = rm_line!(output)
                } else {
                    output = jmp_line!(output, indents);
                }

                output.push_str(&content);
                output.push('\n')
            }
        }
    }

    output
}
