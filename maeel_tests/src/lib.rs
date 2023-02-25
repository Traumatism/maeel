use maeel_lexer::Token;
use maeel_lexer::{extract_blocks, lex_into_tokens};

#[cfg(test)]
mod maeel_lexer_tests {
    use super::*;

    #[test]
    fn lex_blocks() {
        let expression = "do do do end end end";
        let tokens = extract_blocks(&lex_into_tokens(expression));

        assert_eq!(
            tokens,
            vec![Token::Block(vec![Token::Block(vec![Token::Block(vec![])])])]
        );
    }

    #[test]
    fn lex_names() {
        let expression = "PascalCase camelCase snake_case Pascal1Case camel2Case snake_3case";
        let tokens = lex_into_tokens(expression);

        assert_eq!(
            tokens,
            vec![
                Token::Identifier(String::from("PascalCase")),
                Token::Identifier(String::from("camelCase")),
                Token::Identifier(String::from("snake_case")),
                Token::Identifier(String::from("Pascal1Case")),
                Token::Identifier(String::from("camel2Case")),
                Token::Identifier(String::from("snake_3case")),
            ]
        );
    }

    #[test]
    fn lex_symbols() {
        let expression = "= > < ! - + * / %";
        let tokens = lex_into_tokens(expression);

        assert_eq!(
            tokens,
            vec![
                Token::Eq,
                Token::Gt,
                Token::Lt,
                Token::Not,
                Token::Sub,
                Token::Add,
                Token::Mul,
                Token::Div,
                Token::Mod,
            ]
        );
    }
}
