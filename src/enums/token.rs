/// All tokens lexer/parser can understand
#[derive(Debug, Clone)]
pub enum Token {
    Str(String),        // "hello world"
    Integer(i64),       // 123
    Identifier(String), // helloworld
    Float(f64),         // 123.4
    Bool(bool),         // true|false
    Let,                // let
    If,                 // if
    For,                // for
    Modulo,             // %
    Add,                // +
    Sub,                // -
    Mul,                // *
    Div,                // /
    And,                // &
    Or,                 // |
    Xor,                // ^
    Not,                // !
    Eq,                 // =
    Del,                // del
    Take,               // take
    Reverse,            // reverse
    Clear,              // clear
    Dup,                // dup
    Swap,               // swap
    Pop,                // pop
    Return,             // return
    ProcStart,          // proc
    BlockStart,
    BlockEnd,
    Block(Vec<Token>),
}
