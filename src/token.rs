pub type Block = Vec<Token>;

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Str(String),
    Integer(i64),
    Identifier(String),
    Float(f64),
    Bool(bool),

    Sub,
    Add,
    Mul,
    Modulo,
    Div,
    DivQ,

    And,
    Or,
    Xor,
    Not,

    Eq,
    Gt,
    Lt,

    Rotate,
    Clear,
    Over,
    Take,
    Swap,
    Del,
    Dup,
    Pop,
    Len,

    Let,
    ProcStart,
    Return,

    BlockStart,
    Block(Block),
    BlockEnd,

    If,
    For,
    While,
}
