/// The `Token` enum stores all tokens that the
/// lexer can identify
///
/// The `Clone` trait is implemented, which allows
/// the enum members to be easily copied and cloned.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Block(Vec<Token>),
    Str(String),
    Integer(i64),
    Identifier(String),
    Float(f64),
    Bool(bool),
    Sub,
    Add,
    Mul,
    Mod,
    Div,
    Not,
    Eq,
    Gt,
    Lt,
    Clear,
    Over,
    Take,
    Swap,
    Rot,
    Dup,
    Pop,
    Let,
    ProcStart,
    Return,
    BlockStart,
    BlockEnd,
    Include,
    If,
    For,
    While,
}
