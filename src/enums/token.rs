/// All tokens lexer/parser can understand
#[derive(Debug, Clone)]
pub enum Token {
    Str(String, u16),        // "hello world"
    Integer(i64, u16),       // 123
    Identifier(String, u16), // helloworld
    Float(f64, u16),         // 123.4
    Bool(bool, u16),         // true|false
    Let(u16),                // let
    If(u16),                 // if
    For(u16),                // for
    While(u16),              // while
    Modulo(u16),             // %
    Add(u16),                // +
    Sub(u16),                // -
    Mul(u16),                // *
    Div(u16),                // /
    And(u16),                // &
    Or(u16),                 // |
    Xor(u16),                // ^
    Not(u16),                // !
    Eq(u16),                 // =
    Gt(u16),                 // >
    Lt(u16),                 // <
    Del(u16),                // del
    Take(u16),               // take
    Reverse(u16),            // reverse
    Clear(u16),              // clear
    Dup(u16),                // dup
    Swap(u16),               // swap
    Pop(u16),                // pop
    Return(u16),             // return
    ProcStart(u16),          // proc
    BlockStart(u16),
    BlockEnd(u16),
    Block(Vec<Token>, u16),
}
