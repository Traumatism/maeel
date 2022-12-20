#[derive(Debug)]
pub enum Token {
    Separator,
    String(String, u16),
    Integer(i64, u16),
    Float(f64, u16),
    Identifier(String, u16),
}

#[derive(Debug)]
pub enum VMType {
    Float(f64),
    Integer(i64),
    String(String),
    Array(Vec<VMType>),
}
