use crate::vmtypes::VMType;

#[derive(Clone, Debug)]
pub struct InstructionData {
    pub instruction: Instruction,
    pub line: u16,
    pub pos: u16,
}

impl InstructionData {
    pub fn new(instruction: Instruction, line: u16, pos: u16) -> Self {
        Self {
            instruction,
            line,
            pos,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Pass,
    Push(VMType),

    Take,
    Get,

    Identifier(String),

    VarDef(String, Box<InstructionData>),
    ProcDef(String, Vec<InstructionData>),

    If(Vec<InstructionData>),
    While(Vec<InstructionData>),
    For(Vec<InstructionData>),

    Ptr,
    Argv,
    Print,
    Syscall,

    Pop,
    Rot,
    Dup,
    Swap,
    Clear,
    Over,

    Sub,
    Add,
    Mul,
    Mod,
    Div,

    Not,

    Eq,
    Gt,
    Lt,
}
