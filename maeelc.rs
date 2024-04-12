/*
This is STILL in development phase !! (partially works)

This code was made without the help of any language
server and auto-formatter.

(Qualité Artisale)
*/

use std::fs::File;
use std::io::Read;

mod maeellex;

#[macro_use]
use maeellex::*;

macro_rules! expect_token {
    ($token:tt, $tokens:expr, $fl:expr, $line:expr) => {{
        match $tokens.pop() {
            | Some((Token::$token(value), _, _)) => value,
            | Some((other, file, line))          => emit_error!(file, line, format!("expected {:?}, got {other:?}", TokenRepr::$token)),
            | None                               => emit_error!($fl, $line, format!("expected {:?}, got EOF", TokenRepr::$token)),
        }
    }};
}

/* Types that are used by the VM */
#[derive(Debug, Clone)]
enum Cord {
    Flt(M_FLOAT_SIZE),
    Int(M_INT_SIZE),
    Fun(FunData),
    Str(String),
    Lst(Vec<Self>),
}

/* Used for error messages */
#[derive(Debug)] enum CordRepr { Flt, Int, Fun, Str, Lst }

#[derive(Debug)]
enum BinOp { Add, Sub, Mul, Div, Mod, Eq, Lt, Gt }

#[derive(Debug)]
enum BuiltIn { Puts, Get, Len }

#[derive(Debug)]
enum Instruction {
    BinOp(BinOp),
    Push(Cord),
    CallStack,
    CallIf(Vec<Instruction>),
    VarPush(String),
    BuiltInCall(BuiltIn),
    FunPush((String, Vec<Instruction>, bool)),
    FunCall(String),
    VarCall(String),
}

fn parse_tokens(
    tokens: &mut Vec<TokenData>,
    rev: bool,
) -> Vec<Instruction> {
    if rev /* Sometimes we might act like the tokens vec was a stack */ { tokens.reverse(); }

    let mut instructions = Vec::new();

    while let Some((token, file, line)) = tokens.pop() {
        match token {
            | Token::Sym(M_ADD!()) => instructions.push(Instruction::BinOp(BinOp::Add)),
            | Token::Sym(M_SUB!()) => instructions.push(Instruction::BinOp(BinOp::Sub)),
            | Token::Sym(M_MUL!()) => instructions.push(Instruction::BinOp(BinOp::Mul)),
            | Token::Sym(M_DIV!()) => instructions.push(Instruction::BinOp(BinOp::Div)),
            | Token::Sym(M_MOD!()) => instructions.push(Instruction::BinOp(BinOp::Mod)),
            | Token::Sym(M_EQ!()) => instructions.push(Instruction::BinOp(BinOp::Eq)),
            | Token::Sym(M_LT!()) => instructions.push(Instruction::BinOp(BinOp::Lt)),
            | Token::Sym(M_GT!()) => instructions.push(Instruction::BinOp(BinOp::Gt)),
            | Token::Str(_) | Token::Flt(_) | Token::Int(_) => instructions.push(Instruction::Push(token.into())),
            | Token::Block(mut block) => {
                block.reverse(); /* meow */
                instructions.push(Instruction::Push(Cord::Fun((block.as_slice().into(), true))))
            }
            | Token::Sym(M_FUN_PUSH!()) => {
                /* let fun_name /* Fun name */   = expect_token!(Name, tokens, file, line);
                 let fun      /* Fun object */ = funs.get(&fun_name).unwrap_or_else(|| {
                    emit_error!(file, line, format!("undefined function: {fun_name:?}"))
                });

                instructions.push(Instruction::Push(Cord::Fun(fun.clone())))
                */
            }
            | Token::Sym(M_EXEC!()) /* Manually call a function */ => {
                instructions.push(Instruction::CallStack)
            }
            | Token::Sym(M_THEN!()) /* Basically "if" statement */ => {
                let mut tmp_tokens = expect_token!(Block, tokens, file, line);
                instructions.push(Instruction::CallIf(parse_tokens(&mut tmp_tokens, false)))
            }
            | Token::Sym('§') /* Can be pushed by interpreter only */ => {
                instructions.push(Instruction::VarPush(expect_token!(Name, tokens, file, line)))
            }
            | Token::Sym(M_DEF!()) /* Assign stack top value to next name */ => {
                let name = expect_token!(Name, tokens, file, line);
                if name.starts_with("__") /* Private field */ { panic!(/* TODO: make the error message */) }
                instructions.push(Instruction::VarPush(name))
            }
            | Token::Sym(char) => emit_error!(file, line, format!("unknown symbol: {char}.")),
            | Token::Name(name) => match name.as_str() {
                | M_PUTS!() => instructions.push(Instruction::BuiltInCall(BuiltIn::Puts)), 
                | M_ELIST!() => instructions.push(Instruction::Push(Cord::Lst(Vec::new()))),
                | M_FUN!() => {
                    let mut fun_name   = expect_token!(Name, tokens, file, line);
                    let mut fun_tokens = Vec::default();
                    let is_inline      = fun_name == M_INLINE!();

                    if is_inline { fun_name = expect_token!(Name, tokens, file, line) }

                    while let Some(tmp_token) = tokens.pop() {
                        match tmp_token.clone() {
                            | (Token::Block(tmp_tokens), _, _) => {
                                fun_tokens.reverse(); /* uhm */
                                fun_tokens.extend(tmp_tokens);
                                fun_tokens.reverse(); /* never ask if maeel could be faster */
                                break; /* TODO: remove this break, f*ck breaks */
                            }
                            | (Token::Name(_), file, line) => {
                                fun_tokens.push(tmp_token);
                                fun_tokens.push((Token::Sym('§'), file, line));
                            }
                            | (other, file, line) => {
                                emit_error!(
                                    file,
                                    line,
                                    format!("expected name(s) or a code block after 'fun {fun_name}'; got {other:?} instead.")
                                    )
                            }
                        }
                    }
                    instructions.push(Instruction::FunPush((fun_name.clone(), parse_tokens((&mut fun_tokens).into(), false), is_inline)));
                }
                | M_LEN!() => instructions.push(Instruction::BuiltInCall(BuiltIn::Len)),
                | M_GET!() => instructions.push(Instruction::BuiltInCall(BuiltIn::Get)),
                | name => {
                    if (&name).starts_with("f") {
                        instructions.push(Instruction::FunCall(name.to_string()))
                    } else {
                        instructions.push(Instruction::VarCall(name.to_string()))
                    }
                }
            },
        };
    }

    instructions
}

impl std::fmt::Display for Cord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Str(x)  => write!(f, "{}", x),
            | Self::Fun(_)  => write!(f, "Fun"),
            | Self::Flt(x)  => write!(f, "{}", x),
            | Self::Int(x)  => write!(f, "{}", x),
            | Self::Lst(xs) => {
                write!(f, "{{")?;
                xs.iter().enumerate().for_each(|(i, x)| {
                    if i > 0 { write!(f, " ").unwrap() }
                    write!(f, "{}", x).unwrap()
                });
                write!(f, "}}")
            }
        }
    }
}

impl From<Token> for Cord {
    fn from(val: Token) -> Self {
        match val {
            | Token::Str(x)   => Cord::Str(x),
            | Token::Int(x)   => Cord::Int(x),
            | Token::Flt(x)   => Cord::Flt(x),
            | Token::Block(x) => Cord::Fun((x.as_slice().into(), false)),
            | _               => panic!(),
        }
    }
}

fn generate_asm(instructions: Vec<Instruction>) {
    let mut output = String::new();

    output.push_str("BITS 64\n");
    output.push_str("segment .text\n");
    output.push_str("print:\n");
    output.push_str("    mov     r9, -3689348814741910323\n");
    output.push_str("    sub     rsp, 40\n");
    output.push_str("    mov     BYTE [rsp+31], 10\n");
    output.push_str("    lea     rcx, [rsp+30]\n");
    output.push_str(".L2:                                   ;; iterate through each digit\n");
    output.push_str("    mov     rax, rdi                   ;; rax <- rdi\n");
    output.push_str("    lea     r8, [rsp+32]               ;; r8 <- rsp+32\n");
    output.push_str("    mul     r9                         ;; rax <- rax * r9\n");
    output.push_str("    mov     rax, rdi                   ;; rax <- rdi\n");
    output.push_str("    sub     r8, rcx                    ;; r8 <- r8 - rcx\n");
    output.push_str("    shr     rdx, 3                     ;; right shift (3 bits)\n");
    output.push_str("    lea     rsi, [rdx*5]               ;; rsi <- 5*rdx\n");
    output.push_str("    add     rsi, rsi\n");
    output.push_str("    sub     rax, rsi\n");
    output.push_str("    add     eax, 48\n");
    output.push_str("    mov     BYTE [rcx], al\n");
    output.push_str("    mov     rax, rdi\n");
    output.push_str("    mov     rdi, rdx\n");
    output.push_str("    mov     rdx, rcx\n");
    output.push_str("    dec     rcx\n");
    output.push_str("    cmp     rax, 9                     ;; if rax < 9\n");
    output.push_str("    ja      .L2                        ;;    recursive call\n");
    output.push_str("    lea     rax, [rsp+32]\n");
    output.push_str("    mov     edi, 1\n");
    output.push_str("    sub     rdx, rax\n");
    output.push_str("    xor     eax, eax                   ;; clear eax\n");
    output.push_str("    mov     rax, 1                     ;; write(\n");
    output.push_str("    lea     rsi, [rsp+32+rdx]          ;;  integer as a string,\n");
    output.push_str("    mov     rdx, r8                    ;;  char count\n");
    output.push_str("    syscall                            ;; );\n");
    output.push_str("    add     rsp, 40\n");
    output.push_str("    ret\n");
    output.push_str("global _start\n");
    output.push_str("_start:\n");

    let mut idx = 0;

    for instruction in &instructions {

        output.push_str(&format!("a_{idx}:\n"));

        match instruction {
            | Instruction::Push(data) => match data {
                | Cord::Int(value) => {
                    output.push_str(&format!("   ;; push(int) {value}\n"));
                    output.push_str(&format!("   mov rax, {value}\n"));
                    output.push_str("   push rax\n");
                }
                | _ => panic!("oops (data)"),
            }
            | Instruction::BuiltInCall(fun) => match fun {
                | BuiltIn::Puts => {
                    output.push_str("   ;; print\n");
                    output.push_str("   pop rdi\n");
                    output.push_str("   call print\n");
                }
                | _ => panic!("oops (btin)"),
            }
            | Instruction::BinOp(op) => match op {
                | BinOp::Add => {
                    output.push_str("   ;; add \n");
                    output.push_str("   pop rax\n");
                    output.push_str("   pop rbx\n");
                    output.push_str("   add rax, rbx\n");
                    output.push_str("   push rax\n");
                }
                | _ => panic!("oops (op)"),
            }
            | _ => panic!("oops"),
        }

        idx += 1;
    }

    output.push_str(&format!("a_{}:\n", &instructions.len()));
    output.push_str("   mov rax, 60 ;; exit(\n");
    output.push_str("   mov rdi, 0  ;;  0\n");
    output.push_str("   syscall     ;; );\n");

    output.push_str("segment .data\n");
    output.push_str("segment .bss\n");

    println!("{}", output);
}


fn main() {
    let file = std::env::args().nth(1).unwrap();

    let is = parse_tokens(
        &mut lex_into_tokens(&std::fs::read_to_string(&file).unwrap(), &file),
        true,
    );

    generate_asm(is);
}
