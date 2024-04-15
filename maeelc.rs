/*
This is STILL in development phase !! (partially works)
*/

use std::fs::File;
use std::collections::HashMap;
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

enum BinOp { Add, Sub, Mul }

enum BuiltIn { Puts }

enum Instruction {
    BinOp(BinOp),
    Push(M_INT_SIZE),
    BuiltInCall(BuiltIn),
    PushAsm(String),
    Store(String),
    Load(String)
}

fn parse_tokens(
    tokens: &mut Vec<TokenData>,
    rev: bool,
) {
    if rev /* Sometimes we might act like the tokens vec was a stack */ { tokens.reverse(); }

    let mut instructions = Vec::new();
    let mut symbol_table: HashMap<String, i32> = HashMap::new();
    let mut stack_pointer = 0; // Initialize stack pointer
    while let Some((token, file, line)) = tokens.pop() {
        match token {
            | Token::Sym(M_ADD!()) => instructions.push(Instruction::BinOp(BinOp::Add)),
            | Token::Sym(M_SUB!()) => instructions.push(Instruction::BinOp(BinOp::Sub)),
            | Token::Sym(M_MUL!()) => instructions.push(Instruction::BinOp(BinOp::Mul)),
            | Token::Int(value) => instructions.push(Instruction::Push(value)),
            | Token::Sym('~') => {
                let name = expect_token!(Name, tokens, file, line);
                instructions.push(Instruction::Store(name.clone()));
                symbol_table.insert(name, stack_pointer);
                stack_pointer += 64;
            }
            | Token::Sym('$') => {
                let data = expect_token!(Str, tokens, file, line);
                instructions.push(Instruction::PushAsm(data));
            }
            | Token::Name(name) => {
                if let Some(&pos) = symbol_table.get(&name) {
                    instructions.push(Instruction::Load(name.clone()));
                } else {
                    match name.as_str() {
                        | M_PUTS!() => instructions.push(Instruction::BuiltInCall(BuiltIn::Puts)),
                        | _ => unreachable!()
                    }
                }
            }
            | _ => unreachable!()
        }
    }

    println!("section .text");
    println!("extern printf");
    println!("global _start");
    println!("_start:");

    for instruction in &instructions {
        match instruction {
            | Instruction::PushAsm(line) => {
                println!("   ;; custom assembly");
                println!("{line}");
            }
            | Instruction::Push(value) => {
                println!("   ;; push(int) {value}");
                println!("   mov rax, {value}");
                println!("   push rax");
            }
            | Instruction::BuiltInCall(fun) => match fun {
                | BuiltIn::Puts => {
                    println!("   ;; puts integer");
                    println!("   mov rdi, print_int_fmt");
                    println!("   pop rsi");
                    println!("   xor rax, rax");
                    println!("   call printf");
                }
                | _ => unreachable!(),
            }
            Instruction::Store(name) => {
                println!("   ;; store variable {}", name);
                println!("   pop rax");
                println!("   mov [rsp - {}], rax", symbol_table[name]);
            }
            Instruction::Load(name) => {
                println!("   ;; load variable {}", name);
                println!("   mov rax, [rsp - {}]", symbol_table[name]);
                println!("   push rax");
            }
            | Instruction::BinOp(op) => match op {
                 | BinOp::Add => {
                    println!("   ;; add ");
                    println!("   pop rax ;; rax <- RHS");
                    println!("   pop rbx ;; rbx <- LHS");
                    println!("   add rax, rbx");
                    println!("   push rax");
                }
                | BinOp::Mul => {
                    println!("   ;; mul ");
                    println!("   pop rax ;; rax <- RHS");
                    println!("   pop rbx ;; rbx <- LHS");
                    println!("   mul rbx");
                    println!("   push rax");
                }
                | BinOp::Sub => {
                    println!("   ;; sub ");
                    println!("   pop rax ;; rax <- RHS");
                    println!("   pop rbx ;; rbx <- LHS");
                    println!("   sub rbx, rax");
                    println!("   push rbx");
                }
                | _ => unreachable!()
            }
            | _ => unreachable!()
        }
    }

    println!("   mov rax, 60   ;; exit(");
    println!("   xor rdi, rdi  ;;  0");
    println!("   syscall       ;; );");

    println!("section .data");
    println!("  print_int_fmt db \"%d\", 10")
}


fn main() {
    let file = std::env::args().nth(1).unwrap();

    parse_tokens(
        &mut lex_into_tokens(&std::fs::read_to_string(&file).unwrap(), &file),
        true,
    );
}
