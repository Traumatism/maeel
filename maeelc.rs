/*
This is STILL in development phase !! (partially works)
*/

use std::fs::File;
use std::collections::HashMap;
use std::io::Read;

mod maeellex;

#[macro_use] use maeellex::*;

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

enum BuiltIn { Puts, PutsI, Syscall(M_INT_SIZE) }

enum PushT { Int(M_INT_SIZE), Str(String) }

enum Instruction {
    BinOp(BinOp),
    Push(PushT),
    BuiltInCall(BuiltIn),
    Store(String),
    Load(String)
}

fn parse_tokens(
    tokens: &mut Vec<TokenData>,
    rev: bool,
) {
    if rev /* Sometimes we might act like the tokens vec was a stack */ { tokens.reverse(); }

    let mut instructions = Vec::new();
    let mut symbol_table: HashMap<String, i64> = HashMap::new();
    let mut stack_pointer = 0_i64; /* Initialize stack pointer */

    while let Some((token, file, line)) = tokens.pop() {
        match token {
            | Token::Sym(M_ADD!()) => instructions.push(Instruction::BinOp(BinOp::Add)),
            | Token::Sym(M_SUB!()) => instructions.push(Instruction::BinOp(BinOp::Sub)),
            | Token::Sym(M_MUL!()) => instructions.push(Instruction::BinOp(BinOp::Mul)),
            | Token::Int(value) => instructions.push(Instruction::Push(PushT::Int(value))),
            | Token::Str(value) => {
                instructions.push(Instruction::Push(PushT::Str(value.clone())));
                symbol_table.insert(value.clone(), stack_pointer);
                stack_pointer += value.len() as i64 + 1;
            }
            | Token::Sym('~') => {
                let name = expect_token!(Name, tokens, file, line);
                instructions.push(Instruction::Store(name.clone()));
                symbol_table.insert(name, stack_pointer);
                stack_pointer += 1;
            }
            | Token::Name(name) => {
                if let Some(&pos) = symbol_table.get(&name) {
                    instructions.push(Instruction::Load(name.clone()));
                } else {
                    match name.as_str() {
                        | M_PUTS!() => instructions.push(Instruction::BuiltInCall(BuiltIn::Puts)),
                        | "putsi" => instructions.push(Instruction::BuiltInCall(BuiltIn::PutsI)),
                        | "syscall" => {
                            let n = expect_token!(Int, tokens, file, line);
                            instructions.push(Instruction::BuiltInCall(BuiltIn::Syscall(n)));
                        }
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

    let mut strs = Vec::new();

    for instruction in &instructions {
        match instruction {
            | Instruction::Push(pusht) => match pusht {
                | PushT::Int(value) => {
                    println!("   ;; push(int) {value}");
                    println!("   push {value}");
                }
                | PushT::Str(value) => {
                    println!("   ;; push(str)");
                    println!("   push str_{}", strs.len());
                    strs.push(value.clone());
                }
            }
            | Instruction::BuiltInCall(fun) => match fun {
                | BuiltIn::PutsI => {
                    println!("   ;; puts integer");
                    println!("   mov rdi, print_int_fmt");
                    println!("   pop rsi");
                    println!("   xor rax, rax");
                    println!("   call printf");
                }
                | BuiltIn::Puts => {
                    println!("   ;; puts string");
                    println!("   mov rdi, print_str_fmt");
                    println!("   pop rsi");
                    println!("   xor rax, rax");
                    println!("   call printf");
                }
                | BuiltIn::Syscall(n) => {
                    println!("   ;; syscall");
                    for i in 1..n+1 {
                        print!("   pop ");

                        let reg = match i {
                            | 1 => "rax",
                            | 2 => "rdi",
                            | 3 => "rsi",
                            | 4 => "rdx",
                            | 5 => "r10",
                            | 6 => "r8",
                            | 7 => "r9",
                            | _ => unreachable!()
                        };

                        println!("{reg}");
                    }

                    println!("   syscall");
                    println!("   push rax");
                }
                | _ => unreachable!(),
            }
            Instruction::Store(name) => {
                println!("   ;; store variable {}", name);
                println!("   pop rax");
                println!("   mov [rsp - {}], rax", symbol_table[name] + 8);
            }
            Instruction::Load(name) => {
                println!("   ;; load variable {}", name);
                println!("   mov rax, [rsp - {}]", symbol_table[name] + 8);
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
    println!("   print_int_fmt: db \"%d\", 10, 0");
    println!("   print_str_fmt: db \"%s\", 0");
    for (idx, string) in strs.iter().enumerate() {
        println!(
            "   str_{}: db {}, 0",
            idx,
            /* Prevent escape/injection issues */
            string.chars().map(|c| format!("0x{:x}", c as u8)).collect::<Vec<_>>().join(",")
        )
    }
}


fn main() {
    let file = std::env::args().nth(1).unwrap();

    parse_tokens(
        &mut lex_into_tokens(&std::fs::read_to_string(&file).unwrap(), &file),
        true,
    );
}
