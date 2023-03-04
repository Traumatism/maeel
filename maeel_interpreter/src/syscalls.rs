#![allow(semicolon_in_expressions_from_macros)]

use super::VMType;
use core::arch::asm;

#[cfg(not(any(
    all(target_family = "unix", target_arch = "x86_64"),
    all(target_os = "macos", target_arch = "aarch64")
)))]
#[macro_export]
macro_rules! do_syscall {
    ($syscall_nr:expr, $arg_0:expr) => {};
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr) => {};
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr, $arg_2:expr) => {};
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr, $arg_2:expr, $arg_3:expr) => {};
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr, $arg_2:expr, $arg_3:expr, $arg_4:expr) => {};
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr, $arg_2:expr, $arg_3:expr, $arg_4:expr, $arg_5:expr) => {};
}

/// Perform syscalls on Unix x86_64
#[cfg(all(target_family = "unix", target_arch = "x86_64"))]
#[macro_export]
macro_rules! do_syscall {
    ($syscall_nr:expr, $arg_0:expr) => {
        asm!(
            "syscall",
            in("rdi") $arg_0,
            in("rax") $syscall_nr,
        );
    };
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr) => {
        asm!(
            "syscall",
            in("rdi") $arg_0,
            in("rsi") $arg_1,
            in("rax") $syscall_nr,
        );
    };
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr, $arg_2:expr) => {
        asm!(
            "syscall",
            in("rdi") $arg_0,
            in("rsi") $arg_1,
            in("rdx") $arg_2,
            in("rax") $syscall_nr,
        );
    };
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr, $arg_2:expr, $arg_3:expr) => {
        asm!(
            "syscall",
            in("rdi") $arg_0,
            in("rsi") $arg_1,
            in("rdx") $arg_2,
            in("r10") $arg_3,
            in("rax") $syscall_nr,
        );
    };
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr, $arg_2:expr, $arg_3:expr, $arg_4:expr) => {
        asm!(
            "syscall",
            in("rdi") $arg_0,
            in("rsi") $arg_1,
            in("rdx") $arg_2,
            in("r10") $arg_3,
            in("r8") $arg_4,
            in("rax") $syscall_nr,
        );
    };
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr, $arg_2:expr, $arg_3:expr, $arg_4:expr, $arg_5:expr) => {
        asm!(
            "syscall",
            in("rdi") $arg_0,
            in("rsi") $arg_1,
            in("rdx") $arg_2,
            in("r10") $arg_3,
            in("r8") $arg_4,
            in("r9") $arg_5,
            in("rax") $syscall_nr,
        );
    };
}

/// Perform syscalls on apple M1
#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[macro_export]
macro_rules! do_syscall {
    ($syscall_nr:expr, $arg_0:expr) => {
        asm!(
            "svc #0",
            in("x0") $arg_0,
            in("x16") $syscall_nr,
        );
    };
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr) => {
        asm!(
            "svc #0",
            in("x0") $arg_0,
            in("x1") $arg_1,
            in("x16") $syscall_nr,
        );
    };
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr, $arg_2:expr) => {
        asm!(
            "svc #0",
            in("x0") $arg_0,
            in("x1") $arg_1,
            in("x2") $arg_2,
            in("x16") $syscall_nr,
        );
    };
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr, $arg_2:expr, $arg_3:expr) => {
        asm!(
            "svc #0",
            in("x0") $arg_0,
            in("x1") $arg_1,
            in("x2") $arg_2,
            in("x3") $arg_3,
            in("x16") $syscall_nr,
        );
    };
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr, $arg_2:expr, $arg_3:expr, $arg_4:expr) => {
        asm!(
            "svc #0",
            in("x0") $arg_0,
            in("x1") $arg_1,
            in("x2") $arg_2,
            in("x3") $arg_3,
            in("x4") $arg_4,
            in("x16") $syscall_nr,
        );
    };
    ($syscall_nr:expr, $arg_0:expr, $arg_1:expr, $arg_2:expr, $arg_3:expr, $arg_4:expr, $arg_5:expr) => {
        asm!(
            "svc #0",
            in("x0") $arg_0,
            in("x1") $arg_1,
            in("x2") $arg_2,
            in("x3") $arg_3,
            in("x4") $arg_4,
            in("x5") $arg_5,
            in("x16") $syscall_nr,
        );
    };
}

/// Handle a syscall
pub fn handle_syscall(syscall_nr: usize, args: &[VMType]) {
    let mut arg_ptrs: [*const std::ffi::c_void; 6] = [std::ptr::null(); 6];

    for (i, arg) in args.iter().enumerate() {
        match arg {
            VMType::Integer(value) => arg_ptrs[i] = *value as *const std::ffi::c_void,
            VMType::IntPointer(ptr) => {
                arg_ptrs[i] = &**ptr as *const i64 as *const std::ffi::c_void
            }
            VMType::StrPointer(ptr) => arg_ptrs[i] = ptr.as_ptr() as *const std::ffi::c_void,
            _ => panic!("invalid argument type"),
        }
    }

    match args.len() {
        1 => unsafe {
            do_syscall!(syscall_nr, arg_ptrs[0]);
        },

        2 => unsafe {
            do_syscall!(syscall_nr, arg_ptrs[0], arg_ptrs[1]);
        },

        3 => unsafe {
            do_syscall!(syscall_nr, arg_ptrs[0], arg_ptrs[1], arg_ptrs[2]);
        },

        4 => unsafe {
            do_syscall!(
                syscall_nr,
                arg_ptrs[0],
                arg_ptrs[1],
                arg_ptrs[2],
                arg_ptrs[3]
            );
        },

        5 => unsafe {
            do_syscall!(
                syscall_nr,
                arg_ptrs[0],
                arg_ptrs[1],
                arg_ptrs[2],
                arg_ptrs[3],
                arg_ptrs[4]
            );
        },

        6 => unsafe {
            do_syscall!(
                syscall_nr,
                arg_ptrs[0],
                arg_ptrs[1],
                arg_ptrs[2],
                arg_ptrs[3],
                arg_ptrs[4],
                arg_ptrs[5]
            );
        },
        _ => panic!("invalid number of arguments"),
    }
}
