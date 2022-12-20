use maeel::enums::VMType;
use maeel::vm::Stack;

use criterion::{criterion_group, criterion_main, Criterion};

fn fast_pop(vm: &mut Stack<VMType>) {
    vm.push(VMType::String(String::from("hello"))); // tail
    vm.push(VMType::Integer(12)); // mid
    vm.push(VMType::Float(1.1)); // head

    vm.fast_pop();
    vm.fast_pop();
    vm.fast_pop();
}

fn normal_pop(vm: &mut Stack<VMType>) {
    vm.push(VMType::String(String::from("hello"))); // tail
    vm.push(VMType::Integer(12)); // mid
    vm.push(VMType::Float(1.1)); // head

    vm.pop();
    vm.pop();
    vm.pop();
}

fn fast_pop_b(c: &mut Criterion) {
    let mut vm = Stack::default();

    c.bench_function("fast_pop", |b| b.iter(|| fast_pop(&mut vm)));
    c.bench_function("normal_pop", |b| b.iter(|| normal_pop(&mut vm)));
}

criterion_group!(benches, fast_pop_b);
criterion_main!(benches);
