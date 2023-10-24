use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use inkwell::context::Context;
use jitevm::code::{EvmCode, EvmOpParserMode};
use jitevm::jit::{
    contract::{JitContractBuilder, JitEvmContract},
    JitEvmExecutionContext,
};
use revm::{
    db::in_memory_db::BenchmarkDB,
    primitives::{address, Address, Bytecode, Bytes, Env, U256},
    EVM,
};
use std::path::PathBuf;

fn load_evm_code(test_name: &str) -> EvmCode {
    let test_base_dir = std::env::var("CARGO_MANIFEST_DIR").expect("No cargo root");
    let mut path = PathBuf::new();
    path.push(test_base_dir);
    path.push("contracts");
    path.push(format!("{}.bc", test_name));

    let bytecode = std::fs::read_to_string(path).expect("Couldn't open test file");
    let bytes = hex::decode(bytecode).expect("Invalid hex data");

    EvmCode::new_from_bytes(&bytes, EvmOpParserMode::Strict).expect("Failed parsing EVM opcodes")
}

fn interp_get_env_args(code: EvmCode) -> (Env, BenchmarkDB) {
    let bytes = Bytes::copy_from_slice(&code.to_bytes());
    let bytecode = Bytecode::new_raw(bytes).to_checked();
    let database = BenchmarkDB::new_bytecode(bytecode);

    let mut env = Env::default();
    env.tx.caller = address!("Ef8801eaf234ff82801821FFe2d78D60a0237F97");

    (env, database)
}

pub fn evm_benchmark(c: &mut Criterion) {
    let code = load_evm_code("fibonacci");
    let args1 = interp_get_env_args(code);

    let code = load_evm_code("fibonacci_repetitions");
    let args2 = interp_get_env_args(code);

    let code = load_evm_code("exp");
    let args3 = interp_get_env_args(code);

    let code = load_evm_code("mload");
    let args4 = interp_get_env_args(code);

    let code = load_evm_code("mstore8");
    let args5 = interp_get_env_args(code);

    let code = load_evm_code("sha3");
    let args6 = interp_get_env_args(code);

    let mut group = c.benchmark_group("REVM benchmarks");
    group.bench_with_input(BenchmarkId::new("REVM", "fibonacci"), &args1, |b, i| {
        b.iter(|| interpreter_bench(i))
    });
    group.bench_with_input(
        BenchmarkId::new("REVM", "fibonacci_repetitions"),
        &args2,
        |b, i| b.iter(|| interpreter_bench(i)),
    );
    group.bench_with_input(BenchmarkId::new("REVM", "exp"), &args3, |b, i| {
        b.iter(|| interpreter_bench(i))
    });
    group.bench_with_input(BenchmarkId::new("REVM", "mload"), &args4, |b, i| {
        b.iter(|| interpreter_bench(i))
    });
    group.bench_with_input(BenchmarkId::new("REVM", "mstore8"), &args5, |b, i| {
        b.iter(|| interpreter_bench(i))
    });
    group.bench_with_input(BenchmarkId::new("REVM", "sha3"), &args6, |b, i| {
        b.iter(|| interpreter_bench(i))
    });

    group.finish();
}

pub fn jitevm_benchmark(c: &mut Criterion) {
    let context = Context::create();

    let code = load_evm_code("fibonacci");
    let contract1 = JitContractBuilder::with_context("contract1", &context)
        .expect("Could not build builder")
        .build(code.augment().index())
        .expect("Could not JIT contract");

    let code = load_evm_code("fibonacci_repetitions");
    let contract2 = JitContractBuilder::with_context("contract2", &context)
        .expect("Could not build builder")
        .build(code.augment().index())
        .expect("Could not JIT contract");

    let code = load_evm_code("exp");
    let contract3 = JitContractBuilder::with_context("contract3", &context)
        .expect("Could not build builder")
        .build(code.augment().index())
        .expect("Could not JIT contract");

    let code = load_evm_code("mload");
    let contract4 = JitContractBuilder::with_context("contract4", &context)
        .expect("Could not build builder")
        .build(code.augment().index())
        .expect("Could not JIT contract");

    let code = load_evm_code("mstore8");
    let contract5 = JitContractBuilder::with_context("contract5", &context)
        .expect("Could not build builder")
        .build(code.augment().index())
        .expect("Could not JIT contract");

    let code = load_evm_code("sha3");
    let contract6 = JitContractBuilder::with_context("contract6", &context)
        .expect("Could not build builder")
        .build(code.augment().index())
        .expect("Could not JIT contract");

    let mut group = c.benchmark_group("JIT benchmarks");
    group.bench_with_input(BenchmarkId::new("JIT", "fibonacci"), &contract1, |b, i| {
        b.iter(|| jitevm_bench(i))
    });
    group.bench_with_input(
        BenchmarkId::new("JIT", "fibonacci_repetitions"),
        &contract2,
        |b, i| b.iter(|| jitevm_bench(i)),
    );
    group.bench_with_input(BenchmarkId::new("JIT", "exp"), &contract3, |b, i| {
        b.iter(|| jitevm_bench(i))
    });
    group.bench_with_input(BenchmarkId::new("JIT", "mload"), &contract4, |b, i| {
        b.iter(|| jitevm_bench(i))
    });
    group.bench_with_input(BenchmarkId::new("JIT", "mstore8"), &contract5, |b, i| {
        b.iter(|| jitevm_bench(i))
    });
    group.bench_with_input(BenchmarkId::new("JIT", "sha3"), &contract6, |b, i| {
        b.iter(|| jitevm_bench(i))
    });
    group.finish();
}

criterion_group!(benches, evm_benchmark, jitevm_benchmark);
criterion_main!(benches);

fn jitevm_bench<'ctx>(contract: &JitEvmContract<'ctx>) {
    let mut holder = JitEvmExecutionContext::new();
    let _result = contract
        .call(&mut holder)
        .expect("JIT contract call failed");
}

fn interpreter_bench(args: &(Env, BenchmarkDB)) {
    let mut evm = EVM {
        env: args.0.clone(),
        db: Some(args.1.clone()),
    };
    let _result = evm.transact().expect("Bad evm stuff");
}
