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
use revm_primitives::{LatestSpec, Spec};
use std::path::PathBuf;
use std::time::Duration;

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

fn get_env_args(code: EvmCode) -> (Env, BenchmarkDB) {
    let bytes = Bytes::copy_from_slice(&code.to_bytes());
    let bytecode = Bytecode::new_raw(bytes).to_checked();
    let database = BenchmarkDB::new_bytecode(bytecode);

    let mut env = Env::default();
    env.tx.caller = address!("Ef8801eaf234ff82801821FFe2d78D60a0237F97");

    (env, database)
}

pub fn evm_benchmark(c: &mut Criterion) {
    let code = load_evm_code("fibonacci");
    let args1 = get_env_args(code);

    let code = load_evm_code("fibonacci_repetitions");
    let args2 = get_env_args(code);

    let code = load_evm_code("exp");
    let args3 = get_env_args(code);

    let code = load_evm_code("mload");
    let args4 = get_env_args(code);

    let code = load_evm_code("mstore8");
    let args5 = get_env_args(code);

    let code = load_evm_code("sha3");
    let args6 = get_env_args(code);

    let code = load_evm_code("store");
    let args7 = get_env_args(code);

    let code = load_evm_code("sload");
    let args8 = get_env_args(code);

    let mut group = c.benchmark_group("REVM benchmarks");
    group.measurement_time(Duration::from_secs(30));
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
    group.bench_with_input(BenchmarkId::new("REVM", "store"), &args7, |b, i| {
        b.iter(|| interpreter_bench(i))
    });
    group.bench_with_input(BenchmarkId::new("REVM", "sload"), &args8, |b, i| {
        b.iter(|| interpreter_bench(i))
    });
    group.finish();
}

pub fn jitevm_benchmark(c: &mut Criterion) {
    let context = Context::create();

    let code = load_evm_code("fibonacci");
    let (_, args1) = get_env_args(code.clone());
    let contract1 = JitContractBuilder::with_context("contract1", &context)
        .expect("Could not build builder")
        .build(LatestSpec, code.augment().index())
        .expect("Could not JIT contract");

    let code = load_evm_code("fibonacci_repetitions");
    let (_, args2) = get_env_args(code.clone());
    let contract2 = JitContractBuilder::with_context("contract2", &context)
        .expect("Could not build builder")
        .build(LatestSpec, code.augment().index())
        .expect("Could not JIT contract");

    let code = load_evm_code("exp");
    let (_, args3) = get_env_args(code.clone());
    let contract3 = JitContractBuilder::with_context("contract3", &context)
        .expect("Could not build builder")
        .build(LatestSpec, code.augment().index())
        .expect("Could not JIT contract");

    let code = load_evm_code("mload");
    let (_, args4) = get_env_args(code.clone());
    let contract4 = JitContractBuilder::with_context("contract4", &context)
        .expect("Could not build builder")
        .build(LatestSpec, code.augment().index())
        .expect("Could not JIT contract");

    let code = load_evm_code("mstore8");
    let (_, args5) = get_env_args(code.clone());
    let contract5 = JitContractBuilder::with_context("contract5", &context)
        .expect("Could not build builder")
        .build(LatestSpec, code.augment().index())
        .expect("Could not JIT contract");

    let code = load_evm_code("sha3");
    let (_, args6) = get_env_args(code.clone());
    let contract6 = JitContractBuilder::with_context("contract6", &context)
        .expect("Could not build builder")
        .build(LatestSpec, code.augment().index())
        .expect("Could not JIT contract");

    let code = load_evm_code("store");
    let (_, args7) = get_env_args(code.clone());
    let contract7 = JitContractBuilder::with_context("contract7", &context)
        .expect("Could not build builder")
        .build(LatestSpec, code.augment().index())
        .expect("Could not JIT contract");

    let code = load_evm_code("sload");
    let (_, args8) = get_env_args(code.clone());
    let contract8 = JitContractBuilder::with_context("contract8", &context)
        .expect("Could not build builder")
        .build(LatestSpec, code.augment().index())
        .expect("Could not JIT contract");

    let mut group = c.benchmark_group("JIT benchmarks");
    group.measurement_time(Duration::from_secs(30));
    group.bench_with_input(
        BenchmarkId::new("JIT", "fibonacci"),
        &(args1, contract1),
        |b, i| b.iter(|| jitevm_bench(LatestSpec, i)),
    );
    group.bench_with_input(
        BenchmarkId::new("JIT", "fibonacci_repetitions"),
        &(args2, contract2),
        |b, i| b.iter(|| jitevm_bench(LatestSpec, i)),
    );
    group.bench_with_input(
        BenchmarkId::new("JIT", "exp"),
        &(args3, contract3),
        |b, i| b.iter(|| jitevm_bench(LatestSpec, i)),
    );
    group.bench_with_input(
        BenchmarkId::new("JIT", "mload"),
        &(args4, contract4),
        |b, i| b.iter(|| jitevm_bench(LatestSpec, i)),
    );
    group.bench_with_input(
        BenchmarkId::new("JIT", "mstore8"),
        &(args5, contract5),
        |b, i| b.iter(|| jitevm_bench(LatestSpec, i)),
    );
    group.bench_with_input(
        BenchmarkId::new("JIT", "sha3"),
        &(args6, contract6),
        |b, i| b.iter(|| jitevm_bench(LatestSpec, i)),
    );
    group.bench_with_input(
        BenchmarkId::new("JIT", "store"),
        &(args7, contract7),
        |b, i| b.iter(|| jitevm_bench(LatestSpec, i)),
    );
    group.bench_with_input(
        BenchmarkId::new("JIT", "sload"),
        &(args8, contract8),
        |b, i| b.iter(|| jitevm_bench(LatestSpec, i)),
    );
    group.finish();
}

criterion_group!(benches, evm_benchmark, jitevm_benchmark);
criterion_main!(benches);

fn jitevm_bench<'ctx, SPEC: Spec>(spec: SPEC, args: &(BenchmarkDB, JitEvmContract<'ctx, SPEC>)) {
    let mut holder = JitEvmExecutionContext::builder(spec).build_with_db(&args.0);

    let _result = args
        .1
        .transact(&mut holder)
        .expect("JIT contract call failed");
}

fn interpreter_bench(args: &(Env, BenchmarkDB)) {
    let mut evm = EVM {
        env: args.0.clone(),
        db: Some(args.1.clone()),
    };
    let _result = evm.transact().expect("Bad evm stuff");
}
