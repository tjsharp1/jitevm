use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use inkwell::context::Context;
use jit_contract::code::{EvmCode, EvmOpParserMode};
use jit_contract::jit::{
    contract::{JitContractBuilder, JitEvmContract},
    JitEvmExecutionContext,
};
use revm::{
    db::in_memory_db::BenchmarkDB,
    primitives::{address, Address, Bytecode, Bytes, Env, U256},
    EVM,
};
use revm_primitives::{LatestSpec, Spec};
use std::path::{Path, PathBuf};
use std::time::Duration;

fn workspace_dir() -> PathBuf {
    let output = std::process::Command::new(env!("CARGO"))
        .arg("locate-project")
        .arg("--workspace")
        .arg("--message-format=plain")
        .output()
        .unwrap()
        .stdout;
    let cargo_path = Path::new(std::str::from_utf8(&output).unwrap().trim());
    cargo_path.parent().unwrap().to_path_buf()
}

fn load_evm_code(test_name: &str) -> EvmCode {
    let workspace_dir = workspace_dir();

    let mut path = PathBuf::new();
    path.push(workspace_dir);
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
    let code = load_evm_code("reth_random_stuff");
    let args1 = get_env_args(code);

    let mut group = c.benchmark_group("REVM storage");
    group.measurement_time(Duration::from_secs(30));
    group.bench_with_input(
        BenchmarkId::new("REVM", "random_storage"),
        &args1,
        |b, i| b.iter(|| interpreter_bench(i)),
    );
    group.finish();
}

pub fn jitevm_benchmark(c: &mut Criterion) {
    let context = Context::create();

    let code = load_evm_code("reth_random_stuff");
    let (_, args1) = get_env_args(code.clone());
    let contract1 = JitContractBuilder::with_context("contract1", &context)
        .expect("Could not build builder")
        .build(LatestSpec, code.augment().index())
        .expect("Could not JIT contract");

    let mut group = c.benchmark_group("JIT storage");
    group.measurement_time(Duration::from_secs(30));
    group.bench_with_input(
        BenchmarkId::new("JIT", "random_storage"),
        &(args1, contract1),
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
