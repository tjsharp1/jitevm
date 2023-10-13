use criterion::{BenchmarkId, criterion_group, criterion_main, Criterion};
use inkwell::context::Context;
use jitevm::code::{EvmCode, EvmOpParserMode};
use jitevm::jit::{JitEvmContract, JitContractBuilder, JitEvmExecutionContext};
use jitevm::test_data;
use revm::{
    db::in_memory_db::BenchmarkDB,
    primitives::{address, Address, Bytecode, Bytes, Env, U256},
    EVM,
};


pub fn evm_benchmark(c: &mut Criterion) {
    let ops = test_data::get_code_ops_fibonacci();
    let code = EvmCode { ops: ops.clone() };

    let bytes = Bytes::copy_from_slice(&code.to_bytes());
    let bytecode = Bytecode::new_raw(bytes).to_checked();
    let database1 = BenchmarkDB::new_bytecode(bytecode);

    let ops = test_data::get_code_ops_fibonacci_repetitions();
    let code = EvmCode { ops: ops.clone() };
    let bytes = Bytes::copy_from_slice(&code.to_bytes());
    let bytecode = Bytecode::new_raw(bytes).to_checked();
    let database2 = BenchmarkDB::new_bytecode(bytecode);

    let mut env1 = Env::default();
    env1.tx.caller = address!("Ef8801eaf234ff82801821FFe2d78D60a0237F97");
    let args1 = (env1, database1);

    let mut env2 = Env::default();
    env2.tx.caller = address!("Ef8801eaf234ff82801821FFe2d78D60a0237F97");
    let args2 = (env2, database2);

	let mut group = c.benchmark_group("Interpreter benchmarks");
    group.bench_with_input(
		BenchmarkId::new("Interpreter bench", 0),
		&args1,
		|b, i| b.iter(|| interpreter_bench(i))
	);
    group.bench_with_input(
		BenchmarkId::new("Interpreter bench", 1),
		&args2,
		|b, i| b.iter(|| interpreter_bench(i))
	);

	group.finish();
}

pub fn jitevm_benchmark(c: &mut Criterion) {
    let context = Context::create();

    let ops = test_data::get_code_ops_fibonacci();
    let code = EvmCode { ops: ops.clone() };
    let contract1 = JitContractBuilder::with_context("contract1", &context)
	    .expect("Could not build builder")
        .build(code.augment().index())
		.expect("Could not JIT contract");

    let ops = test_data::get_code_ops_fibonacci_repetitions();
    let code = EvmCode { ops: ops.clone() };
    let contract2 = JitContractBuilder::with_context("contract2", &context)
	    .expect("Could not build builder")
        .build(code.augment().index())
		.expect("Could not JIT contract");


    let mut group = c.benchmark_group("JIT benchmarks");
    group.bench_with_input(
		BenchmarkId::new("Jit bench", 0),
		&contract1,
		|b, i| b.iter(|| jitevm_bench(i))
	);
    group.bench_with_input(
		BenchmarkId::new("Jit bench", 1),
		&contract2,
		|b, i| b.iter(|| jitevm_bench(i))
	);
	group.finish();
}

criterion_group!(benches, evm_benchmark, jitevm_benchmark);
criterion_main!(benches);

fn jitevm_bench<'ctx>(contract: &JitEvmContract<'ctx>) {
	let mut holder = JitEvmExecutionContext::new();
	let _result = contract.call(&mut holder)
	    .expect("JIT contract call failed");
}


fn interpreter_bench(args: &(Env, BenchmarkDB)) {
    let mut evm = EVM {
        env: args.0.clone(),
        db: Some(args.1.clone()),
    };
	let _result = evm.transact().expect("Bad evm stuff");
}
