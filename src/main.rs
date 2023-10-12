use eyre::Result;
use jitevm::code::{EvmCode, EvmOpParserMode};
use jitevm::jit::{JitContractBuilder, JitEvmExecutionContext};
use jitevm::test_data;
use revm::{
    db::in_memory_db::BenchmarkDB,
    primitives::{address, Address, Bytecode, Bytes, Env, U256},
    EVM,
};
use std::error::Error;
use std::time::Instant;

fn main() -> Result<(), Box<dyn Error>> {
    let ops = test_data::get_code_ops_fibonacci();
    // TESTING BASIC OPERATIONS WITH EVMOP AND EVMCODE

    let code = EvmCode { ops: ops.clone() };
    let augmented_code = code.augment();

    assert!(code.to_bytes() == augmented_code.to_bytes());
    assert!(code == EvmCode::new_from_bytes(&augmented_code.to_bytes(), EvmOpParserMode::Strict)?);

    // TESTING JIT

    use inkwell::context::Context;
    let context = Context::create();
    let contract = JitContractBuilder::with_context("some-contract-address", &context)?
        .debug_ir("jit_bench.ll")
        .debug_asm("jit_bench.asm")
        .build(EvmCode { ops: ops.clone() }.augment().index())?;

    println!("Benchmark compiled execution ...");
    for _i in 0..10 {
        let mut holder = JitEvmExecutionContext::new();
        let measurement_now = Instant::now();
        let result = contract.call(&mut holder)?;
        let measurement_runtime = measurement_now.elapsed();
        let JitEvmExecutionContext { storage, .. } = holder;
        println!("{:?}", result);
        println!("{:?}", storage.keys());
        println!(
            "measurement_runtime {:?} ({:x})",
            measurement_runtime,
            storage[&0.into()]
        );
    }

    // TESTING REVM

    let bytes = Bytes::copy_from_slice(&code.to_bytes());
    let bytecode = Bytecode::new_raw(bytes).to_checked();
    let database = BenchmarkDB::new_bytecode(bytecode);

    let env = Env::default();
    let mut evm = EVM {
        env,
        db: Some(database),
    };

    evm.env.tx.caller = address!("Ef8801eaf234ff82801821FFe2d78D60a0237F97");

    println!("Benching revm execution");

    for _i in 0..10 {
        let measurement_now = Instant::now();
        let result = evm.transact().expect("Bad evm stuff");
        let measurement_runtime = measurement_now.elapsed();
        let value = &result.state[&Address::default()].storage[&U256::ZERO].present_value;
        println!("measurement_runtime {:?} {:x}", measurement_runtime, value);
    }

    Ok(())
}
