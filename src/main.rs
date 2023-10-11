use eyre::Result;
use jitevm::code::{EvmCode, EvmOpParserMode};
use jitevm::jit::{JitContractBuilder, JitEvmExecutionContext};
use jitevm::test_data;
use std::error::Error;
use std::time::Instant;

fn main() -> Result<(), Box<dyn Error>> {
    let ops = test_data::get_code_ops_fibonacci();
    // TESTING BASIC OPERATIONS WITH EVMOP AND EVMCODE

    let code = EvmCode { ops: ops.clone() };
    let augmented_code = code.augment();

    assert!(code.to_bytes() == augmented_code.to_bytes());
    assert!(code == EvmCode::new_from_bytes(&augmented_code.to_bytes(), EvmOpParserMode::Strict)?);

    let bcode = test_data::get_code_bin_revm_test1();
    let code = EvmCode::new_from_bytes(&bcode, EvmOpParserMode::Lax)?;

    // TESTING JIT

    use inkwell::context::Context;
    let context = Context::create();
    let contract = JitContractBuilder::with_context("some-contract-address", &context)?
        .build(EvmCode { ops: ops.clone() }.augment().index())?;

    println!("Benchmark compiled execution ...");
    for _i in 0..10 {
        let mut holder = JitEvmExecutionContext::new();
        let measurement_now = Instant::now();
        contract.call(&mut holder)?;
        let measurement_runtime = measurement_now.elapsed();
        println!("measurement_runtime {:?}", measurement_runtime);
    }

    Ok(())
}
