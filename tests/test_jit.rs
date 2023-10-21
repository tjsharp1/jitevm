use inkwell::context::Context;
use jitevm::code::{EvmCode, EvmOpParserMode};
use jitevm::jit::{contract::JitContractBuilder, ExecutionResult, JitEvmExecutionContext, Success};
use primitive_types::U256;
use revm::{
    db::in_memory_db::BenchmarkDB,
    primitives::{
        address, Bytecode, Bytes, Env, Eval, ExecutionResult as REVMExecutionResult, ResultAndState,
    },
    EVM,
};
use std::path::PathBuf;


fn test_jit_with_code(code: EvmCode) -> ExecutionResult {
    let context = Context::create();
    let contract = JitContractBuilder::with_context("contract", &context)
        .expect("Could not build builder")
        .debug_ir("evm_equivalence.ll")
        .build(code.augment().index())
        .expect("Could not JIT contract");

    let mut holder = JitEvmExecutionContext::new();
    holder.transaction_context.set_gas_limit(u64::MAX);
    let result = contract
        .call(&mut holder)
        .expect("JIT contract call failed");
    // TJDEBUG ///////////////////////////////////
    let JitEvmExecutionContext { storage, .. } = holder;
    println!("TJDEBUG {:#?}", storage.keys());
    println!("TJDEBUG {:#?}", storage.values());
    //println!("TJDEBUG {:x}", storage.get(&U256::zero()).unwrap());
    // TJDEBUG ///////////////////////////////////
    result
}

fn test_evm_with_code(code: EvmCode) -> ResultAndState {
    let bytes = Bytes::copy_from_slice(&code.to_bytes());
    let bytecode = Bytecode::new_raw(bytes).to_checked();
    let database = BenchmarkDB::new_bytecode(bytecode);

    let mut env = Env::default();
    env.tx.caller = address!("Ef8801eaf234ff82801821FFe2d78D60a0237F97");

    let mut evm = EVM {
        env,
        db: Some(database),
    };
    evm.transact().expect("Bad evm stuff")
}

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

macro_rules! assert_evm_jit_equivalence {
    ($test_data:ident) => {
        let name = stringify!($test_data);
        let code = load_evm_code(name);

        let jit_result = test_jit_with_code(code.clone());
        let ResultAndState { result, state } = test_evm_with_code(code);

        match result {
            REVMExecutionResult::Success {
                reason, gas_used, ..
            } => {
                let ExecutionResult::Success {
                    reason: jit_reason,
                    gas_used: jit_gas,
                } = jit_result
                else {
                    panic!("JIT did not return success when it should have!");
                };

                match reason {
                    Eval::Stop => assert_eq!(jit_reason, Success::Stop),
                    Eval::Return => assert_eq!(jit_reason, Success::Return),
                    Eval::SelfDestruct => assert_eq!(jit_reason, Success::SelfDestruct),
                }

                assert_eq!(gas_used, jit_gas, "EVM and JIT gas not equivalent");
            }
            _ => unimplemented!("Not implemented for error results!"),
        }
    };
}

#[test]
fn test_evm_and_jit() {
    assert_evm_jit_equivalence!(fibonacci);
    assert_evm_jit_equivalence!(fibonacci_repetitions);
}
