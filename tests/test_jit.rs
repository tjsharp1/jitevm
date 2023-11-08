use inkwell::context::Context;
use jitevm::code::{EvmCode, EvmOpParserMode};
use jitevm::jit::{
    contract::JitContractBuilder, ExecutionResult, Halt, JitEvmExecutionContext, Success,
};
use revm::{
    db::in_memory_db::BenchmarkDB,
    primitives::{
        address, Bytecode, Bytes, Env, Eval, ExecutionResult as REVMExecutionResult,
        Halt as REVMHalt, OutOfGasError, ResultAndState,
    },
    EVM,
};
use revm_primitives::LatestSpec;
use std::path::PathBuf;

fn test_jit_with_code(code: EvmCode) -> ExecutionResult {
    let context = Context::create();
    let contract = JitContractBuilder::with_context("contract", &context)
        .expect("Could not build builder")
        .debug_ir("evm_equivalence.ll")
        .debug_asm("evm_equivalence.asm")
        .build(LatestSpec, code.augment().index())
        .expect("Could not JIT contract");

    let bytes = Bytes::copy_from_slice(&code.to_bytes());
    let bytecode = Bytecode::new_raw(bytes).to_checked();
    let database = BenchmarkDB::new_bytecode(bytecode);

    let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&database);
    let result = contract
        .transact(&mut ctx)
        .expect("JIT contract call failed");
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
        let ResultAndState { result, .. } = test_evm_with_code(code);

        println!("TJDEBUG evmresult {:#?}", result);
        println!("TJDEBUG jitresult {:#?}", jit_result);
        match result {
            REVMExecutionResult::Success {
                reason,
                gas_used,
                gas_refunded,
                ..
            } => {
                let ExecutionResult::Success {
                    reason: jit_reason,
                    gas_used: jit_gas,
                    gas_refunded: jit_refund,
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
                assert_eq!(
                    gas_refunded, jit_refund,
                    "EVM and JIT refund not equivalent"
                );
            }
            REVMExecutionResult::Halt { reason, gas_used } => {
                let ExecutionResult::Halt {
                    reason: jit_reason,
                    gas_used: jit_gas_used,
                } = jit_result
                else {
                    panic!("JIT did not return halt when it should have!");
                };

                assert_eq!(gas_used, jit_gas_used, "EVM and JIT gas not equivalent!");

                match reason {
                    REVMHalt::OutOfGas(oog) => match oog {
                        OutOfGasError::BasicOutOfGas => {
                            assert_eq!(jit_reason, Halt::OutOfGasBasicOutOfGas)
                        }
                        OutOfGasError::MemoryLimit => {
                            assert_eq!(jit_reason, Halt::OutOfGasMemoryLimit)
                        }
                        OutOfGasError::Memory => assert_eq!(jit_reason, Halt::OutOfGasMemory),
                        OutOfGasError::Precompile => {
                            assert_eq!(jit_reason, Halt::OutOfGasPrecompile)
                        }
                        OutOfGasError::InvalidOperand => {
                            assert_eq!(jit_reason, Halt::OutOfGasInvalidOperand)
                        }
                    },
                    REVMHalt::OpcodeNotFound => assert_eq!(jit_reason, Halt::OpcodeNotFound),
                    REVMHalt::InvalidFEOpcode => assert_eq!(jit_reason, Halt::InvalidFEOpcode),
                    REVMHalt::InvalidJump => assert_eq!(jit_reason, Halt::InvalidJump),
                    REVMHalt::NotActivated => assert_eq!(jit_reason, Halt::NotActivated),
                    REVMHalt::StackUnderflow => assert_eq!(jit_reason, Halt::StackUnderflow),
                    REVMHalt::StackOverflow => assert_eq!(jit_reason, Halt::StackOverflow),
                    REVMHalt::OutOfOffset => assert_eq!(jit_reason, Halt::OutOfOffset),
                    REVMHalt::CreateCollision => assert_eq!(jit_reason, Halt::CreateCollision),
                    REVMHalt::PrecompileError => assert_eq!(jit_reason, Halt::PrecompileError),
                    REVMHalt::NonceOverflow => assert_eq!(jit_reason, Halt::NonceOverflow),
                    REVMHalt::CreateContractSizeLimit => {
                        assert_eq!(jit_reason, Halt::CreateContractSizeLimit)
                    }
                    REVMHalt::CreateContractStartingWithEF => {
                        assert_eq!(jit_reason, Halt::CreateContractStartingWithEF)
                    }
                    REVMHalt::CreateInitcodeSizeLimit => {
                        assert_eq!(jit_reason, Halt::CreateInitcodeSizeLimit)
                    }
                    REVMHalt::OverflowPayment => assert_eq!(jit_reason, Halt::OverflowPayment),
                    REVMHalt::StateChangeDuringStaticCall => {
                        assert_eq!(jit_reason, Halt::StateChangeDuringStaticCall)
                    }
                    REVMHalt::CallNotAllowedInsideStatic => {
                        assert_eq!(jit_reason, Halt::CallNotAllowedInsideStatic)
                    }
                    REVMHalt::OutOfFund => assert_eq!(jit_reason, Halt::OutOfFund),
                    REVMHalt::CallTooDeep => assert_eq!(jit_reason, Halt::CallTooDeep),
                }
            }
            _ => unimplemented!("Not implemented for revert results!"),
        }
    };
}

#[test]
fn test_evm_and_jit_fibonacci() {
    assert_evm_jit_equivalence!(fibonacci);
}

#[test]
fn test_evm_and_jit_fibonacci_repetitions() {
    assert_evm_jit_equivalence!(fibonacci_repetitions);
}

#[test]
fn test_evm_and_jit_exp() {
    assert_evm_jit_equivalence!(exp);
}

#[test]
fn test_evm_and_jit_mload() {
    assert_evm_jit_equivalence!(mload);
}

#[test]
fn test_evm_and_jit_mstore8() {
    assert_evm_jit_equivalence!(mstore8);
}

#[test]
fn test_evm_and_jit_sha3() {
    assert_evm_jit_equivalence!(sha3);
}

#[test]
fn test_evm_and_jit_sstore() {
    assert_evm_jit_equivalence!(store);
}

#[test]
fn test_evm_and_jit_sload() {
    assert_evm_jit_equivalence!(sload);
}

#[test]
fn test_evm_and_jit_reth_random_stuff() {
    assert_evm_jit_equivalence!(reth_random_stuff);
}

#[test]
fn test_evm_and_jit_snailtracer() {
    assert_evm_jit_equivalence!(snailtracer);
}
