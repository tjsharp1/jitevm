use inkwell::context::Context;
use jit_contract::{
    code::{EvmCode, EvmOpParserMode},
    jit::{contract::JitContractBuilder, ExecutionResult, JitEvmExecutionContext, TransactionConfig},
};
use revm::{EVM, InMemoryDB};
use revm_primitives::{AccountInfo, Address, address, B256, Bytecode, Bytes, Env, HashMap, LatestSpec, ResultAndState, TransactTo, U256};
use serde::{Serialize, Deserialize};
use std::path::{Path, PathBuf};


#[derive(Clone, Serialize, Deserialize)]
pub struct DeployImage {
    pub address: Address,
    pub code_hash: B256,
    pub code: Bytes,
    pub storage: HashMap<U256, U256>,
}

impl DeployImage {
    pub fn new_from_bytecode(test_name: &str) -> DeployImage {
        let workspace_dir = workspace_dir();

        let mut path = PathBuf::new();
        path.push(workspace_dir);
        path.push("contracts");
        path.push(format!("{}.bc", test_name));

        let bytecode = std::fs::read_to_string(path).expect("Couldn't open test file");
        let bytes = hex::decode(bytecode).expect("Invalid hex data");

        let bytes = Bytes::copy_from_slice(&bytes);
        let bytecode = Bytecode::new_raw(bytes).to_checked();

        DeployImage {
            address: Address::ZERO,
            code_hash: bytecode.hash_slow(),
            code: bytecode.bytes().clone(),
            storage: HashMap::default(),
        }
    }
}

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

pub fn get_env_args(bundle: DeployImage) -> (Env, InMemoryDB) {
    let DeployImage {
        address,
        code_hash,
        code,
        storage,
    } = bundle;

    let bytecode = Bytecode::new_raw(code).to_checked();
    let mut database = InMemoryDB::default();

    let one_eth = U256::from(1).pow(U256::from(18));

    let mut info = AccountInfo {
        balance: one_eth * U256::from(10),
        nonce: 0,
        code_hash,
        code: Some(bytecode),
    };

    database.insert_contract(&mut info);
    database.insert_account_info(address, info);
    database
        .replace_account_storage(address, storage)
        .expect("Could not initialize storage");

    let mut env = Env::default();
    env.tx.caller = address!("Ef8801eaf234ff82801821FFe2d78D60a0237F97");
    env.tx.transact_to = TransactTo::Call(address);

    (env, database)
}

pub fn run_jit_with_code(bundle: DeployImage, calldata: Bytes) -> ExecutionResult {
    let code = EvmCode::new_from_bytes(&bundle.code, EvmOpParserMode::Strict)
        .expect("Failed parsing EVM code");

    let (_, database) = get_env_args(bundle.clone());

    let context = Context::create();
    let indexed = code.augment().index();
    let contract = JitContractBuilder::with_context("contract", &context)
        .expect("Could not build builder")
        .debug_ir("evm_equivalence.ll")
        .debug_asm("evm_equivalence.asm")
        .build(LatestSpec, indexed)
        .expect("Could not JIT contract");

    let mut cfg = TransactionConfig::default();
    cfg.caller = address!("Ef8801eaf234ff82801821FFe2d78D60a0237F97");
    cfg.transact_to = bundle.address;

    let mut ctx = JitEvmExecutionContext::builder(LatestSpec)
        .with_transaction_config(cfg)
        .with_calldata(calldata)
        // snailtracer needs about 10MB
        .memory_size(12 * 1024 * 1024)
        .build_with_db(&database);

    let result = contract
        .transact(&mut ctx)
        .expect("JIT contract call failed");

    result
}

pub fn run_evm_with_code(deploy: DeployImage, calldata: Bytes) -> ResultAndState {
    let (env, database) = get_env_args(deploy);

    let mut evm = EVM {
        env,
        db: Some(database),
    };
    evm.env.tx.data = calldata;

    evm.transact().expect("REVM execution failed")
}

macro_rules! assert_evm_jit_equivalence {
    ($image:ident, $calldata:ident) => {

        let ResultAndState { result, .. } = run_evm_with_code($image.clone(), $calldata.clone());

        let jit_result = run_jit_with_code($image, $calldata);

        match result {
            REVMExecutionResult::Success {
                reason,
                gas_used,
                gas_refunded,
                output,
                ..
            } => {
                let ExecutionResult::Success {
                    reason: jit_reason,
                    gas_used: jit_gas,
                    gas_refunded: jit_refund,
                    output: jit_output,
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
                match output {
                    Output::Call(bytes) => {
                        assert_eq!(bytes, jit_output, "EVM and JIT output not equivalent");
                    }
                    Output::Create(_, _) => todo!("Handle create outputs"),
                }
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
            REVMExecutionResult::Revert { gas_used, output } => {
                let ExecutionResult::Revert {
                    gas_used: jit_gas_used,
                    output: jit_output,
                } = jit_result
                else {
                    panic!("JIT dit not return revert when it should have!");
                };

                assert_eq!(
                    gas_used, jit_gas_used,
                    "REVERT: EVM and JIT gas not equivalent!"
                );
                assert_eq!(
                    output, jit_output,
                    "REVERT: EVM and JIT output not equivalent!"
                );
            }
        }
    };
}

pub(crate) use assert_evm_jit_equivalence;
