use super::{expect_halt, test_jit};
use crate::jit::{EvmOp, ExecutionResult, Halt, JitEvmExecutionContext};
use alloy_primitives::U256;
use revm::db::InMemoryDB;
use revm_primitives::LatestSpec;

#[test]
fn operations_jit_test_invalid() {
    let db = InMemoryDB::default();
    let mut execution_context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);

    let ops = vec![
        EvmOp::Push(32, U256::from(100)),
        EvmOp::Push(32, U256::from(101)),
        EvmOp::Push(32, U256::from(102)),
        EvmOp::Invalid,
    ];

    let result = test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");
    expect_halt!(test_invalid, result, Halt::InvalidFEOpcode);
}
