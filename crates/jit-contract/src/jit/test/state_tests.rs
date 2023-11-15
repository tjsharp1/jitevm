use super::{expect_revert, expect_success, memory_gas_calc, test_jit};
use crate::jit::{gas, EvmOp, ExecutionResult, JitEvmExecutionContext, Success};
use alloy_primitives::U256;
use revm::db::InMemoryDB;
use revm_primitives::{Bytes, LatestSpec};

#[test]
fn operations_jit_test_revert() {
    let db = InMemoryDB::default();
    let mut execution_context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);

    let init_cost = gas::init_gas::<LatestSpec>(&[]);
    let push_cost = gas::const_cost::<LatestSpec>(EvmOp::Push(32, U256::ZERO));
    let mstore_cost = gas::const_cost::<LatestSpec>(EvmOp::Mstore);

    let ops = vec![
        EvmOp::Push(32, U256::from(0)),
        EvmOp::Push(32, U256::from(0)),
        EvmOp::Revert,
    ];

    let result = test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");

    let expected_gas = init_cost + 2 * push_cost;
    let expected_output = Bytes::new();

    expect_revert!(test_revert, result, expected_gas, expected_output);

    let return_data = U256::from(2123234345);

    let ops = vec![
        EvmOp::Push(32, return_data),
        EvmOp::Push(32, U256::from(0)),
        EvmOp::Mstore,
        EvmOp::Push(32, U256::from(0x20)),
        EvmOp::Push(32, U256::from(0x0)),
        EvmOp::Revert,
    ];

    let result = test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");

    let mem_gas = memory_gas_calc::<LatestSpec>(32);

    let expected_gas = init_cost + 4 * push_cost + mstore_cost + mem_gas;
    let expected_output = Bytes::copy_from_slice(&return_data.to_be_bytes_vec());

    expect_revert!(test_revert, result, expected_gas, expected_output);
}

#[test]
fn operations_jit_test_return() {
    let db = InMemoryDB::default();
    let mut execution_context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);

    let init_cost = gas::init_gas::<LatestSpec>(&[]);
    let push_cost = gas::const_cost::<LatestSpec>(EvmOp::Push(32, U256::ZERO));
    let mstore_cost = gas::const_cost::<LatestSpec>(EvmOp::Mstore);

    let ops = vec![
        EvmOp::Push(32, U256::from(0)),
        EvmOp::Push(32, U256::from(0)),
        EvmOp::Return,
    ];

    let result = test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");

    let expected_gas = init_cost + 2 * push_cost;
    let expected_refund = 0;
    let expected_output = Bytes::new();

    expect_success!(
        test_return,
        result,
        Success::Return,
        expected_gas,
        expected_refund,
        expected_output
    );

    let return_data = U256::from(2123234345);

    let ops = vec![
        EvmOp::Push(32, return_data),
        EvmOp::Push(32, U256::from(0)),
        EvmOp::Mstore,
        EvmOp::Push(32, U256::from(0x20)),
        EvmOp::Push(32, U256::from(0x0)),
        EvmOp::Return,
    ];

    let result = test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");

    let mem_gas = memory_gas_calc::<LatestSpec>(32);

    let expected_gas = init_cost + 4 * push_cost + mstore_cost + mem_gas;
    let expected_refund = 0;
    let expected_output = Bytes::copy_from_slice(&return_data.to_be_bytes_vec());

    expect_success!(
        test_return,
        result,
        Success::Return,
        expected_gas,
        expected_refund,
        expected_output
    );
}
