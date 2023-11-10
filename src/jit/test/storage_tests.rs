use super::{expect_halt, expect_success, memory_gas_calc, test_jit};
use crate::jit::{gas, EvmOp, ExecutionResult, Halt, JitEvmExecutionContext, Success};
use alloy_primitives::{Address, U256};
use rand::Rng;
use revm::db::InMemoryDB;
use revm_primitives::LatestSpec;
use std::collections::{HashMap, HashSet};

#[test]
fn operations_jit_test_sstore_refunds() {
    use crate::jit::EvmOp::*;

    fn setup_state(values: Vec<U256>) -> InMemoryDB {
        let mut db = InMemoryDB::default();
        let address = Default::default();
        db.insert_account_info(address, Default::default());

        for (idx, v) in values.iter().enumerate() {
            db.insert_account_storage(address, U256::from(idx), *v)
                .expect("Failed insertion");
        }
        db
    }

    let init_cost = gas::init_gas::<LatestSpec>(&[]);
    let push_cost = gas::const_cost::<LatestSpec>(EvmOp::Push(32, U256::ZERO));
    let base_cost = init_cost + 2 * push_cost;

    let values = vec![1, 5, 7].into_iter().map(|v| U256::from(v)).collect();
    let db = setup_state(values);

    // unchanged, no refund
    let ops = vec![Push(32, U256::from(1)), Push(32, U256::ZERO), Sstore];

    let mut execution_context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
    let result = test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");

    let (sstore_cost, _) =
        gas::sstore_gas::<LatestSpec>(U256::from(1), U256::from(1), U256::from(1), false);

    let expected_gas = base_cost + sstore_cost;

    expect_success!(test_sstore, result, Success::Stop, expected_gas);

    // back to zero, refund
    let values = vec![1, 5, 7].into_iter().map(|v| U256::from(v)).collect();
    let db = setup_state(values);

    let ops = vec![Push(32, U256::ZERO), Push(32, U256::from(1)), Sstore];

    let mut execution_context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
    let result = test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");

    let (sstore_cost, refund) =
        gas::sstore_gas::<LatestSpec>(U256::from(1), U256::from(1), U256::ZERO, false);

    let expected_gas = base_cost + sstore_cost - refund as u64;

    expect_success!(test_sstore, result, Success::Stop, expected_gas, refund);

    let values = vec![1, 5, 7].into_iter().map(|v| U256::from(v)).collect();
    let db = setup_state(values);

    // just change value, no refunds
    let ops = vec![Push(32, U256::from(1)), Push(32, U256::from(2)), Sstore];

    let mut execution_context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
    let result = test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");

    let (sstore_cost, refund) =
        gas::sstore_gas::<LatestSpec>(U256::from(1), U256::from(1), U256::from(2), false);

    let expected_gas = base_cost + sstore_cost - refund as u64;

    expect_success!(test_sstore, result, Success::Stop, expected_gas, refund);

    let values = vec![1, 5, 7].into_iter().map(|v| U256::from(v)).collect();
    let db = setup_state(values);

    // same slot, current value != original
    let ops = vec![
        Push(32, U256::from(1)),
        Push(32, U256::from(2)),
        Sstore,
        Push(32, U256::ZERO),
        Push(32, U256::from(2)),
        Sstore,
    ];

    let mut execution_context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
    let result = test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");

    let (sstore_cost1, refund1) =
        gas::sstore_gas::<LatestSpec>(U256::from(7), U256::from(7), U256::from(1), false);
    let (sstore_cost2, refund2) =
        gas::sstore_gas::<LatestSpec>(U256::from(7), U256::from(1), U256::ZERO, true);

    let refund = refund1 as u64 + refund2 as u64;
    let expected_gas = base_cost + 2 * push_cost + sstore_cost1 + sstore_cost2 - refund;

    expect_success!(test_sstore, result, Success::Stop, expected_gas, refund);

    let values = vec![1, 5, 7].into_iter().map(|v| U256::from(v)).collect();
    let db = setup_state(values);

    // same slot, current value != original
    let ops = vec![
        Push(32, U256::from(1)),
        Push(32, U256::from(9)),
        Sstore,
        Push(32, U256::ZERO),
        Push(32, U256::from(9)),
        Sstore,
    ];

    let mut execution_context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
    let result = test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");

    let (sstore_cost1, refund1) =
        gas::sstore_gas::<LatestSpec>(U256::ZERO, U256::ZERO, U256::from(1), false);
    let (sstore_cost2, refund2) =
        gas::sstore_gas::<LatestSpec>(U256::ZERO, U256::from(1), U256::ZERO, true);

    let refund = refund1 as u64 + refund2 as u64;
    let expected_gas = base_cost + sstore_cost1 + sstore_cost2 + 2 * push_cost - refund;

    expect_success!(test_sstore, result, Success::Stop, expected_gas, refund);
}

#[test]
fn operations_jit_test_sstore() {
    let mut expected_store = HashMap::new();
    let mut ops = Vec::new();

    for _ in 0..20 {
        let a = rand::thread_rng().gen::<[u8; 32]>();
        let b = rand::thread_rng().gen::<[u8; 32]>();

        let value = U256::from_be_bytes(a);
        let key = U256::from_be_bytes(b);

        expected_store.insert(key, value);

        ops.push(EvmOp::Push(32, value));
        ops.push(EvmOp::Push(32, key));
    }
    for _ in 0..20 {
        ops.push(EvmOp::Sstore);
    }

    let db = InMemoryDB::default();
    let mut execution_context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
    let result = test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");

    let init_cost = gas::init_gas::<LatestSpec>(&[]);
    let push_cost = gas::const_cost::<LatestSpec>(EvmOp::Push(32, U256::ZERO));
    let (sstore_cost, _refund) =
        gas::sstore_gas::<LatestSpec>(U256::ZERO, U256::ZERO, U256::from(1), false);

    let expected_gas = init_cost + push_cost * 40 + sstore_cost * 20;
    expect_success!(test_sstore, result, Success::Stop, expected_gas);

    let mut state = execution_context.final_state();
    let account = state
        .remove(&Address::ZERO)
        .expect("Should be zero address");

    let expected_keys: HashSet<U256> = expected_store.keys().cloned().collect();
    let actual_keys: HashSet<U256> = account.storage.keys().cloned().collect();

    let diff = expected_keys.symmetric_difference(&actual_keys).count();
    assert_eq!(diff, 0);

    for (key, value) in expected_store.iter() {
        let stored = account.storage.get(key).expect("Storage should have item");
        assert_eq!(*value, stored.present_value);
    }
}

#[test]
fn operations_stack_underflow_sstore() {
    use crate::code::EvmOp::*;

    let push_gas = gas::const_cost::<LatestSpec>(Push(32, U256::ZERO));
    let init_cost = gas::init_gas::<LatestSpec>(&[]);

    let mut ops = Vec::new();

    for i in 0..2 {
        let mut cloned = ops.clone();
        cloned.push(EvmOp::Sstore);

        let db = InMemoryDB::default();

        let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
        let result = test_jit(LatestSpec, cloned, &mut ctx).expect("Contract build failed");

        expect_halt!(stack_underflow_sstore, result, Halt::StackUnderflow);

        ops.push(Push(32, U256::from(i)));
    }
    ops.push(EvmOp::Sstore);

    let (sstore_cost, _refund) =
        gas::sstore_gas::<LatestSpec>(U256::ZERO, U256::ZERO, U256::ZERO, false);

    let expected_gas = init_cost + push_gas * 2 + sstore_cost;

    let db = InMemoryDB::default();
    let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
    let result = test_jit(LatestSpec, ops, &mut ctx).expect("Contract build failed");

    expect_success!(stack_underflow_sstore, result, Success::Stop, expected_gas);
}

#[test]
fn operations_jit_test_sload() {
    let mut ops = Vec::new();
    let mut expected_values = Vec::new();

    let mut db = InMemoryDB::default();

    for _ in 0..20 {
        let a = rand::thread_rng().gen::<[u8; 32]>();
        let b = rand::thread_rng().gen::<[u8; 32]>();

        let value = U256::from_be_bytes(a);
        let key = U256::from_be_bytes(b);

        expected_values.push(value);
        db.insert_account_storage(Address::ZERO, key, value)
            .expect("Insert storage error");

        ops.push(EvmOp::Push(32, key));
    }
    for i in 0..20 {
        ops.push(EvmOp::Sload);
        ops.push(EvmOp::Push(32, U256::from(i * 32)));
        ops.push(EvmOp::Mstore);
    }

    let mut execution_context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
    let result = test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");

    let init_cost = gas::init_gas::<LatestSpec>(&[]);
    let push_cost = gas::const_cost::<LatestSpec>(EvmOp::Push(32, U256::ZERO));
    let sload_cost = gas::sload_gas::<LatestSpec>(false);
    let const_cost = gas::const_cost::<LatestSpec>(EvmOp::Mstore);

    let mem_gas = memory_gas_calc::<LatestSpec>(0x20 * 20u64);

    let expected_gas = init_cost + push_cost * 40 + sload_cost * 20 + mem_gas + const_cost * 20;

    expect_success!(test_sload, result, Success::Stop, expected_gas);

    let JitEvmExecutionContext { memory, .. } = execution_context;

    let end = expected_values.len() - 1;
    for (index, value) in expected_values.iter().enumerate() {
        let offset = (end - index) * 32;

        let mut bytes = [0u8; 32];
        bytes.copy_from_slice(&memory[offset..offset + 32]);
        let actual = U256::from_be_bytes(bytes);
        assert_eq!(actual, *value);
    }
}

#[test]
fn operations_stack_underflow_sload() {
    use crate::code::EvmOp::*;

    let push_gas = gas::const_cost::<LatestSpec>(Push(32, U256::ZERO));
    let init_cost = gas::init_gas::<LatestSpec>(&[]);

    let mut ops = Vec::new();

    for i in 0..1 {
        let mut cloned = ops.clone();
        cloned.push(EvmOp::Sload);

        let db = InMemoryDB::default();

        let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
        let result = test_jit(LatestSpec, cloned, &mut ctx).expect("Contract build failed");

        expect_halt!(stack_underflow_sload, result, Halt::StackUnderflow);

        ops.push(Push(32, U256::from(i)));
    }
    ops.push(EvmOp::Sload);

    let sload_cost = gas::sload_gas::<LatestSpec>(false);

    let expected_gas = init_cost + push_gas + sload_cost;

    let db = InMemoryDB::default();
    let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
    let result = test_jit(LatestSpec, ops, &mut ctx).expect("Contract build failed");

    expect_success!(stack_underflow_sload, result, Success::Stop, expected_gas);
}
