use super::{expect_halt, expect_stack_underflow, expect_success, test_jit};
use crate::jit::{gas, EvmOp, ExecutionResult, Halt, JitEvmExecutionContext, Success};
use alloy_primitives::{Address, U256};
use paste::paste;
use rand::Rng;
use revm::db::InMemoryDB;
use revm_primitives::LatestSpec;
use std::collections::{HashMap, HashSet};

#[test]
fn operations_jit_test_sstore() {
    for _ in 0..1000 {
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
        let result =
            test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");
        // TODO: need to do storage hot/cold tracking for the gas accounting.
        let expected_gas = 0;
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
}
expect_stack_underflow!(sstore, EvmOp::Sstore, 2);

#[test]
fn operations_jit_test_sload() {
    for _ in 0..1000 {
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
        let result =
            test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");
        // TODO: need to do storage hot/cold tracking for the gas accounting.
        let expected_gas = 0;
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
}
expect_stack_underflow!(sload, EvmOp::Sload, 1);
