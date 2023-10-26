use crate::spec::SpecId;
use crate::jit::{
    ExecutionResult,
    EvmOp,
	Halt,
	JitEvmExecutionContext,
	gas,
	Success,
};
use paste::paste;
use primitive_types::U256;
use rand::Rng;
use std::collections::{HashSet, HashMap};
use super::{expect_halt, expect_success, expect_stack_underflow, test_jit};


#[test]
fn operations_jit_test_sstore() {
    for _ in 0..1000 {
        let mut expected_store = HashMap::new();
        let mut ops = Vec::new();

        for _ in 0..20 {
            let a = rand::thread_rng().gen::<[u8; 32]>();
            let b = rand::thread_rng().gen::<[u8; 32]>();

            let value = U256::from_big_endian(&a);
            let key = U256::from_big_endian(&b);

            expected_store.insert(key, value);

            ops.push(EvmOp::Push(32, value));
            ops.push(EvmOp::Push(32, key));
        }
        for _ in 0..20 {
            ops.push(EvmOp::Sstore);
        }

        let mut execution_context = JitEvmExecutionContext::new();
        let result = test_jit(ops, &mut execution_context).expect("Contract build failed");
        // TODO: need to do storage hot/cold tracking for the gas accounting.
        let expected_gas = 0;
        expect_success!(test_sstore, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { storage, .. } = execution_context;

        let expected_keys: HashSet<U256> = expected_store.keys().cloned().collect();
        let actual_keys: HashSet<U256> = storage.keys().cloned().collect();

        let diff = expected_keys.symmetric_difference(&actual_keys).count();
        assert_eq!(diff, 0);

        for (key, value) in expected_store.iter() {
            let stored = *storage.get(key).expect("Storage should have item");
            assert_eq!(*value, stored);
        }
    }
}
expect_stack_underflow!(sstore, EvmOp::Sstore, 2);

#[test]
fn operations_jit_test_sload() {
    for _ in 0..1000 {
        let mut ops = Vec::new();
        let mut expected_values = Vec::new();

        let mut execution_context = JitEvmExecutionContext::new();

        for _ in 0..20 {
            let a = rand::thread_rng().gen::<[u8; 32]>();
            let b = rand::thread_rng().gen::<[u8; 32]>();

            let value = U256::from_big_endian(&a);
            let key = U256::from_big_endian(&b);

            expected_values.push(value);
            execution_context.storage.insert(key, value);

            ops.push(EvmOp::Push(32, key));
        }
        for i in 0..20 {
            ops.push(EvmOp::Sload);
            ops.push(EvmOp::Push(32, U256::from(i * 32)));
            ops.push(EvmOp::Mstore);
        }

        let result = test_jit(ops, &mut execution_context).expect("Contract build failed");
        // TODO: need to do storage hot/cold tracking for the gas accounting.
        let expected_gas = 0;
        expect_success!(test_sload, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { memory, .. } = execution_context;

        let end = expected_values.len() - 1;
        for (index, value) in expected_values.iter().enumerate() {
            let offset = (end - index) * 32;

            let actual = U256::from_big_endian(&memory[offset..offset + 32]);
            assert_eq!(actual, *value);
        }
    }
}
expect_stack_underflow!(sload, EvmOp::Sload, 1);
