use super::{expect_halt, expect_success, test_jit};
use crate::jit::{gas, EvmOp, ExecutionResult, Halt, JitEvmExecutionContext, Success};
use crate::spec::SpecId;
use alloy_primitives::U256;
use rand::Rng;
use revm::db::InMemoryDB;
use std::ops::{BitAnd, Not};

#[test]
fn operations_jit_test_mload() {
    const CHUNKS: usize = 256;
    const BYTES: usize = 32;

    for _ in 0..1000 {
        let mut ops = Vec::new();
        let db = InMemoryDB::default();
        let mut execution_context = JitEvmExecutionContext::new_with_db(&db);
        let mut expected_memory = Vec::with_capacity(CHUNKS * BYTES);

        for _ in 0..CHUNKS {
            let values = rand::thread_rng().gen::<[u8; BYTES]>();
            expected_memory.extend(values);
        }

        execution_context.memory[..CHUNKS * BYTES].copy_from_slice(&expected_memory);

        let mut indices = Vec::new();
        let mem_range = 0..(expected_memory.len() - BYTES);

        let mut max = 0;
        for _ in 0..20 {
            let r = rand::thread_rng().gen_range(mem_range.clone());
            indices.push(r);

            max = max.max(r);
            ops.push(EvmOp::Push(32, U256::from(r)));
            ops.push(EvmOp::Mload);
        }

        let gas = gas::Gas::new(SpecId::LATEST);
        let push_cost = gas.const_cost(EvmOp::Push(32, U256::ZERO));
        let const_cost = gas.const_cost(EvmOp::Mload);
        let mem_cost = gas.memory_gas();
        let init_cost = gas.init_gas(&[]);

        let offset = (max + 32) as u64;
        let up = offset.bitand(31).not().wrapping_add(1).bitand(31);
        let size = up.checked_add(offset).expect("Overflow on add");
        let size_words = size / 32;

        let mem_gas = (size_words * size_words) / 512 + mem_cost * size_words;
        let expected_gas = init_cost + (push_cost + const_cost) * 20 + mem_gas;

        let result = test_jit(ops, &mut execution_context).expect("Contract build failed");
        expect_success!(test_mload, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { stack, .. } = execution_context;

        for (stackidx, memidx) in indices.iter().enumerate() {
            let range = *memidx..(memidx + BYTES);
            let mut bytes = [0u8; 32];
            bytes.copy_from_slice(&expected_memory[range]);
            let expected = U256::from_be_bytes(bytes);
            let got = stack[stackidx];
            assert_eq!(expected, got);
        }
    }
}

#[test]
fn operations_stack_underflow_mload() {
    use crate::code::EvmOp::*;

    let gas = gas::Gas::new(SpecId::LATEST);
    let push_cost = gas.const_cost(Push(32, U256::ZERO));
    let init_cost = gas.init_gas(&[]);

    let const_cost = gas.const_cost(EvmOp::Mload);
    let mem_cost = gas.memory_gas();

    let mut ops = Vec::new();

    for i in 0..1 {
        let mut cloned = ops.clone();
        cloned.push(Mload);

        let expected_gas = init_cost + push_cost * i;

        let db = InMemoryDB::default();
        let mut ctx = JitEvmExecutionContext::new_with_db(&db);
        let result = test_jit(cloned, &mut ctx).expect("Contract build failed");

        expect_halt!(mload, result, Halt::StackUnderflow, expected_gas);

        ops.push(Push(32, U256::from(i)));
    }
    ops.push(Mload);

    let offset = 32u64;
    let up = offset.bitand(31).not().wrapping_add(1).bitand(31);
    let size = up.checked_add(offset).expect("Overflow on add");
    let size_words = size / 32;

    let mem_gas = (size_words * size_words) / 512 + mem_cost * size_words;
    let expected_gas = init_cost + push_cost + const_cost + mem_gas;

    let db = InMemoryDB::default();
    let mut ctx = JitEvmExecutionContext::new_with_db(&db);
    let result = test_jit(ops, &mut ctx).expect("Contract build failed");

    expect_success!(mload, result, Success::Stop, expected_gas);
}

#[test]
fn operations_jit_test_mstore8() {
    const CHUNKS: usize = 256;
    const BYTES: usize = 32;

    for _ in 0..1000 {
        let mut ops = Vec::new();
        let db = InMemoryDB::default();
        let mut execution_context = JitEvmExecutionContext::new_with_db(&db);
        let mut expected_memory = vec![0u8; CHUNKS * BYTES];
        let mut indices = Vec::new();

        let gas = gas::Gas::new(SpecId::LATEST);
        let push_cost = gas.const_cost(EvmOp::Push(32, U256::ZERO));
        let init_cost = gas.init_gas(&[]);

        let const_cost = gas.const_cost(EvmOp::Mstore8);
        let mem_cost = gas.memory_gas();

        let mem_range = 0..(expected_memory.len() - BYTES);

        let mut max = 0;
        for _ in 0..20 {
            let offset = rand::thread_rng().gen_range(mem_range.clone());
            let value = rand::thread_rng().gen::<u8>();
            indices.push(offset);
            expected_memory[offset] = value;

            max = max.max(offset);
            ops.push(EvmOp::Push(32, U256::from(value)));
            ops.push(EvmOp::Push(32, U256::from(offset)));
            ops.push(EvmOp::Mstore8);
        }

        let result = test_jit(ops, &mut execution_context).expect("Contract build failed");

        let offset = (max + 1) as u64;
        let up = offset.bitand(31).not().wrapping_add(1).bitand(31);
        let size = up.checked_add(offset).expect("Overflow on add");
        let size_words = size / 32;

        let mem_gas = (size_words * size_words) / 512 + mem_cost * size_words;
        let expected_gas = init_cost + (push_cost * 2 + const_cost) * 20 + mem_gas;

        expect_success!(test_mstore8, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { memory, .. } = execution_context;

        for memidx in indices {
            let expected = expected_memory[memidx];
            let got = memory[memidx];

            assert_eq!(expected, got);
        }
    }
}

#[test]
fn operations_stack_underflow_mstore8() {
    use crate::code::EvmOp::*;

    let gas = gas::Gas::new(SpecId::LATEST);
    let push_cost = gas.const_cost(Push(32, U256::ZERO));
    let init_cost = gas.init_gas(&[]);

    let const_cost = gas.const_cost(EvmOp::Mstore8);
    let mem_cost = gas.memory_gas();

    let mut ops = Vec::new();

    for i in 0..2 {
        let mut cloned = ops.clone();
        cloned.push(Mstore8);

        let expected_gas = init_cost + push_cost * i;

        let db = InMemoryDB::default();
        let mut ctx = JitEvmExecutionContext::new_with_db(&db);
        let result = test_jit(cloned, &mut ctx).expect("Contract build failed");

        expect_halt!(mload, result, Halt::StackUnderflow, expected_gas);

        ops.push(Push(32, U256::from(i)));
    }
    ops.push(Mstore8);

    let offset = 32u64;
    let up = offset.bitand(31).not().wrapping_add(1).bitand(31);
    let size = up.checked_add(offset).expect("Overflow on add");
    let size_words = size / 32;

    let mem_gas = (size_words * size_words) / 512 + mem_cost * size_words;
    let expected_gas = init_cost + push_cost * 2 + const_cost + mem_gas;

    let db = InMemoryDB::default();
    let mut ctx = JitEvmExecutionContext::new_with_db(&db);
    let result = test_jit(ops, &mut ctx).expect("Contract build failed");

    expect_success!(mload, result, Success::Stop, expected_gas);
}

#[test]
fn operations_jit_test_mstore() {
    const CHUNKS: usize = 256;
    const BYTES: usize = 32;

    let gas = gas::Gas::new(SpecId::LATEST);
    let push_cost = gas.const_cost(EvmOp::Push(32, U256::ZERO));
    let init_cost = gas.init_gas(&[]);

    let const_cost = gas.const_cost(EvmOp::Mstore);
    let mem_cost = gas.memory_gas();

    for _ in 0..1000 {
        let mut ops = Vec::new();
        let db = InMemoryDB::default();
        let mut execution_context = JitEvmExecutionContext::new_with_db(&db);
        let mut expected_memory = vec![0u8; CHUNKS * BYTES];
        let mut indices = Vec::new();

        let mem_range = 0..(expected_memory.len() - BYTES);

        let mut max = 0;
        for _ in 0..20 {
            let offset = rand::thread_rng().gen_range(mem_range.clone());
            let value = rand::thread_rng().gen::<[u8; BYTES]>();
            indices.push(offset);
            expected_memory[offset..offset + BYTES].copy_from_slice(&value);

            max = max.max(offset);
            ops.push(EvmOp::Push(32, U256::from_be_bytes(value)));
            ops.push(EvmOp::Push(32, U256::from(offset)));
            ops.push(EvmOp::Mstore);
        }

        let result = test_jit(ops, &mut execution_context).expect("Contract build failed");

        let offset = (max + 32) as u64;
        let up = offset.bitand(31).not().wrapping_add(1).bitand(31);
        let size = up.checked_add(offset).expect("Overflow on add");
        let size_words = size / 32;

        let mem_gas = (size_words * size_words) / 512 + mem_cost * size_words;
        let expected_gas = init_cost + (push_cost * 2 + const_cost) * 20 + mem_gas;
        expect_success!(test_mstore, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { memory, .. } = execution_context;

        for memidx in indices {
            let range = memidx..memidx + BYTES;
            let expected = &expected_memory[range.clone()];
            let got = &memory[range];

            assert_eq!(expected, got);
        }
    }
}

#[test]
fn operations_stack_underflow_mstore() {
    use crate::code::EvmOp::*;

    let gas = gas::Gas::new(SpecId::LATEST);
    let push_cost = gas.const_cost(Push(32, U256::ZERO));
    let init_cost = gas.init_gas(&[]);

    let const_cost = gas.const_cost(EvmOp::Mstore);
    let mem_cost = gas.memory_gas();

    let mut ops = Vec::new();

    for i in 0..2 {
        let mut cloned = ops.clone();
        cloned.push(Mstore);

        let expected_gas = init_cost + push_cost * i;

        let db = InMemoryDB::default();
        let mut ctx = JitEvmExecutionContext::new_with_db(&db);
        let result = test_jit(cloned, &mut ctx).expect("Contract build failed");

        expect_halt!(mload, result, Halt::StackUnderflow, expected_gas);

        ops.push(Push(32, U256::from(i)));
    }
    ops.push(Mstore);

    let offset = (1 + 32) as u64;
    let up = offset.bitand(31).not().wrapping_add(1).bitand(31);
    let size = up.checked_add(offset).expect("Overflow on add");
    let size_words = size / 32;

    let mem_gas = (size_words * size_words) / 512 + mem_cost * size_words;
    let expected_gas = init_cost + push_cost * 2 + const_cost + mem_gas;

    let db = InMemoryDB::default();
    let mut ctx = JitEvmExecutionContext::new_with_db(&db);
    let result = test_jit(ops, &mut ctx).expect("Contract build failed");

    expect_success!(mload, result, Success::Stop, expected_gas);
}
