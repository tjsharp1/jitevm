use super::{expect_halt, expect_success, memory_gas_calc, test_jit};
use crate::jit::{gas, EvmOp, ExecutionResult, Halt, JitEvmExecutionContext, Success};
use alloy_primitives::U256;
use rand::Rng;
use revm::db::InMemoryDB;
use revm_primitives::LatestSpec;

#[test]
fn operations_jit_test_mload() {
    const CHUNKS: usize = 256;
    const BYTES: usize = 32;

    for _ in 0..1000 {
        let mut ops = Vec::new();
        let db = InMemoryDB::default();
        let mut execution_context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
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

        let push_cost = gas::const_cost::<LatestSpec>(EvmOp::Push(32, U256::ZERO));
        let const_cost = gas::const_cost::<LatestSpec>(EvmOp::Mload);
        let init_cost = gas::init_gas::<LatestSpec>(&[]);

        let mem_gas = memory_gas_calc::<LatestSpec>(max as u64 + 32);
        let expected_gas = init_cost + (push_cost + const_cost) * 20 + mem_gas;

        let result =
            test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");
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

    let push_cost = gas::const_cost::<LatestSpec>(Push(32, U256::ZERO));
    let init_cost = gas::init_gas::<LatestSpec>(&[]);

    let const_cost = gas::const_cost::<LatestSpec>(EvmOp::Mload);

    let mut ops = Vec::new();

    for i in 0..1 {
        let mut cloned = ops.clone();
        cloned.push(Mload);

        let db = InMemoryDB::default();
        let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
        let result = test_jit(LatestSpec, cloned, &mut ctx).expect("Contract build failed");

        expect_halt!(mload, result, Halt::StackUnderflow);

        ops.push(Push(32, U256::from(i)));
    }
    ops.push(Mload);

    let mem_gas = memory_gas_calc::<LatestSpec>(32);
    let expected_gas = init_cost + push_cost + const_cost + mem_gas;

    let db = InMemoryDB::default();
    let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
    let result = test_jit(LatestSpec, ops, &mut ctx).expect("Contract build failed");

    expect_success!(mload, result, Success::Stop, expected_gas);
}

#[test]
fn operations_jit_test_mstore8() {
    const CHUNKS: usize = 256;
    const BYTES: usize = 32;

    for _ in 0..1000 {
        let mut ops = Vec::new();
        let db = InMemoryDB::default();
        let mut execution_context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
        let mut expected_memory = vec![0u8; CHUNKS * BYTES];
        let mut indices = Vec::new();

        let push_cost = gas::const_cost::<LatestSpec>(EvmOp::Push(32, U256::ZERO));
        let init_cost = gas::init_gas::<LatestSpec>(&[]);

        let const_cost = gas::const_cost::<LatestSpec>(EvmOp::Mstore8);

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

        let result =
            test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");

        let mem_gas = memory_gas_calc::<LatestSpec>(max as u64 + 1);
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

    let push_cost = gas::const_cost::<LatestSpec>(Push(32, U256::ZERO));
    let init_cost = gas::init_gas::<LatestSpec>(&[]);

    let const_cost = gas::const_cost::<LatestSpec>(EvmOp::Mstore8);

    let mut ops = Vec::new();

    for i in 0..2 {
        let mut cloned = ops.clone();
        cloned.push(Mstore8);

        let db = InMemoryDB::default();
        let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
        let result = test_jit(LatestSpec, cloned, &mut ctx).expect("Contract build failed");

        expect_halt!(mload, result, Halt::StackUnderflow);

        ops.push(Push(32, U256::from(i)));
    }
    ops.push(Mstore8);

    let mem_gas = memory_gas_calc::<LatestSpec>(32);
    let expected_gas = init_cost + push_cost * 2 + const_cost + mem_gas;

    let db = InMemoryDB::default();
    let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
    let result = test_jit(LatestSpec, ops, &mut ctx).expect("Contract build failed");

    expect_success!(mload, result, Success::Stop, expected_gas);
}

#[test]
fn operations_jit_test_mstore() {
    const CHUNKS: usize = 256;
    const BYTES: usize = 32;

    let push_cost = gas::const_cost::<LatestSpec>(EvmOp::Push(32, U256::ZERO));
    let init_cost = gas::init_gas::<LatestSpec>(&[]);

    let const_cost = gas::const_cost::<LatestSpec>(EvmOp::Mstore);

    for _ in 0..1000 {
        let mut ops = Vec::new();
        let db = InMemoryDB::default();
        let mut execution_context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
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

        let result =
            test_jit(LatestSpec, ops, &mut execution_context).expect("Contract build failed");

        let mem_gas = memory_gas_calc::<LatestSpec>(max as u64 + 32);
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

    let push_cost = gas::const_cost::<LatestSpec>(Push(32, U256::ZERO));
    let init_cost = gas::init_gas::<LatestSpec>(&[]);

    let const_cost = gas::const_cost::<LatestSpec>(EvmOp::Mstore);

    let mut ops = Vec::new();

    for i in 0..2 {
        let mut cloned = ops.clone();
        cloned.push(Mstore);

        let db = InMemoryDB::default();
        let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
        let result = test_jit(LatestSpec, cloned, &mut ctx).expect("Contract build failed");

        expect_halt!(mload, result, Halt::StackUnderflow);

        ops.push(Push(32, U256::from(i)));
    }
    ops.push(Mstore);

    let mem_gas = memory_gas_calc::<LatestSpec>(1 + 32);
    let expected_gas = init_cost + push_cost * 2 + const_cost + mem_gas;

    let db = InMemoryDB::default();
    let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
    let result = test_jit(LatestSpec, ops, &mut ctx).expect("Contract build failed");

    expect_success!(mload, result, Success::Stop, expected_gas);
}
