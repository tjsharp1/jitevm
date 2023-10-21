use crate::{
    code::EvmOp,
    constants::EVM_STACK_SIZE,
    jit::{
        contract::JitContractBuilder, gas, ExecutionResult, Halt, JitEvmEngineError,
        JitEvmExecutionContext, Success,
    },
    spec::SpecId,
};
use paste::paste;
use primitive_types::{H160, H256, U256};
use rand::{prelude::SliceRandom, Rng, RngCore};
use sha3::{Digest, Keccak256};
use std::collections::{HashMap, HashSet};

mod i256;
mod operations;

fn test_jit(
    ops: Vec<EvmOp>,
    execution_context: &mut JitEvmExecutionContext,
) -> Result<ExecutionResult, JitEvmEngineError> {
    use crate::code::EvmCode;
    use inkwell::context::Context;

    let context = Context::create();
    let contract = JitContractBuilder::with_context("jit-instructions", &context)
        .expect("Could not build jit contract")
        .debug_ir("jit_test.ll")
        .debug_asm("jit_test.asm")
        .build(EvmCode { ops: ops.clone() }.augment().index())?;
    Ok(contract
        .call(execution_context)
        .expect("Contract call failed"))
}

macro_rules! expect_success {
    ($fname:ident, $result:ident, $reason:expr, $gas:ident) => {
        let name_str = stringify!($fname);

        match $result {
            ExecutionResult::Success { reason, gas_used } => {
                assert_eq!(
                    reason, $reason,
                    "expect_success - {}: expected {:?}, got {:?}",
                    name_str, $reason, reason
                );
                assert_eq!(
                    gas_used, $gas,
                    "expect_success - {}: incorrect gas usage.",
                    name_str
                );
            }
            o => panic!(
                "expect_success - {}: Expected success, got: {:?}",
                name_str, o
            ),
        }
    };
}

//macro_rules! expect_revert {
//    ($fname:ident, $result:ident) => {
//        let name_str = stringify!($fname);
//
//        match $result {
//            ExecutionResult::Revert => {}
//            o => panic!("{}: Expected revert, got: {:?}", name_str, o),
//        }
//    };
//}

macro_rules! expect_halt {
    ($fname:ident, $result:ident, $reason:expr, $gas:ident) => {
        let name_str = stringify!($fname);

        match $result {
            ExecutionResult::Halt { reason, gas_used } => {
                assert_eq!(
                    reason, $reason,
                    "expect_halt - {}: expected {:?}, got {:?}",
                    name_str, $reason, reason
                );
                assert_eq!(
                    gas_used, $gas,
                    "expect_halt - {}: incorrect gas usage.",
                    name_str
                );
            }
            o => panic!("expect_halt - {}: Expected halt, got: {:?}", name_str, o),
        }
    };
}

macro_rules! expect_stack_overflow {
    ($fname:ident, $evmop:expr, $stack_growth:literal) => {
        paste! {
            #[test]
            fn [<operations_stack_overflow_ $fname>]() {
                use crate::code::EvmOp::*;

                let gas = gas::Gas::new(SpecId::LATEST);
                let init_cost = gas.init_gas(&[]);
                let push_gas = gas.const_cost(Push(32, U256::zero()));
                let op_cost = gas.const_cost($evmop);

                let mut ops = vec![Push(32, U256::zero()); EVM_STACK_SIZE];

                for i in 0..$stack_growth {
                    ops.push($evmop);

                    let mut ctx = JitEvmExecutionContext::new();
                    let result = test_jit(ops.clone(), &mut ctx).expect("Contract build failed");

                    let pushes = (EVM_STACK_SIZE - i) as u64;
                    let expected_gas = init_cost + pushes * push_gas + op_cost;

                    expect_halt!($fname, result, Halt::StackOverflow, expected_gas);

                    ops.pop();
                    ops.pop();
                }
                ops.push($evmop);

                let pushes = (EVM_STACK_SIZE - $stack_growth) as u64;
                let expected_gas = init_cost + pushes * push_gas + op_cost;

                let mut ctx = JitEvmExecutionContext::new();
                let result = test_jit(ops, &mut ctx).expect("Contract build failed");

                expect_success!($fname, result, Success::Stop, expected_gas);
            }
        }
    };
}

macro_rules! expect_stack_underflow {
    ($fname:ident, $evmop:expr, $min_stack:literal) => {
        paste! {
            #[test]
            fn [<operations_stack_underflow_ $fname>]() {
                use crate::code::EvmOp::*;

                let gas = gas::Gas::new(SpecId::LATEST);
                let push_gas = gas.const_cost(Push(32, U256::zero()));
                let init_cost = gas.init_gas(&[]);

                let op_cost = if gas.is_const_gas($evmop) {
                    gas.const_cost($evmop)
                } else {
                    // TODO: non-const gas ops
                    0
                };

                let mut ops = Vec::new();

                for i in 0..$min_stack {
                    let mut cloned = ops.clone();
                    cloned.push($evmop);

                    let expected_gas = init_cost + push_gas * i + op_cost;

                    let mut ctx = JitEvmExecutionContext::new();
                    let result = test_jit(cloned, &mut ctx).expect("Contract build failed");

                    expect_halt!($fname, result, Halt::StackUnderflow, expected_gas);

                    ops.push(Push(32, U256::one()*i));
                }
                ops.push($evmop);

                let expected_gas = init_cost + push_gas * $min_stack + op_cost;

                let mut ctx = JitEvmExecutionContext::new();
                let result = test_jit(ops, &mut ctx).expect("Contract build failed");

                expect_success!($fname, result, Success::Stop, expected_gas);
            }
        }
    };
}

macro_rules! check_context {
    ($evmop:expr, $setter:ident, $ty:ident) => {{
        for _ in 0..1000 {
            let mut context = JitEvmExecutionContext::new();

            let mut expected_store = HashMap::new();
            let mut expected_mem = Vec::new();
            let mut ops = Vec::new();

            check_context!($setter, expected_mem, context, expected_store, $ty);

            ops.push($evmop);
            ops.push(EvmOp::Push(32, U256::zero()));
            ops.push(EvmOp::Sstore);
            ops.push(EvmOp::Push(32, U256::zero()));
            ops.push(EvmOp::Sload);
            ops.push(EvmOp::Push(32, U256::zero()));
            ops.push(EvmOp::Mstore);

            let result = test_jit(ops, &mut context).expect("Contract build failed");

            // TODO: need context gas
            let expected_gas = 0;
            expect_success!($setter, result, Success::Stop, expected_gas);

            let JitEvmExecutionContext {
                storage, memory, ..
            } = context;

            let expected_keys: HashSet<U256> = expected_store.keys().cloned().collect();
            let actual_keys: HashSet<U256> = storage.keys().cloned().collect();

            let diff = expected_keys.symmetric_difference(&actual_keys).count();
            assert_eq!(diff, 0);

            for (key, value) in expected_store.iter() {
                let stored = *storage.get(key).expect("Storage should have item");
                let start = key.as_usize() * 0x20;
                let mem_range = start..start + 0x20;
                assert_eq!(memory[mem_range.clone()], expected_mem[mem_range.clone()]);
                assert_eq!(*value, stored);
            }
        }
    }};
    ($setter:ident, $mem:ident, $ctx:ident, $store:ident, u256) => {{
        let val = rand::thread_rng().gen::<[u8; 32]>();
        $mem.extend(val.to_vec());
        let val = U256::from(val);
        $ctx.block_context.$setter(val);
        $store.insert(U256::zero(), val);
    }};
    ($setter:ident, $mem:ident, $ctx:ident, $store:ident, h160) => {{
        let mut val = rand::thread_rng().gen::<[u8; 32]>();
        val[..12].copy_from_slice(&[0u8; 12]);
        $mem.extend(val.to_vec());
        $ctx.block_context.$setter(H160::from_slice(&val[12..]));
        $store.insert(U256::zero(), U256::from_big_endian(&val));
    }};
    ($setter:ident, $mem:ident, $ctx:ident, $store:ident, h256) => {{
        let val = rand::thread_rng().gen::<[u8; 32]>();
        $mem.extend(val.to_vec());
        $ctx.block_context.$setter(H256::from_slice(&val));
        $store.insert(U256::zero(), U256::from_big_endian(&val));
    }};
}

#[test]
fn operations_jit_test_block_context_number() {
    check_context!(EvmOp::Number, set_number, u256);
}
expect_stack_overflow!(number, EvmOp::Number, 1);

#[test]
fn operations_jit_test_block_context_coinbase() {
    check_context!(EvmOp::Coinbase, set_coinbase, h160);
}
expect_stack_overflow!(coinbase, EvmOp::Coinbase, 1);

#[test]
fn operations_jit_test_block_context_timestamp() {
    check_context!(EvmOp::Timestamp, set_timestamp, u256);
}
expect_stack_overflow!(timestamp, EvmOp::Timestamp, 1);

#[test]
fn operations_jit_test_block_context_randao() {
    check_context!(EvmOp::PrevRandao, set_prevrandao, h256);
}
expect_stack_overflow!(randao, EvmOp::PrevRandao, 1);

#[test]
fn operations_jit_test_block_context_basefee() {
    check_context!(EvmOp::BaseFee, set_basefee, u256);
}
expect_stack_overflow!(basefee, EvmOp::BaseFee, 1);

#[test]
fn operations_jit_test_block_context_gas_limit() {
    check_context!(EvmOp::GasLimit, set_gas_limit, u256);
}
expect_stack_overflow!(gas_limit, EvmOp::GasLimit, 1);

#[test]
fn operations_jit_test_stop() {
    for _ in 0..1000 {
        let mut execution_context = JitEvmExecutionContext::new();

        let mut expected_store = HashMap::new();
        let mut ops = Vec::new();

        expected_store.insert(U256::zero(), U256::one());
        ops.push(EvmOp::Push(32, U256::one()));
        ops.push(EvmOp::Push(32, U256::zero()));
        ops.push(EvmOp::Sstore);

        expected_store.insert(U256::one(), U256::one() * 2);
        ops.push(EvmOp::Push(32, U256::one() * 2));
        ops.push(EvmOp::Push(32, U256::one()));
        ops.push(EvmOp::Sstore);

        ops.push(EvmOp::Stop);
        ops.push(EvmOp::Push(32, U256::one() * 3));
        ops.push(EvmOp::Push(32, U256::one()));
        ops.push(EvmOp::Sstore);
        ops.push(EvmOp::Push(32, U256::one() * 8));
        ops.push(EvmOp::Push(32, U256::zero()));
        ops.push(EvmOp::Sstore);

        let result = test_jit(ops, &mut execution_context).expect("Contract build failed");
        //TODO: need to do stop gas
        let expected_gas = 0;
        expect_success!(test_stop, result, Success::Stop, expected_gas);

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

#[test]
fn operations_test_jump() {
    for _ in 0..1000 {
        let mut execution_context = JitEvmExecutionContext::new();
        let jumpdest_offset = 0x20;

        let mut expected_store = HashMap::new();
        let mut ops = Vec::new();

        ops.push(EvmOp::Push(32, U256::from(jumpdest_offset)));
        ops.push(EvmOp::Mload);
        ops.push(EvmOp::Jump);
        ops.push(EvmOp::Push(32, U256::from(1)));
        ops.push(EvmOp::Push(32, U256::from(0)));
        ops.push(EvmOp::Sstore);
        ops.push(EvmOp::Push(32, U256::from(1)));
        ops.push(EvmOp::Push(32, U256::from(1)));
        ops.push(EvmOp::Sstore);

        let bytes = ops.iter().fold(0usize, |a, op| a + op.len());
        let jumpdest = U256::from(bytes);

        ops.push(EvmOp::Push(32, U256::from(1)));
        ops.push(EvmOp::Push(32, U256::from(1)));

        let mut slice = &mut execution_context.memory[jumpdest_offset..jumpdest_offset + 0x20];
        jumpdest.to_big_endian(&mut slice);

        let result = test_jit(ops.clone(), &mut execution_context);
        assert_eq!(result, Err(JitEvmEngineError::NoValidJumpDestinations(2)));

        let key0 = U256::from(0);
        let value0 = U256::from(9);
        let key1 = U256::from(1);
        let value1 = U256::from(90);

        expected_store.insert(key0, value0);
        expected_store.insert(key1, value1);

        // remove the ops that should've been jumpdest above
        ops.pop();
        ops.pop();
        ops.push(EvmOp::Jumpdest);
        ops.push(EvmOp::Push(32, value0));
        ops.push(EvmOp::Push(32, key0));
        ops.push(EvmOp::Sstore);
        ops.push(EvmOp::Push(32, value1));
        ops.push(EvmOp::Push(32, key1));
        ops.push(EvmOp::Sstore);

        let result = test_jit(ops.clone(), &mut execution_context).expect("Should return OK()");
        // TODO: need to do jump gas
        let expected_gas = 0;
        expect_success!(test_jump, result, Success::Stop, expected_gas);

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

#[test]
fn test_jump_underflow() {
    let gas = gas::Gas::new(SpecId::LATEST);
    let init_cost = gas.init_gas(&[]);
    let expected_gas = init_cost + gas.const_cost(EvmOp::Jump);
    let mut execution_context = JitEvmExecutionContext::new();

    let ops = vec![EvmOp::Jump, EvmOp::Jumpdest];
    let result = test_jit(ops, &mut execution_context).expect("Should return OK()");
    expect_halt!(jump_underflow, result, Halt::StackUnderflow, expected_gas);
}

#[test]
fn operations_test_jumpi() {
    for _ in 0..1000 {
        let mut execution_context = JitEvmExecutionContext::new();
        let jumpdest_offset = 0x20;

        let mut expected_store = HashMap::new();
        let mut ops = Vec::new();

        let zero = U256::zero();
        let val = rand::thread_rng().gen::<[u8; 32]>();
        let nonzero = U256::from_big_endian(&val);
        let choices = [zero, nonzero];
        let condition = choices
            .choose(&mut rand::thread_rng())
            .expect("Should make random choice");
        let key0_branch0 = U256::from(0);
        let value0_branch0 = U256::from(1);
        let key1_branch0 = U256::from(1);
        let value1_branch0 = U256::from(8);

        ops.push(EvmOp::Push(32, *condition));
        ops.push(EvmOp::Push(32, U256::from(jumpdest_offset)));
        ops.push(EvmOp::Mload);
        ops.push(EvmOp::Jumpi);
        ops.push(EvmOp::Push(32, value0_branch0));
        ops.push(EvmOp::Push(32, key0_branch0));
        ops.push(EvmOp::Sstore);
        ops.push(EvmOp::Push(32, value1_branch0));
        ops.push(EvmOp::Push(32, key1_branch0));
        ops.push(EvmOp::Sstore);
        ops.push(EvmOp::Stop);

        let bytes = ops.iter().fold(0usize, |a, op| a + op.len());
        let jumpdest = U256::from(bytes);

        // push a couple non-jumpdest instructions, to check jumpdest error
        ops.push(EvmOp::Push(32, U256::from(1)));
        ops.push(EvmOp::Push(32, U256::from(1)));

        let mut slice = &mut execution_context.memory[jumpdest_offset..jumpdest_offset + 0x20];
        jumpdest.to_big_endian(&mut slice);

        let result = test_jit(ops.clone(), &mut execution_context);
        assert_eq!(result, Err(JitEvmEngineError::NoValidJumpDestinations(3)));

        let key0_branch1 = U256::from(0);
        let value0_branch1 = U256::from(9);
        let key1_branch1 = U256::from(1);
        let value1_branch1 = U256::from(90);

        if *condition == U256::zero() {
            expected_store.insert(key0_branch0, value0_branch0);
            expected_store.insert(key1_branch0, value1_branch0);
        } else {
            expected_store.insert(key0_branch1, value0_branch1);
            expected_store.insert(key1_branch1, value1_branch1);
        }

        // remove the ops that should've been jumpdest above
        ops.pop();
        ops.pop();
        ops.push(EvmOp::Jumpdest);
        ops.push(EvmOp::Push(32, value0_branch1));
        ops.push(EvmOp::Push(32, key0_branch1));
        ops.push(EvmOp::Sstore);
        ops.push(EvmOp::Push(32, value1_branch1));
        ops.push(EvmOp::Push(32, key1_branch1));
        ops.push(EvmOp::Sstore);

        let result = test_jit(ops.clone(), &mut execution_context).expect("Should return OK()");
        // TODO: need to do jump gas accounting.
        let expected_gas = 0;
        expect_success!(test_jumpi, result, Success::Stop, expected_gas);

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

#[test]
fn test_jumpi_underflow() {
    // TODO: need to do jump gas accounting.
    let expected_gas = 0;
    let mut execution_context = JitEvmExecutionContext::new();

    let ops = vec![EvmOp::Jumpi, EvmOp::Jumpdest];
    let result = test_jit(ops, &mut execution_context).expect("Should return OK()");
    expect_halt!(jumpi_underflow1, result, Halt::StackUnderflow, expected_gas);

    let mut execution_context = JitEvmExecutionContext::new();

    let ops = vec![
        EvmOp::Push(32, U256::one()),
        EvmOp::Mload,
        EvmOp::Jumpi,
        EvmOp::Jumpdest,
    ];
    let result = test_jit(ops, &mut execution_context).expect("Should return OK()");
    expect_halt!(jumpi_underflow2, result, Halt::StackUnderflow, expected_gas);
}

#[test]
fn operations_test_augmented_jump() {
    let mut execution_context = JitEvmExecutionContext::new();

    let mut expected_store = HashMap::new();
    let mut ops = Vec::new();

    ops.push(EvmOp::Push(32, U256::zero()));
    ops.push(EvmOp::Jump);
    ops.push(EvmOp::Push(32, U256::from(1)));
    ops.push(EvmOp::Push(32, U256::from(0)));
    ops.push(EvmOp::Sstore);
    ops.push(EvmOp::Push(32, U256::from(1)));
    ops.push(EvmOp::Push(32, U256::from(1)));
    ops.push(EvmOp::Sstore);

    let bytes = ops.iter().fold(0usize, |a, op| a + op.len());
    let jumpdest = U256::from(bytes);

    ops[0] = EvmOp::Push(32, jumpdest);

    ops.push(EvmOp::Push(32, U256::from(1)));
    ops.push(EvmOp::Push(32, U256::from(1)));

    let result = test_jit(ops.clone(), &mut execution_context);
    assert_eq!(result, Err(JitEvmEngineError::NoValidJumpDestinations(0)));

    let key0 = U256::from(0);
    let value0 = U256::from(9);
    let key1 = U256::from(1);
    let value1 = U256::from(90);

    expected_store.insert(key0, value0);
    expected_store.insert(key1, value1);

    // remove the ops that should've been jumpdest above
    ops.pop();
    ops.pop();
    ops.push(EvmOp::Jumpdest);
    ops.push(EvmOp::Push(32, value0));
    ops.push(EvmOp::Push(32, key0));
    ops.push(EvmOp::Sstore);
    ops.push(EvmOp::Push(32, value1));
    ops.push(EvmOp::Push(32, key1));
    ops.push(EvmOp::Sstore);

    let result = test_jit(ops.clone(), &mut execution_context).expect("Should return OK()");
    // TODO: need to do augmented jump gas accounting.
    let expected_gas = 0;
    expect_success!(augmented_jump, result, Success::Stop, expected_gas);

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

#[test]
fn operations_test_augmented_jumpi() {
    for _ in 0..1000 {
        let mut execution_context = JitEvmExecutionContext::new();

        let mut expected_store = HashMap::new();
        let mut ops = Vec::new();

        let zero = U256::zero();
        let val = rand::thread_rng().gen::<[u8; 32]>();
        let nonzero = U256::from_big_endian(&val);
        let choices = [zero, nonzero];
        let condition = choices
            .choose(&mut rand::thread_rng())
            .expect("Should make random choice");
        let key0_branch0 = U256::from(0);
        let value0_branch0 = U256::from(1);
        let key1_branch0 = U256::from(1);
        let value1_branch0 = U256::from(8);

        ops.push(EvmOp::Push(32, *condition));
        ops.push(EvmOp::Push(32, U256::zero()));
        ops.push(EvmOp::Jumpi);
        ops.push(EvmOp::Push(32, value0_branch0));
        ops.push(EvmOp::Push(32, key0_branch0));
        ops.push(EvmOp::Sstore);
        ops.push(EvmOp::Push(32, value1_branch0));
        ops.push(EvmOp::Push(32, key1_branch0));
        ops.push(EvmOp::Sstore);
        ops.push(EvmOp::Stop);

        let bytes = ops.iter().fold(0usize, |a, op| a + op.len());
        let jumpdest = U256::from(bytes);

        ops[1] = EvmOp::Push(32, jumpdest);

        // push a couple non-jumpdest instructions, to check jumpdest error
        ops.push(EvmOp::Push(32, U256::from(1)));
        ops.push(EvmOp::Push(32, U256::from(1)));

        let result = test_jit(ops.clone(), &mut execution_context);
        assert_eq!(result, Err(JitEvmEngineError::NoValidJumpDestinations(1)));

        let key0_branch1 = U256::from(0);
        let value0_branch1 = U256::from(9);
        let key1_branch1 = U256::from(1);
        let value1_branch1 = U256::from(90);

        if *condition == U256::zero() {
            expected_store.insert(key0_branch0, value0_branch0);
            expected_store.insert(key1_branch0, value1_branch0);
        } else {
            expected_store.insert(key0_branch1, value0_branch1);
            expected_store.insert(key1_branch1, value1_branch1);
        }

        // remove the ops that should've been jumpdest above
        ops.pop();
        ops.pop();
        ops.push(EvmOp::Jumpdest);
        ops.push(EvmOp::Push(32, value0_branch1));
        ops.push(EvmOp::Push(32, key0_branch1));
        ops.push(EvmOp::Sstore);
        ops.push(EvmOp::Push(32, value1_branch1));
        ops.push(EvmOp::Push(32, key1_branch1));
        ops.push(EvmOp::Sstore);

        let result = test_jit(ops.clone(), &mut execution_context).expect("Should be Ok()");
        // TODO: need to do jumpi gas accounting.
        let expected_gas = 0;
        expect_success!(augmented_jumpi, result, Success::Stop, expected_gas);

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

#[test]
fn operations_jit_test_sha3() {
    for _ in 0..1000 {
        let mut execution_context = JitEvmExecutionContext::new();

        let mut expected_store = HashMap::new();
        let mut ops = Vec::new();

        let mut offset = 0;
        let mem_range = 0..512;
        let mut a = vec![0u8; 512];

        for i in 0..20 {
            rand::thread_rng().fill_bytes(&mut a);
            let r = rand::thread_rng().gen_range(mem_range.clone());

            let off_range = offset..(offset + r);
            execution_context.memory[off_range].copy_from_slice(&a[..r]);

            let value = U256::from(Keccak256::digest(&a[..r]).as_slice());
            expected_store.insert(U256::from(i), value);

            ops.push(EvmOp::Push(32, U256::from(r)));
            ops.push(EvmOp::Push(32, U256::from(offset)));
            ops.push(EvmOp::Sha3);
            ops.push(EvmOp::Push(32, U256::from(i)));
            ops.push(EvmOp::Sstore);

            offset += r;
        }

        let result = test_jit(ops, &mut execution_context).expect("Contract build failed");
        // TODO: need to do sha3 gas accounting.
        let expected_gas = 0;
        expect_success!(test_sha3, result, Success::Stop, expected_gas);

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
expect_stack_underflow!(sha3, EvmOp::Sha3, 2);

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

#[test]
fn operations_jit_test_mload() {
    const CHUNKS: usize = 256;
    const BYTES: usize = 32;

    for _ in 0..1000 {
        let mut ops = Vec::new();
        let mut execution_context = JitEvmExecutionContext::new();
        let mut expected_memory = Vec::with_capacity(CHUNKS * BYTES);

        for _ in 0..CHUNKS {
            let values = rand::thread_rng().gen::<[u8; BYTES]>();
            expected_memory.extend(values);
        }

        execution_context.memory[..CHUNKS * BYTES].copy_from_slice(&expected_memory);

        let mut indices = Vec::new();
        let mem_range = 0..(expected_memory.len() - BYTES);

        for _ in 0..20 {
            let r = rand::thread_rng().gen_range(mem_range.clone());
            indices.push(r);

            ops.push(EvmOp::Push(32, U256::one() * r));
            ops.push(EvmOp::Mload);
        }

        let result = test_jit(ops, &mut execution_context).expect("Contract build failed");
        // TODO: need to do memory growth tracking for the gas accounting.
        let expected_gas = 0;
        expect_success!(test_mload, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { stack, .. } = execution_context;

        for (stackidx, memidx) in indices.iter().enumerate() {
            let range = *memidx..(memidx + BYTES);
            let expected = U256::from_big_endian(&expected_memory[range]);
            let got = stack[stackidx];
            assert_eq!(expected, got);
        }
    }
}
expect_stack_underflow!(mload, EvmOp::Mload, 1);

#[test]
fn operations_jit_test_mstore8() {
    const CHUNKS: usize = 256;
    const BYTES: usize = 32;

    for _ in 0..1000 {
        let mut ops = Vec::new();
        let mut execution_context = JitEvmExecutionContext::new();
        let mut expected_memory = vec![0u8; CHUNKS * BYTES];
        let mut indices = Vec::new();

        let mem_range = 0..(expected_memory.len() - BYTES);

        for _ in 0..20 {
            let offset = rand::thread_rng().gen_range(mem_range.clone());
            let value = rand::thread_rng().gen::<u8>();
            indices.push(offset);
            expected_memory[offset] = value;

            ops.push(EvmOp::Push(32, U256::one() * value));
            ops.push(EvmOp::Push(32, U256::one() * offset));
            ops.push(EvmOp::Mstore8);
        }

        let result = test_jit(ops, &mut execution_context).expect("Contract build failed");
        // TODO: need to do memory growth tracking for the gas accounting.
        let expected_gas = 0;
        expect_success!(test_mstore8, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { memory, .. } = execution_context;

        for memidx in indices {
            let expected = expected_memory[memidx];
            let got = memory[memidx];

            assert_eq!(expected, got);
        }
    }
}
expect_stack_underflow!(mstore8, EvmOp::Mstore8, 2);

#[test]
fn operations_jit_test_mstore() {
    const CHUNKS: usize = 256;
    const BYTES: usize = 32;

    for _ in 0..1000 {
        let mut ops = Vec::new();
        let mut execution_context = JitEvmExecutionContext::new();
        let mut expected_memory = vec![0u8; CHUNKS * BYTES];
        let mut indices = Vec::new();

        let mem_range = 0..(expected_memory.len() - BYTES);

        for _ in 0..20 {
            let offset = rand::thread_rng().gen_range(mem_range.clone());
            let value = rand::thread_rng().gen::<[u8; BYTES]>();
            indices.push(offset);
            expected_memory[offset..offset + BYTES].copy_from_slice(&value);

            ops.push(EvmOp::Push(32, U256::from_big_endian(&value)));
            ops.push(EvmOp::Push(32, U256::one() * offset));
            ops.push(EvmOp::Mstore);
        }

        let result = test_jit(ops, &mut execution_context).expect("Contract build failed");
        // TODO: need to do memory growth tracking for the gas accounting.
        let expected_gas = 0;
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
expect_stack_underflow!(mstore, EvmOp::Mstore, 2);

macro_rules! test_op1 {
    ($fname:ident, $evmop:expr, $opname:expr) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;
                use operations::*;

                fn _test(a: U256) {
                    let mut ctx = JitEvmExecutionContext::new();

                    let gas = gas::Gas::new(SpecId::LATEST);
                    let push_cost = gas.const_cost(Push(32, U256::zero()));
                    let op_cost = gas.const_cost($evmop);
                    let init_cost = gas.init_gas(&[]);
                    let expected_gas = init_cost + push_cost + op_cost;

                    let result = test_jit(vec![
                        Push(32, a),
                        $evmop,
                    ], &mut ctx).expect("Contract build failed");

                    expect_success!($fname, result, Success::Stop, expected_gas);

                    let JitEvmExecutionContext { stack, .. } = ctx;
                    let d = stack[0];
                    let d_ = $opname(a);
                    if d != d_ {
                        println!("a = {:?} / d = {:?} / d' = {:?}", a, d, d_);
                    }
                    assert_eq!(d, d_);
                }

                _test(U256::zero());
                _test(U256::one());

                for _i in 0..1000 {
                    let a = rand::thread_rng().gen::<[u8; 32]>();
                    let a = U256::from_big_endian(&a);
                    _test(a);
                }
            }
        }
        expect_stack_underflow!($fname, $evmop, 1);
    };
}

#[test]
fn operations_jit_equivalence_exp() {
    use crate::code::EvmOp::*;
    use operations;

    fn _test(a: U256, b: U256) {
        let mut ctx = JitEvmExecutionContext::new();

        let gas = gas::Gas::new(SpecId::LATEST);
        let push_cost = gas.const_cost(Push(32, U256::zero()));
        let (base, exp) = gas.exp_cost();
        let init_cost = gas.init_gas(&[]);

        let zeros = (256 - b.leading_zeros()) as u64;
        let bytes = (zeros / 8) + if zeros % 8 == 0 { 0 } else { 1 };
        let expected_gas = init_cost + push_cost * 2 + base + exp * bytes;

        let result = test_jit(vec![Push(32, b), Push(32, a), EvmOp::Exp], &mut ctx)
            .expect("Contract build failed");

        expect_success!(equivalence_exp, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { stack, .. } = ctx;
        let d = stack[0];
        let d_ = operations::exp(a, b);
        if d != d_ {
            println!("a = {:?} / b = {:?} \n\n d = {:?} / d' = {:?}", a, b, d, d_);
        }
        assert_eq!(d, d_);
    }

    _test(U256::zero(), U256::zero());
    _test(U256::zero(), U256::one());
    _test(U256::one(), U256::zero());
    _test(U256::one(), U256::one());

    for _i in 0..1000 {
        let a = rand::thread_rng().gen::<[u8; 32]>();
        let b = rand::thread_rng().gen::<[u8; 32]>();
        let a = U256::from_big_endian(&a);
        let b = U256::from_big_endian(&b);
        _test(a, b);
    }
}

#[test]
fn operations_underflow_exp() {
    use crate::code::EvmOp::*;

    let gas = gas::Gas::new(SpecId::LATEST);
    let push_gas = gas.const_cost(Push(32, U256::zero()));
    let init_cost = gas.init_gas(&[]);
    let (base, exp) = gas.exp_cost();

    let mut ops = Vec::new();

    for i in 0..2 {
        let mut cloned = ops.clone();
        cloned.push(EvmOp::Exp);

        let expected_gas = init_cost + push_gas * i;

        let mut ctx = JitEvmExecutionContext::new();
        let result = test_jit(cloned, &mut ctx).expect("Contract build failed");

        expect_halt!(
            operations_underflow_exp,
            result,
            Halt::StackUnderflow,
            expected_gas
        );

        ops.push(Push(32, U256::one() * (i + 1)));
    }
    ops.push(EvmOp::Exp);

    let expected_gas = init_cost + push_gas * 2 + base + exp;

    let mut ctx = JitEvmExecutionContext::new();
    let result = test_jit(ops, &mut ctx).expect("Contract build failed");

    expect_success!(
        operations_underflow_exp,
        result,
        Success::Stop,
        expected_gas
    );
}

macro_rules! test_op2 {
    ($fname:ident, $evmop:expr, $opname:expr) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;
                use operations;

                fn _test(a: U256, b: U256) {
                    let mut ctx = JitEvmExecutionContext::new();

                    let gas = gas::Gas::new(SpecId::LATEST);
                    let push_cost = gas.const_cost(Push(32, U256::zero()));
                    let op_cost = gas.const_cost($evmop);
                    let init_cost = gas.init_gas(&[]);
                    let expected_gas = init_cost + push_cost * 2 + op_cost;

                    let result = test_jit(vec![
                        Push(32, b),
                        Push(32, a),
                        $evmop,
                    ], &mut ctx).expect("Contract build failed");

                    expect_success!($fname, result, Success::Stop, expected_gas);

                    let JitEvmExecutionContext { stack, .. } = ctx;
                    let d = stack[0];
                    let d_ = $opname(a, b);
                    if d != d_ {
                        println!("a = {:?} / b = {:?} \n\n d = {:?} / d' = {:?}", a, b, d, d_);
                    }
                    assert_eq!(d, d_);
                }

                _test(U256::zero(), U256::zero());
                _test(U256::zero(), U256::one());
                _test(U256::one(), U256::zero());
                _test(U256::one(), U256::one());

                for _i in 0..1000 {
                    let a = rand::thread_rng().gen::<[u8; 32]>();
                    let b = rand::thread_rng().gen::<[u8; 32]>();
                    let a = U256::from_big_endian(&a);
                    let b = U256::from_big_endian(&b);
                    _test(a, b);
                }
            }
        }
        expect_stack_underflow!($fname, $evmop, 2);
    };
}

macro_rules! test_op2_small {
    ($fname:ident, $evmop:expr, $opname:expr) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;
                use operations;

                fn _test(a: U256, b: U256) {
                    let mut ctx = JitEvmExecutionContext::new();

                    let gas = gas::Gas::new(SpecId::LATEST);
                    let push_cost = gas.const_cost(Push(32, U256::zero()));
                    let op_cost = gas.const_cost($evmop);
                    let init_cost = gas.init_gas(&[]);
                    let expected_gas = init_cost + push_cost * 2 + op_cost;

                    let result = test_jit(vec![
                        Push(32, b),
                        Push(32, a),
                        $evmop,
                    ], &mut ctx).expect("Contract build failed");

                    expect_success!($fname, result, Success::Stop, expected_gas);

                    let JitEvmExecutionContext { stack, .. } = ctx;

                    let d = stack[0];
                    let d_ = $opname(a, b);
                    if d != d_ {
                        println!("a = {:?} / b = {:?} \n\n d = {:?} / d' = {:?}", a, b, d, d_);
                    }
                    assert_eq!(d, d_);
                }

                _test(U256::zero(), U256::zero());
                _test(U256::zero(), U256::one());
                _test(U256::one(), U256::zero());
                _test(U256::one(), U256::one());

                for _i in 0..1000 {
                    let t = rand::thread_rng().gen::<u8>();
                    let b = rand::thread_rng().gen::<[u8; 32]>();

                    let mut a: [u8; 32] = [0u8; 32];
                    a[31] = t;
                    let a = U256::from_big_endian(&a);
                    let b = U256::from_big_endian(&b);
                    _test(a, b);
                }
            }
        }
        expect_stack_underflow!($fname, $evmop, 2);
    };
}

macro_rules! test_op_dup {
    ($fname:ident, $evmop:expr, $position:literal) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;

                fn _test(values: Vec<U256>) {
                    let original_stack = values.clone();

                    let gas = gas::Gas::new(SpecId::LATEST);
                    let push_cost = gas.const_cost(Push(32, U256::zero()));
                    let op_cost = gas.const_cost($evmop);
                    let init_cost = gas.init_gas(&[]);
                    let expected_gas = init_cost + push_cost * values.len() as u64 + op_cost;

                    let mut ops = values
                        .into_iter()
                        .map(|v| Push(32, v))
                        .collect::<Vec<_>>();
                    ops.push($evmop);
                    let mut ctx = JitEvmExecutionContext::new();

                    let result = test_jit(ops.clone(), &mut ctx).expect("Contract build failed");

                    expect_success!($fname, result, Success::Stop, expected_gas);
                    let JitEvmExecutionContext { stack, .. } = ctx;

                    let d = &stack[..original_stack.len() + 1];
                    assert_eq!(d[original_stack.len()], d[original_stack.len() - $position]);
                    // TODO: did sp update correctly?
                    for (orig,output) in original_stack.iter().zip(d.iter()) {
                        assert_eq!(orig, output);
                    }
                }

                for _i in 0..1000 {
                    let data = (0..20).map(|_| {
                        let a = rand::thread_rng().gen::<[u8; 32]>();
                        U256::from_big_endian(&a)
                    }).collect();

                    _test(data);
                }
            }
        }
        expect_stack_underflow!($fname, $evmop, $position);
        expect_stack_overflow!($fname, $evmop, 1);
    };
}

macro_rules! test_op_swap {
    ($fname:ident, $evmop:expr, $min_stack:literal) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;
                const POSITION: usize = $min_stack - 1;

                fn _test(values: Vec<U256>) {
                    let original_stack = values.clone();

                    let gas = gas::Gas::new(SpecId::LATEST);
                    let push_cost = gas.const_cost(Push(32, U256::zero()));
                    let op_cost = gas.const_cost($evmop);
                    let init_cost = gas.init_gas(&[]);
                    let expected_gas = init_cost + push_cost * values.len() as u64 + op_cost;

                    let mut ops = values
                        .into_iter()
                        .map(|v| Push(32, v))
                        .collect::<Vec<_>>();
                    ops.push($evmop);

                    let mut ctx = JitEvmExecutionContext::new();
                    let result = test_jit(ops.clone(), &mut ctx).expect("Contract build failed");
                    expect_success!($fname, result, Success::Stop, expected_gas);

                    let JitEvmExecutionContext { stack, .. } = ctx;
                    let d = &stack[..original_stack.len()];
                    let top = original_stack.len() - 1;
                    assert_eq!(original_stack[top], d[top - POSITION]);
                    assert_eq!(original_stack[top - POSITION], d[top]);

                    for (index, value) in original_stack.iter().enumerate() {
                        if index == top || index == top - POSITION {
                            assert!(*value != d[index]);
                        } else {
                            assert_eq!(*value, d[index]);
                        }
                    }
                }

                for _i in 0..1000 {
                    let data = (0..20).map(|_| {
                        let a = rand::thread_rng().gen::<[u8; 32]>();
                        U256::from_big_endian(&a)
                    }).collect();

                    _test(data);
                }
            }
        }
        expect_stack_underflow!($fname, $evmop, $min_stack);
    };
}

macro_rules! test_op3 {
    ($fname:ident, $evmop:expr, $opname:expr) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;
                use operations;

                fn _test(a: U256, b: U256, c: U256) {
                    let mut ctx = JitEvmExecutionContext::new();

                    let gas = gas::Gas::new(SpecId::LATEST);
                    let push_cost = gas.const_cost(Push(32, U256::zero()));
                    let op_cost = gas.const_cost($evmop);
                    let init_cost = gas.init_gas(&[]);
                    let expected_gas = init_cost + push_cost * 3 + op_cost;

                    let result = test_jit(vec![
                        Push(32, c),
                        Push(32, b),
                        Push(32, a),
                        $evmop,
                    ], &mut ctx).expect("Contract build failed");

                    expect_success!($fname, result, Success::Stop, expected_gas);

                    let JitEvmExecutionContext { stack, .. } = ctx;
                    let d = stack[0];
                    let d_ = $opname(a, b, c);
                    if d != d_ {
                        println!("a = {:?} / d = {:?} / d' = {:?}", a, d, d_);
                    }
                    assert_eq!(d, d_);
                }

                _test(U256::zero(), U256::zero(), U256::zero());
                _test(U256::zero(), U256::zero(), U256::one());
                _test(U256::zero(), U256::one(), U256::zero());
                _test(U256::zero(), U256::one(), U256::one());
                _test(U256::one(), U256::zero(), U256::zero());
                _test(U256::one(), U256::zero(), U256::one());
                _test(U256::one(), U256::one(), U256::zero());
                _test(U256::one(), U256::one(), U256::one());

                for _i in 0..1000 {
                    let a = rand::thread_rng().gen::<[u8; 32]>();
                    let b = rand::thread_rng().gen::<[u8; 32]>();
                    let c = rand::thread_rng().gen::<[u8; 32]>();
                    let a = U256::from_big_endian(&a);
                    let b = U256::from_big_endian(&b);
                    let c = U256::from_big_endian(&c);

                    _test(a, b, c);
                }
            }
        }
        expect_stack_underflow!($fname, $evmop, 3);
    };
}

test_op1!(iszero, EvmOp::Iszero, operations::iszero);
test_op1!(not, EvmOp::Not, operations::not);
test_op2!(add, EvmOp::Add, operations::add);
test_op2!(sub, EvmOp::Sub, operations::sub);
test_op2!(mul, EvmOp::Mul, operations::mul);
test_op2!(div, EvmOp::Div, operations::div);
test_op2!(sdiv, EvmOp::Sdiv, operations::sdiv);
test_op2!(mod, EvmOp::Mod, operations::rem);
test_op2!(eq, EvmOp::Eq, operations::eq);
test_op2!(lt, EvmOp::Lt, operations::lt);
test_op2!(gt, EvmOp::Gt, operations::gt);
test_op2!(slt, EvmOp::Slt, operations::slt);
test_op2!(sgt, EvmOp::Sgt, operations::sgt);
test_op2!(and, EvmOp::And, operations::and);
test_op2!(or, EvmOp::Or, operations::or);
test_op2!(xor, EvmOp::Xor, operations::xor);
test_op2!(smod, EvmOp::Smod, operations::smod);
test_op2_small!(shl, EvmOp::Shl, operations::shl);
test_op2_small!(shr, EvmOp::Shr, operations::shr);
test_op2_small!(sar, EvmOp::Sar, operations::sar);
test_op2_small!(signextend, EvmOp::Signextend, operations::signextend);
test_op2_small!(byte, EvmOp::Byte, operations::byte);
test_op3!(addmod, EvmOp::Addmod, operations::addmod);
test_op3!(mulmod, EvmOp::Mulmod, operations::mulmod);
test_op_dup!(dup1, EvmOp::Dup1, 1);
test_op_dup!(dup2, EvmOp::Dup2, 2);
test_op_dup!(dup3, EvmOp::Dup3, 3);
test_op_dup!(dup4, EvmOp::Dup4, 4);
test_op_dup!(dup5, EvmOp::Dup5, 5);
test_op_dup!(dup6, EvmOp::Dup6, 6);
test_op_dup!(dup7, EvmOp::Dup7, 7);
test_op_dup!(dup8, EvmOp::Dup8, 8);
test_op_dup!(dup9, EvmOp::Dup9, 9);
test_op_dup!(dup10, EvmOp::Dup10, 10);
test_op_dup!(dup11, EvmOp::Dup11, 11);
test_op_dup!(dup12, EvmOp::Dup12, 12);
test_op_dup!(dup13, EvmOp::Dup13, 13);
test_op_dup!(dup14, EvmOp::Dup14, 14);
test_op_dup!(dup15, EvmOp::Dup15, 15);
test_op_dup!(dup16, EvmOp::Dup16, 16);
test_op_swap!(swap1, EvmOp::Swap1, 2);
test_op_swap!(swap2, EvmOp::Swap2, 3);
test_op_swap!(swap3, EvmOp::Swap3, 4);
test_op_swap!(swap4, EvmOp::Swap4, 5);
test_op_swap!(swap5, EvmOp::Swap5, 6);
test_op_swap!(swap6, EvmOp::Swap6, 7);
test_op_swap!(swap7, EvmOp::Swap7, 8);
test_op_swap!(swap8, EvmOp::Swap8, 9);
test_op_swap!(swap9, EvmOp::Swap9, 10);
test_op_swap!(swap10, EvmOp::Swap10, 11);
test_op_swap!(swap11, EvmOp::Swap11, 12);
test_op_swap!(swap12, EvmOp::Swap12, 13);
test_op_swap!(swap13, EvmOp::Swap13, 14);
test_op_swap!(swap14, EvmOp::Swap14, 15);
test_op_swap!(swap15, EvmOp::Swap15, 16);
test_op_swap!(swap16, EvmOp::Swap16, 17);

// TODO: remaining instructions
//Address,
//Balance,
//Origin,
//Caller,
//Callvalue,
//Calldataload,
//Calldatasize,
//CalldataCopy,
//Codesize,
//CodeCopy,
//GasPrice,
//ExtCodeSize,
//ExtCodeCopy,
//ReturnDataSize,
//ReturnDataCopy,
//ExtCodeHash,
//BlockHash,
//ChainId,
//SelfBalance,
//BaseFee,
//Pc,
//Msize,
//Gas,
//Log0,
//Log1,
//Log2,
//Log3,
//Log4,
//Create,
//Call,
//CallCode,
//Return,
//DelegateCall,
//Create2,
//StaticCall,
//Jumpdest,
//Revert,
//Invalid,
//Selfdestruct,
