use super::{expect_halt, expect_stack_overflow, expect_success, memory_gas_calc, test_jit};
use crate::jit::{
    gas, EvmOp, ExecutionResult, Halt, JitEvmExecutionContext, Success, TransactionConfig,
    EVM_STACK_SIZE,
};
use alloy_primitives::{Address, B256, U256};
use paste::paste;
use rand::Rng;
use revm::InMemoryDB;
use revm_primitives::LatestSpec;

macro_rules! check_context {
    ($evmop:expr, $setter:ident, $ty:ident) => {{
        for _ in 0..1000 {
            let db = InMemoryDB::default();
            let mut context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);

            let mut expected_mem = Vec::new();
            let mut ops = Vec::new();

            check_context!($setter, expected_mem, context, $ty);

            ops.push($evmop);
            ops.push(EvmOp::Push(32, U256::ZERO));
            ops.push(EvmOp::Mstore);

            let result = test_jit(LatestSpec, ops, &mut context).expect("Contract build failed");

            let op_cost = gas::const_cost::<LatestSpec>($evmop);
            let push_cost = gas::const_cost::<LatestSpec>(EvmOp::Push(1, U256::ZERO));

            let const_cost = gas::const_cost::<LatestSpec>(EvmOp::Mstore);
            let init_cost = gas::init_gas::<LatestSpec>(&[]);

            let mem_gas = memory_gas_calc::<LatestSpec>(32);
            let expected_gas = init_cost + push_cost + const_cost + mem_gas + op_cost;

            expect_success!($setter, result, Success::Stop, expected_gas);

            let JitEvmExecutionContext { memory, .. } = context;

            let mem_range = 0..0x20;
            assert_eq!(memory[mem_range.clone()], expected_mem[mem_range.clone()]);
        }
    }};
    ($setter:ident, $mem:ident, $ctx:ident, address) => {{
        let val = rand::thread_rng().gen::<[u8; 20]>();
        let addr = Address::from_slice(&val);
        let word = addr.into_word();

        $mem.extend(word.to_vec());
        $ctx.$setter(addr);
    }};
    ($setter:ident, $mem:ident, $ctx:ident, u256) => {{
        let val = rand::thread_rng().gen::<[u8; 32]>();
        $mem.extend(val.to_vec());
        let val = U256::from_be_bytes(val);
        $ctx.$setter(val);
    }};
    ($setter:ident, $mem:ident, $ctx:ident, h160) => {{
        let mut val = rand::thread_rng().gen::<[u8; 32]>();
        val[..12].copy_from_slice(&[0u8; 12]);
        $mem.extend(val.to_vec());
        $ctx.$setter(Address::from_slice(&val[12..]));
    }};
    ($setter:ident, $mem:ident, $ctx:ident, h256) => {{
        let val = rand::thread_rng().gen::<[u8; 32]>();
        $mem.extend(val.to_vec());
        $ctx.$setter(B256::from_slice(&val));
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
    check_context!(EvmOp::GasLimit, set_block_gas_limit, u256);
}
expect_stack_overflow!(gas_limit, EvmOp::GasLimit, 1);

#[test]
fn operations_jit_test_transaction_context_callvalue() {
    check_context!(EvmOp::Callvalue, set_callvalue, u256);
}
expect_stack_overflow!(callvalue, EvmOp::Callvalue, 1);

#[test]
fn operations_jit_test_transaction_context_caller() {
    check_context!(EvmOp::Caller, set_caller, address);
}
expect_stack_overflow!(caller, EvmOp::Caller, 1);

#[test]
fn operations_jit_test_get_origin() {
    for _ in 0..200 {
        let mut ops = Vec::new();

        let db = InMemoryDB::default();
        let mut tx_config = TransactionConfig::default();

        let origin = Address::random();
        let caller = Address::random();

        tx_config.caller = origin;

        let mut context = JitEvmExecutionContext::builder(LatestSpec)
            .with_transaction_config(tx_config)
            .build_with_db(&db);

        context.set_caller(caller);

        ops.push(EvmOp::Origin);
        ops.push(EvmOp::Push(32, U256::ZERO));
        ops.push(EvmOp::Mstore);
        ops.push(EvmOp::Caller);
        ops.push(EvmOp::Push(32, U256::from(0x20)));
        ops.push(EvmOp::Mstore);

        let mut expected_mem = origin.into_word().as_slice().to_vec();
        expected_mem.extend(caller.into_word().as_slice().to_vec());

        let result = test_jit(LatestSpec, ops, &mut context).expect("Contract build failed");

        let origin_cost = gas::const_cost::<LatestSpec>(EvmOp::Origin);
        let caller_cost = gas::const_cost::<LatestSpec>(EvmOp::Caller);
        let push_cost = gas::const_cost::<LatestSpec>(EvmOp::Push(32, U256::ZERO));

        let const_cost = gas::const_cost::<LatestSpec>(EvmOp::Mstore);
        let init_cost = gas::init_gas::<LatestSpec>(&[]);

        let mem_gas = memory_gas_calc::<LatestSpec>(64);
        let expected_gas =
            init_cost + (push_cost + const_cost) * 2 + mem_gas + caller_cost + origin_cost;

        expect_success!(test_origin, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { memory, .. } = context;

        let mem_range = 0..0x20;
        assert_eq!(memory[mem_range.clone()], expected_mem[mem_range.clone()]);

        let mem_range = 0x20..0x40;
        assert_eq!(memory[mem_range.clone()], expected_mem[mem_range.clone()]);
    }
}
expect_stack_overflow!(origin, EvmOp::Origin, 1);

#[test]
fn operations_jit_test_codesize() {
    let mut ops = Vec::new();

    for i in 0..200 {
        let db = InMemoryDB::default();
        let mut context = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);

        let mut cloned = ops.clone();

        cloned.push(EvmOp::Codesize);
        cloned.push(EvmOp::Push(32, U256::ZERO));
        cloned.push(EvmOp::Mstore);

        let size = cloned.iter().fold(0, |accum, op| accum + op.len());
        let size = U256::from(size);

        let expected_mem = size.to_be_bytes_vec();

        let result = test_jit(LatestSpec, cloned, &mut context).expect("Contract build failed");

        let op_cost = gas::const_cost::<LatestSpec>(EvmOp::Codesize);
        let push_cost = gas::const_cost::<LatestSpec>(EvmOp::Push(32, U256::ZERO));

        let const_cost = gas::const_cost::<LatestSpec>(EvmOp::Mstore);
        let init_cost = gas::init_gas::<LatestSpec>(&[]);

        let mem_gas = memory_gas_calc::<LatestSpec>(32);
        let expected_gas = init_cost + push_cost * (i + 1) + const_cost + mem_gas + op_cost;

        expect_success!(test_codesize, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { memory, .. } = context;

        let mem_range = 0..0x20;
        assert_eq!(memory[mem_range.clone()], expected_mem[mem_range.clone()]);

        let push_size = (i as usize % 32) + 1;
        ops.push(EvmOp::Push(push_size, U256::ZERO));
    }
}
expect_stack_overflow!(codesize, EvmOp::Codesize, 1);
