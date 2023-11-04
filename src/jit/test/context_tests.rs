use super::{expect_halt, expect_stack_overflow, expect_success, memory_gas_calc, test_jit};
use crate::jit::{
    gas, EvmOp, ExecutionResult, Halt, JitEvmExecutionContext, Success, EVM_STACK_SIZE,
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
