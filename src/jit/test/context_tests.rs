use crate::spec::SpecId;
use crate::jit::{
    ExecutionResult,
    EvmOp,
	EVM_STACK_SIZE,
	Halt,
	JitEvmExecutionContext,
	gas,
	Success,
};
use paste::paste;
use primitive_types::{H160, H256, U256};
use rand::Rng;
use std::ops::{BitAnd, Not};
use super::{expect_halt, expect_success, expect_stack_overflow, test_jit};


macro_rules! check_context {
    ($evmop:expr, $setter:ident, $ty:ident) => {{
        for _ in 0..1000 {
            let mut context = JitEvmExecutionContext::new();

            let mut expected_mem = Vec::new();
            let mut ops = Vec::new();

            check_context!($setter, expected_mem, context, $ty);

            ops.push($evmop);
            ops.push(EvmOp::Push(32, U256::zero()));
            ops.push(EvmOp::Mstore);

            let result = test_jit(ops, &mut context).expect("Contract build failed");

            let gas = gas::Gas::new(SpecId::LATEST);
			let op_cost = gas.const_cost($evmop);
			let push_cost = gas.const_cost(EvmOp::Push(1, U256::zero()));

			let const_cost = gas.const_cost(EvmOp::Mstore);
			let mem_cost = gas.memory_gas();
			let init_cost = gas.init_gas(&[]);

			let offset = 32u64;
			let up = offset.bitand(31).not().wrapping_add(1).bitand(31);
			let size = up.checked_add(offset).expect("Overflow on add");
			let size_words = size / 32;

			let mem_gas = (size_words * size_words) / 512 + mem_cost * size_words;
			let expected_gas = init_cost + push_cost + const_cost + mem_gas + op_cost;

            expect_success!($setter, result, Success::Stop, expected_gas);

            let JitEvmExecutionContext {
                memory, ..
            } = context;

			let mem_range = 0..0x20;
			assert_eq!(memory[mem_range.clone()], expected_mem[mem_range.clone()]);
        }
    }};
    ($setter:ident, $mem:ident, $ctx:ident, u256) => {{
        let val = rand::thread_rng().gen::<[u8; 32]>();
        $mem.extend(val.to_vec());
        let val = U256::from(val);
        $ctx.block_context.$setter(val);
    }};
    ($setter:ident, $mem:ident, $ctx:ident, h160) => {{
        let mut val = rand::thread_rng().gen::<[u8; 32]>();
        val[..12].copy_from_slice(&[0u8; 12]);
        $mem.extend(val.to_vec());
        $ctx.block_context.$setter(H160::from_slice(&val[12..]));
    }};
    ($setter:ident, $mem:ident, $ctx:ident, h256) => {{
        let val = rand::thread_rng().gen::<[u8; 32]>();
        $mem.extend(val.to_vec());
        $ctx.block_context.$setter(H256::from_slice(&val));
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
