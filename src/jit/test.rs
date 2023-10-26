use crate::{
    code::EvmOp,
    jit::{
        contract::JitContractBuilder, gas, ExecutionResult, Halt, JitEvmEngineError,
        JitEvmExecutionContext, Success,
    },
    spec::SpecId,
};
use paste::paste;
use primitive_types::U256;
use rand::{Rng, RngCore};
use sha3::{Digest, Keccak256};
use std::{
    collections::{HashMap, HashSet},
    ops::*,
};

mod i256;
mod operations;

mod arithmetic_tests;
mod context_tests;
mod jump_tests;
mod memory_tests;
mod stack_tests;
mod storage_tests;


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

                let op_cost = gas.const_cost($evmop);

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

pub(crate) use expect_halt;
pub(crate) use expect_success;
pub(crate) use expect_stack_underflow;
pub(crate) use expect_stack_overflow;

#[test]
fn operations_jit_test_stop() {
    for _ in 0..1000 {
        let mut execution_context = JitEvmExecutionContext::new();

        let mut expected_mem = vec![0u8; 64];
        let mut ops = Vec::new();

        U256::one().to_big_endian(&mut expected_mem[..32]);
        ops.push(EvmOp::Push(32, U256::one()));
        ops.push(EvmOp::Push(32, U256::zero()));
        ops.push(EvmOp::Mstore);

        U256::from(2).to_big_endian(&mut expected_mem[32..64]);
        ops.push(EvmOp::Push(32, U256::one() * 2));
        ops.push(EvmOp::Push(32, U256::one() * 32));
        ops.push(EvmOp::Mstore);

        ops.push(EvmOp::Stop);
        ops.push(EvmOp::Push(32, U256::one() * 3));
        ops.push(EvmOp::Push(32, U256::one()));
        ops.push(EvmOp::Mstore);
        ops.push(EvmOp::Push(32, U256::one() * 8));
        ops.push(EvmOp::Push(32, U256::zero()));
        ops.push(EvmOp::Mstore);

        let result = test_jit(ops, &mut execution_context).expect("Contract build failed");

		let gas = gas::Gas::new(SpecId::LATEST);
		let push_gas = gas.const_cost(EvmOp::Push(32, U256::zero()));
        let const_cost = gas.const_cost(EvmOp::Mstore);
        let mem_cost = gas.memory_gas();
		let init_cost = gas.init_gas(&[]);

        let offset = 64u64;
        let up = offset.bitand(31).not().wrapping_add(1).bitand(31);
        let size = up.checked_add(offset).expect("Overflow on add");
        let size_words = size / 32;

        let mem_gas = (size_words * size_words) / 512 + mem_cost * size_words;
		let expected_gas = init_cost + push_gas * 4 + mem_gas + const_cost * 2;

        expect_success!(test_stop, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { memory, .. } = execution_context;

		assert_eq!(memory[..64], expected_mem[..64], "Memory was not expected");
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
