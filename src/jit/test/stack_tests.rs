use super::{expect_halt, expect_stack_overflow, expect_stack_underflow, expect_success, test_jit};
use crate::jit::{
    gas, EvmOp, ExecutionResult, Halt, JitEvmExecutionContext, Success, EVM_STACK_SIZE,
};
use crate::spec::SpecId;
use alloy_primitives::U256;
use paste::paste;
use rand::Rng;
use revm::db::InMemoryDB;

macro_rules! test_op_dup {
    ($fname:ident, $evmop:expr, $position:literal) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;

                fn _test(values: Vec<U256>) {
                    let original_stack = values.clone();

                    let gas = gas::Gas::new(SpecId::LATEST);
                    let push_cost = gas.const_cost(Push(32, U256::ZERO));
                    let op_cost = gas.const_cost($evmop);
                    let init_cost = gas.init_gas(&[]);
                    let expected_gas = init_cost + push_cost * values.len() as u64 + op_cost;

                    let mut ops = values
                        .into_iter()
                        .map(|v| Push(32, v))
                        .collect::<Vec<_>>();
                    ops.push($evmop);
                    let db = InMemoryDB::default();
                    let mut ctx = JitEvmExecutionContext::new_with_db(&db);

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
                        U256::from_be_bytes(a)
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
                    let push_cost = gas.const_cost(Push(32, U256::ZERO));
                    let op_cost = gas.const_cost($evmop);
                    let init_cost = gas.init_gas(&[]);
                    let expected_gas = init_cost + push_cost * values.len() as u64 + op_cost;

                    let mut ops = values
                        .into_iter()
                        .map(|v| Push(32, v))
                        .collect::<Vec<_>>();
                    ops.push($evmop);

                    let db = InMemoryDB::default();
                    let mut ctx = JitEvmExecutionContext::new_with_db(&db);
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
                        U256::from_be_bytes(a)
                    }).collect();

                    _test(data);
                }
            }
        }
        expect_stack_underflow!($fname, $evmop, $min_stack);
    };
}

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
