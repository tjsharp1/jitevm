use crate::{
    code::EvmOp,
    jit::{JitContractBuilder, JitEvmExecutionContext},
};
use paste::paste;
use primitive_types::{H160, H256, U256};
use rand::{Rng, RngCore};
use sha3::{Digest, Keccak256};
use std::collections::{HashMap, HashSet};

fn test_jit(ops: Vec<EvmOp>, execution_context: &mut JitEvmExecutionContext) {
    use crate::code::EvmCode;
    use inkwell::context::Context;

    let context = Context::create();
    let contract = JitContractBuilder::with_context("jit-instructions", &context)
        .expect("Could not build jit contract")
        .debug_ir("jit_test.ll")
        .debug_asm("jit_test.asm")
        .build(EvmCode { ops: ops.clone() }.index())
        .unwrap();
    // TODO: this return ptr should be a data structure with final state info?
    let ret = contract
        .call(execution_context)
        .expect("Contract call failed");
    assert_eq!(ret, 0);
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

            test_jit(ops, &mut context);

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

#[test]
fn operations_jit_test_block_context_coinbase() {
    check_context!(EvmOp::Coinbase, set_coinbase, h160);
}

#[test]
fn operations_jit_test_block_context_timestamp() {
    check_context!(EvmOp::Timestamp, set_timestamp, u256);
}

#[test]
fn operations_jit_test_block_context_randao() {
    check_context!(EvmOp::PrevRandao, set_prevrandao, h256);
}

#[test]
fn operations_jit_test_block_context_basefee() {
    check_context!(EvmOp::BaseFee, set_basefee, u256);
}

#[test]
fn operations_jit_test_block_context_gas_limit() {
    check_context!(EvmOp::GasLimit, set_gas_limit, u256);
}

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

        test_jit(ops, &mut execution_context);

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

        test_jit(ops, &mut execution_context);

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
        test_jit(ops, &mut execution_context);

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

        test_jit(ops, &mut execution_context);

        let JitEvmExecutionContext { memory, .. } = execution_context;

        let end = expected_values.len() - 1;
        for (index, value) in expected_values.iter().enumerate() {
            let offset = (end - index) * 32;

            let actual = U256::from_big_endian(&memory[offset..offset + 32]);
            assert_eq!(actual, *value);
        }
    }
}

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

        test_jit(ops, &mut execution_context);

        let JitEvmExecutionContext { stack, .. } = execution_context;

        for (stackidx, memidx) in indices.iter().enumerate() {
            let range = *memidx..(memidx + BYTES);
            let expected = U256::from_big_endian(&expected_memory[range]);
            let got = stack[stackidx];
            assert_eq!(expected, got);
        }
    }
}

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

        test_jit(ops, &mut execution_context);

        let JitEvmExecutionContext { memory, .. } = execution_context;

        for memidx in indices {
            let expected = expected_memory[memidx];
            let got = memory[memidx];

            assert_eq!(expected, got);
        }
    }
}

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

        test_jit(ops, &mut execution_context);

        let JitEvmExecutionContext { memory, .. } = execution_context;

        for memidx in indices {
            let range = memidx..memidx + BYTES;
            let expected = &expected_memory[range.clone()];
            let got = &memory[range];

            assert_eq!(expected, got);
        }
    }
}

macro_rules! test_op1 {
    ($fname:ident, $evmop:expr, $opname:expr) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;
                use crate::operations;

                fn _test(a: U256) {
                    let mut ctx = JitEvmExecutionContext::new();

                    test_jit(vec![
                        Push(32, a),
                        $evmop,
                    ], &mut ctx);
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
    };
}

macro_rules! test_op2 {
    ($fname:ident, $evmop:expr, $opname:expr) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;
                use crate::operations;

                fn _test(a: U256, b: U256) {
                    let mut ctx = JitEvmExecutionContext::new();

                    test_jit(vec![
                        Push(32, b),
                        Push(32, a),
                        $evmop,
                    ], &mut ctx);
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
    };
}

macro_rules! test_op2_small {
    ($fname:ident, $evmop:expr, $opname:expr) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;
                use crate::operations;

                fn _test(a: U256, b: U256) {
                    let mut ctx = JitEvmExecutionContext::new();

                    test_jit(vec![
                        Push(32, b),
                        Push(32, a),
                        $evmop,
                    ], &mut ctx);
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
    };
}

//TODO: should check stack pointer is as expected too, this would be nice to have generally for tracing abilities...
macro_rules! test_op_dup {
    ($fname:ident, $evmop:expr, $position:literal) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;

                fn _test(values: Vec<U256>) {
                    let original_stack = values.clone();

                    let mut ops = values
                        .into_iter()
                        .map(|v| Push(32, v))
                        .collect::<Vec<_>>();
                    ops.push($evmop);
                    let mut ctx = JitEvmExecutionContext::new();

                    test_jit(ops.clone(), &mut ctx);
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
    };
}

macro_rules! test_op_swap {
    ($fname:ident, $evmop:expr, $position:literal) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;

                fn _test(values: Vec<U256>) {
                    let original_stack = values.clone();

                    let mut ops = values
                        .into_iter()
                        .map(|v| Push(32, v))
                        .collect::<Vec<_>>();
                    ops.push($evmop);

                    let mut ctx = JitEvmExecutionContext::new();
                    test_jit(ops.clone(), &mut ctx);

                    let JitEvmExecutionContext { stack, .. } = ctx;
                    let d = &stack[..original_stack.len()];
                    let top = original_stack.len() - 1;
                    assert_eq!(original_stack[top], d[top - $position]);
                    assert_eq!(original_stack[top - $position], d[top]);

                    for (index, value) in original_stack.iter().enumerate() {
                        if index == top || index == top - $position {
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
    };
}

macro_rules! test_op3 {
    ($fname:ident, $evmop:expr, $opname:expr) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;
                use crate::operations;

                fn _test(a: U256, b: U256, c: U256) {
                    let mut ctx = JitEvmExecutionContext::new();

                    test_jit(vec![
                        Push(32, c),
                        Push(32, b),
                        Push(32, a),
                        $evmop,
                    ], &mut ctx);
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
    };
}

test_op1!(iszero, EvmOp::Iszero, operations::Iszero);
test_op2!(add, EvmOp::Add, operations::Add);
test_op2!(sub, EvmOp::Sub, operations::Sub);
test_op2!(mul, EvmOp::Mul, operations::Mul);
test_op2!(div, EvmOp::Div, operations::Div);
test_op2!(sdiv, EvmOp::Sdiv, operations::Sdiv);
test_op2!(mod, EvmOp::Mod, operations::Mod);
test_op2!(eq, EvmOp::Eq, operations::Eq);
test_op2!(lt, EvmOp::Lt, operations::Lt);
test_op2!(gt, EvmOp::Gt, operations::Gt);
test_op2!(slt, EvmOp::Slt, operations::Slt);
test_op2!(sgt, EvmOp::Sgt, operations::Sgt);

test_op3!(addmod, EvmOp::Addmod, operations::Addmod);
test_op3!(mulmod, EvmOp::Mulmod, operations::Mulmod);
test_op2!(exp, EvmOp::Exp, operations::Exp);

test_op2!(and, EvmOp::And, operations::And);
test_op2!(or, EvmOp::Or, operations::Or);
test_op2!(xor, EvmOp::Xor, operations::Xor);
test_op2_small!(shl, EvmOp::Shl, operations::Shl);
test_op2_small!(shr, EvmOp::Shr, operations::Shr);
test_op2_small!(sar, EvmOp::Sar, operations::Sar);
test_op2_small!(signextend, EvmOp::Signextend, operations::Signextend);
test_op2!(smod, EvmOp::Smod, operations::Smod);
test_op1!(not, EvmOp::Not, operations::Not);
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
test_op_swap!(swap1, EvmOp::Swap1, 1);
test_op_swap!(swap2, EvmOp::Swap2, 2);
test_op_swap!(swap3, EvmOp::Swap3, 3);
test_op_swap!(swap4, EvmOp::Swap4, 4);
test_op_swap!(swap5, EvmOp::Swap5, 5);
test_op_swap!(swap6, EvmOp::Swap6, 6);
test_op_swap!(swap7, EvmOp::Swap7, 7);
test_op_swap!(swap8, EvmOp::Swap8, 8);
test_op_swap!(swap9, EvmOp::Swap9, 9);
test_op_swap!(swap10, EvmOp::Swap10, 10);
test_op_swap!(swap11, EvmOp::Swap11, 11);
test_op_swap!(swap12, EvmOp::Swap12, 12);
test_op_swap!(swap13, EvmOp::Swap13, 13);
test_op_swap!(swap14, EvmOp::Swap14, 14);
test_op_swap!(swap15, EvmOp::Swap15, 15);
test_op_swap!(swap16, EvmOp::Swap16, 16);
//Byte,
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
//Pop,
//Jump,
//Jumpi,
//Pc,
//Msize,
//Gas,
//Jumpdest,
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
//Revert,
//Invalid,
//Selfdestruct,

//AugmentedPushJump(usize, U256),
//AugmentedPushJumpi(usize, U256),
