use super::{expect_halt, expect_stack_underflow, expect_success, operations, test_jit};
use crate::jit::{gas, EvmOp, ExecutionResult, Halt, JitEvmExecutionContext, Success};
use alloy_primitives::U256;
use paste::paste;
use rand::Rng;
use revm::db::InMemoryDB;
use revm_primitives::LatestSpec;

macro_rules! test_op1 {
    ($fname:ident, $evmop:expr, $opname:expr) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;

                fn _test(a: U256) {
                    let db = InMemoryDB::default();
                    let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);

                    let push_cost = gas::const_cost::<LatestSpec>(Push(32, U256::ZERO));
                    let op_cost = gas::const_cost::<LatestSpec>($evmop);
                    let init_cost = gas::init_gas::<LatestSpec>(&[]);
                    let expected_gas = init_cost + push_cost + op_cost;

                    let result = test_jit(LatestSpec, vec![
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

                _test(U256::ZERO);
                _test(U256::from(1));

                for _i in 0..1000 {
                    let a = rand::thread_rng().gen::<[u8; 32]>();
                    let a = U256::from_be_bytes(a);
                    _test(a);
                }
            }
        }
        expect_stack_underflow!($fname, $evmop, 1);
    };
}

macro_rules! test_op2 {
    ($fname:ident, $evmop:expr, $opname:expr) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;

                fn _test(a: U256, b: U256) {
                    let db = InMemoryDB::default();
                    let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);

                    let push_cost = gas::const_cost::<LatestSpec>(Push(32, U256::ZERO));
                    let op_cost = gas::const_cost::<LatestSpec>($evmop);
                    let init_cost = gas::init_gas::<LatestSpec>(&[]);
                    let expected_gas = init_cost + push_cost * 2 + op_cost;

                    let result = test_jit(LatestSpec, vec![
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

                _test(U256::ZERO, U256::ZERO);
                _test(U256::ZERO, U256::from(1));
                _test(U256::from(1), U256::ZERO);
                _test(U256::from(1), U256::from(1));

                for _i in 0..1000 {
                    let a = rand::thread_rng().gen::<[u8; 32]>();
                    let b = rand::thread_rng().gen::<[u8; 32]>();
                    let a = U256::from_be_bytes(a);
                    let b = U256::from_be_bytes(b);
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

                fn _test(a: U256, b: U256) {
                    let db = InMemoryDB::default();
                    let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);

                    let push_cost = gas::const_cost::<LatestSpec>(Push(32, U256::ZERO));
                    let op_cost = gas::const_cost::<LatestSpec>($evmop);
                    let init_cost = gas::init_gas::<LatestSpec>(&[]);
                    let expected_gas = init_cost + push_cost * 2 + op_cost;

                    let result = test_jit(LatestSpec, vec![
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

                _test(U256::ZERO, U256::ZERO);
                _test(U256::ZERO, U256::from(1));
                _test(U256::from(1), U256::ZERO);
                _test(U256::from(1), U256::from(1));

                for _i in 0..1000 {
                    let t = rand::thread_rng().gen::<u8>();
                    let b = rand::thread_rng().gen::<[u8; 32]>();

                    let mut a: [u8; 32] = [0u8; 32];
                    a[31] = t;
                    let a = U256::from_be_bytes(a);
                    let b = U256::from_be_bytes(b);
                    _test(a, b);
                }
            }
        }
        expect_stack_underflow!($fname, $evmop, 2);
    };
}

macro_rules! test_op3 {
    ($fname:ident, $evmop:expr, $opname:expr) => {
        paste! {
            #[test]
            fn [<operations_jit_equivalence_ $fname>]() {
                use crate::code::EvmOp::*;

                fn _test(a: U256, b: U256, c: U256) {
                    let db = InMemoryDB::default();
                    let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);

                    let push_cost = gas::const_cost::<LatestSpec>(Push(32, U256::ZERO));
                    let op_cost = gas::const_cost::<LatestSpec>($evmop);
                    let init_cost = gas::init_gas::<LatestSpec>(&[]);
                    let expected_gas = init_cost + push_cost * 3 + op_cost;

                    let result = test_jit(LatestSpec, vec![
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

                _test(U256::ZERO, U256::ZERO, U256::ZERO);
                _test(U256::ZERO, U256::ZERO, U256::from(1));
                _test(U256::ZERO, U256::from(1), U256::ZERO);
                _test(U256::ZERO, U256::from(1), U256::from(1));
                _test(U256::from(1), U256::ZERO, U256::ZERO);
                _test(U256::from(1), U256::ZERO, U256::from(1));
                _test(U256::from(1), U256::from(1), U256::ZERO);
                _test(U256::from(1), U256::from(1), U256::from(1));

                for _i in 0..1000 {
                    let a = rand::thread_rng().gen::<[u8; 32]>();
                    let b = rand::thread_rng().gen::<[u8; 32]>();
                    let c = rand::thread_rng().gen::<[u8; 32]>();
                    let a = U256::from_be_bytes(a);
                    let b = U256::from_be_bytes(b);
                    let c = U256::from_be_bytes(c);

                    _test(a, b, c);
                }
            }
        }
        expect_stack_underflow!($fname, $evmop, 3);
    };
}

#[test]
fn operations_underflow_exp() {
    use crate::code::EvmOp::*;

    let push_gas = gas::const_cost::<LatestSpec>(Push(32, U256::ZERO));
    let init_cost = gas::init_gas::<LatestSpec>(&[]);
    let (base, exp) = gas::exp_cost::<LatestSpec>();

    let mut ops = Vec::new();

    for i in 0..2 {
        let mut cloned = ops.clone();
        cloned.push(EvmOp::Exp);

        let db = InMemoryDB::default();
        let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
        let result = test_jit(LatestSpec, cloned, &mut ctx).expect("Contract build failed");

        expect_halt!(operations_underflow_exp, result, Halt::StackUnderflow);

        ops.push(Push(32, U256::from(i + 1)));
    }
    ops.push(EvmOp::Exp);

    let expected_gas = init_cost + push_gas * 2 + base + exp;

    let db = InMemoryDB::default();
    let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);
    let result = test_jit(LatestSpec, ops, &mut ctx).expect("Contract build failed");

    expect_success!(
        operations_underflow_exp,
        result,
        Success::Stop,
        expected_gas
    );
}

#[test]
fn operations_jit_equivalence_exp() {
    use crate::code::EvmOp::*;

    fn _test(a: U256, b: U256) {
        let db = InMemoryDB::default();
        let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);

        let push_cost = gas::const_cost::<LatestSpec>(Push(32, U256::ZERO));
        let (base, exp) = gas::exp_cost::<LatestSpec>();
        let init_cost = gas::init_gas::<LatestSpec>(&[]);

        let zeros = (256 - b.leading_zeros()) as u64;
        let bytes = (zeros / 8) + if zeros % 8 == 0 { 0 } else { 1 };
        let expected_gas = init_cost + push_cost * 2 + base + exp * bytes;

        let result = test_jit(
            LatestSpec,
            vec![Push(32, b), Push(32, a), EvmOp::Exp],
            &mut ctx,
        )
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

    _test(U256::ZERO, U256::ZERO);
    _test(U256::ZERO, U256::from(1));
    _test(U256::from(1), U256::ZERO);
    _test(U256::from(1), U256::from(1));

    for _i in 0..1000 {
        let a = rand::thread_rng().gen::<[u8; 32]>();
        let b = rand::thread_rng().gen::<[u8; 32]>();
        let a = U256::from_be_bytes(a);
        let b = U256::from_be_bytes(b);
        _test(a, b);
    }
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
