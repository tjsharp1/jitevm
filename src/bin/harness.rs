use jitevm::{
    code::{EvmCode, EvmOp},
    jit::{contract::JitContractBuilder, ExecutionResult, JitEvmExecutionContext, Success},
};
use primitive_types::{H160, U256};
use rand::Rng;
use std::collections::{HashMap, HashSet};

fn test_jit(ops: Vec<EvmOp>, execution_context: &mut JitEvmExecutionContext) {
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

    match ret {
        ExecutionResult::Success { reason } => {
            assert_eq!(reason, Success::Stop);
        }
        o => panic!("Unexpected result {:?}", o),
    }
}

fn main() {
    let mut execution_context = JitEvmExecutionContext::new();

    let mut expected_store = HashMap::new();
    let mut ops = Vec::new();

    let mut coinbase = rand::thread_rng().gen::<[u8; 32]>();
    coinbase[..12].copy_from_slice(&[0u8; 12]);
    execution_context
        .block_context
        .set_coinbase(H160::from_slice(&coinbase[12..]));
    expected_store.insert(U256::one(), U256::from_little_endian(&coinbase));

    ops.push(EvmOp::Coinbase);
    ops.push(EvmOp::Push(32, U256::one()));
    ops.push(EvmOp::Sstore);

    test_jit(ops, &mut execution_context);

    let JitEvmExecutionContext { storage, .. } = execution_context;

    let expected_keys: HashSet<U256> = expected_store.keys().cloned().collect();
    let actual_keys: HashSet<U256> = storage.keys().cloned().collect();

    let diff = expected_keys.symmetric_difference(&actual_keys).count();
    assert_eq!(diff, 0);

    for (key, value) in expected_store.iter() {
        let stored = *storage.get(key).expect("Storage should have item");
        println!("e: {:x?}", value);
        println!("g: {:x?}", stored);
        assert_eq!(*value, stored);
    }
}
