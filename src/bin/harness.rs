use jitevm::{
    code::{EvmCode, EvmOp},
    jit::{JitContractBuilder, JitEvmExecutionContext},
};
use primitive_types::U256;
use rand::Rng;

fn test_jit_storage(ops: Vec<EvmOp>, execution_context: &mut JitEvmExecutionContext) {
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

fn main() {
    use jitevm::operations;

    fn _test(a: U256, b: U256) {
        let mut ctx = JitEvmExecutionContext::new();

        test_jit_storage(
            vec![EvmOp::Push(32, b), EvmOp::Push(32, a), EvmOp::Exp],
            &mut ctx,
        );

        let JitEvmExecutionContext { stack, .. } = ctx;

        let d = stack[0];
        let d_ = operations::Exp(a, b);
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
