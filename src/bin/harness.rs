use jitevm::{
    code::{EvmCode, EvmOp},
    jit::{
        contract::JitContractBuilder, gas, ExecutionResult, JitEvmEngineError,
        JitEvmExecutionContext, Success,
    },
    spec::SpecId,
};
use primitive_types::U256;
use rand::Rng;

macro_rules! expect_success {
    ($fname:ident, $result:ident, $reason:expr, $gas:ident) => {
        let name_str = stringify!($fname);

        match $result {
            ExecutionResult::Success { reason, gas_used } => {
                assert_eq!(
                    reason, $reason,
                    "{}: expected {:?}, got {:?}",
                    name_str, $reason, reason
                );
                assert_eq!(gas_used, $gas, "{}: incorrect gas usage.", name_str);
            }
            o => panic!("{}: Expected success, got: {:?}", name_str, o),
        }
    };
}

fn test_jit(
    ops: Vec<EvmOp>,
    execution_context: &mut JitEvmExecutionContext,
) -> Result<ExecutionResult, JitEvmEngineError> {
    use inkwell::context::Context;

    let context = Context::create();
    let contract = JitContractBuilder::with_context("jit-instructions", &context)
        .expect("Could not build jit contract")
        .debug_ir("jit_test.ll")
        .debug_asm("jit_test.asm")
        .build(EvmCode { ops: ops.clone() }.index())
        .unwrap();
    Ok(contract
        .call(execution_context)
        .expect("Contract call failed"))
}

fn main() {
    use jitevm::code::EvmOp::*;

    fn test(a: U256) {
        let mut ctx = JitEvmExecutionContext::new();

        let gas = gas::Gas::new(SpecId::LATEST);
        let push_cost = gas.const_cost(Push(32, U256::zero()));
        let op_cost = gas.const_cost(Iszero);
        let expected_gas = push_cost + op_cost;

        let result = test_jit(vec![Push(32, a), Iszero], &mut ctx).expect("JIT test failed");

        expect_success!(zero, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { stack, .. } = ctx;
        let d = stack[0];
        println!("{:?}", d);
    }

    test(U256::zero());
    test(U256::one());

    for _i in 0..1000 {
        let a = rand::thread_rng().gen::<[u8; 32]>();
        let a = U256::from_big_endian(&a);
        test(a);
    }
}
