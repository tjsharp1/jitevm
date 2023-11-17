use super::Tracer;
use crate::jit::{
    context::JitContractExecutionResult,
    contract::{BuilderContext, JitEvmEngineSimpleBlock},
    cursor::CurrentInstruction,
    error::JitEvmEngineError,
};
use inkwell::{values::GlobalValue, AddressSpace};

pub(crate) struct CycleBreakpoint<'ctx> {
    breakpoint: GlobalValue<'ctx>,
    cycles: GlobalValue<'ctx>,
}

impl<'ctx> CycleBreakpoint<'ctx> {
    pub fn initialize(ctx: &BuilderContext<'ctx>, cycles: u64) -> CycleBreakpoint<'ctx> {
        let addr = AddressSpace::default();

        // TODO: add to executecontext args so this doesn't have to re-JIT for different cycle
        // counts.
        let const_break = ctx.types.type_i64.const_int(cycles, false);
        let breakpoint = ctx
            .module
            .add_global(ctx.types.type_i64, Some(addr), "breakpoint");
        breakpoint.set_thread_local(true);
        breakpoint.set_initializer(&const_break);

        let const_0 = ctx.types.type_i64.const_int(0, false);
        let cycles = ctx
            .module
            .add_global(ctx.types.type_i64, Some(addr), "cycles");
        cycles.set_thread_local(true);
        cycles.set_initializer(&const_0);

        CycleBreakpoint { breakpoint, cycles }
    }
}

impl<'ctx> Tracer<'ctx> for CycleBreakpoint<'ctx> {
    fn step(
        &self,
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'_, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let breakpoint_addr = self.breakpoint.as_pointer_value();
        let cycles_addr = self.cycles.as_pointer_value();

        let breakpoint = ctx
            .builder
            .build_load(ctx.types.type_i64, breakpoint_addr, "")?
            .into_int_value();
        let cycles = ctx
            .builder
            .build_load(ctx.types.type_i64, cycles_addr, "")?
            .into_int_value();

        let const_1 = ctx.types.type_i64.const_int(1, false);
        let cycles = ctx.builder.build_int_add(cycles, const_1, "")?;
        ctx.builder.build_store(cycles_addr, cycles)?;

        let idx = current.idx();
        let end = JitEvmEngineSimpleBlock::new(
            ctx,
            current.block().block,
            &format!("cyle-breakpoint: {}", idx),
            &format!("break_{}", idx),
        )?;
        let next = JitEvmEngineSimpleBlock::new(
            ctx,
            current.block().block,
            &format!("cycle-continue: {}", idx),
            &format!("continue_{}", idx),
        )?;

        next.add_incoming(current.book_ref(), current.block());
        end.add_incoming(current.book_ref(), current.block());

        ctx.builder.position_at_end(current.block().block);

        let cmp =
            ctx.builder
                .build_int_compare(inkwell::IntPredicate::UGE, cycles, breakpoint, "")?;
        ctx.builder
            .build_conditional_branch(cmp, end.block, next.block)?;

        ctx.builder.position_at_end(end.block);
        JitContractExecutionResult::build_exit_stop(ctx, &end)?;

        ctx.builder.position_at_end(next.block);
        current.update_current_block(next);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        code::EvmOp,
        jit::{
            contract::JitContractBuilder, gas, ExecutionResult, JitEvmExecutionContext, Success,
        },
    };
    use alloy_primitives::U256;
    use revm::db::InMemoryDB;
    use revm_primitives::{LatestSpec, Spec};

    fn test_jit<SPEC: Spec>(
        spec: SPEC,
        ops: Vec<EvmOp>,
        ctx: &mut JitEvmExecutionContext<SPEC>,
        cycles: u64,
    ) -> Result<ExecutionResult, JitEvmEngineError> {
        use crate::code::EvmCode;
        use inkwell::context::Context;

        let context = Context::create();
        let contract = JitContractBuilder::with_context("jit-instructions", &context)
            .expect("Could not build jit contract")
            .cycle_breakpoint(cycles)
            //.debug_ir("jit_test.ll")
            //.debug_asm("jit_test.asm")
            .build(spec, EvmCode { ops: ops.clone() }.augment().index())?;
        Ok(contract.transact(ctx).expect("Contract call failed"))
    }

    #[test]
    fn loop_break() {
        use EvmOp::*;
        let breakpoints = vec![
            24, 47, 48, 49, 91, 101, 190, 191, 192, 193, 211, 1000, 1001, 3000,
        ];

        fn stack_check(cycles: u64, stack: Vec<U256>) {
            let cycles = cycles - 3;
            let (q, r) = (cycles / 4, cycles % 4);

            if r <= 1 {
                assert_eq!(stack[0], U256::from(q));
            } else {
                assert_eq!(stack[0], U256::from(q + 1));
            }
        }

        let ops = vec![
            Push(1, U256::ZERO),
            Push(1, U256::from(1)),
            Jumpdest,               // 0 + k*4
            Add,                    // 1 + k*4
            Push(1, U256::from(1)), // 2 + k*4
            Push(1, U256::from(4)), // 3 + k*4
            Jump,
        ];

        for cycles in &breakpoints {
            let db = InMemoryDB::default();
            let mut ctx = JitEvmExecutionContext::builder(LatestSpec).build_with_db(&db);

            let result =
                test_jit(LatestSpec, ops.clone(), &mut ctx, *cycles).expect("Failed JIT test");
            match result {
                ExecutionResult::Success { reason, .. } => {
                    assert_eq!(reason, Success::Stop, "Exit reason should be Stop!");
                }
                _ => panic!("Exit should be success!"),
            }
            let JitEvmExecutionContext { stack, .. } = ctx;
            stack_check(*cycles, stack);
        }
    }
}
