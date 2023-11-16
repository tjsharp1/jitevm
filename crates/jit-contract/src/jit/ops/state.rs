use crate::jit::{
    contract::BuilderContext,
    cursor::CurrentInstruction,
    gas::{build_memory_gas_check, const_cost, memory_expansion_cost, memory_gas},
    ops::{build_stack_check, build_stack_pop},
    JitEvmEngineError,
};
use inkwell::AddressSpace;
use revm_primitives::Spec;

pub(crate) fn build_revert_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 2, 0);

    let offset = build_stack_pop!(ctx, current);
    let size = build_stack_pop!(ctx, current);

    build_memory_gas_check!(ctx, current, offset, size);

    let block = current.block();

    JitContractExecutionResult::build_exit_revert(ctx, block, offset, size)?;

    Ok(())
}

pub(crate) fn build_return_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 2, 0);

    let offset = build_stack_pop!(ctx, current);
    let size = build_stack_pop!(ctx, current);

    build_memory_gas_check!(ctx, current, offset, size);

    let block = current.block();

    JitContractExecutionResult::build_exit_return(ctx, block, offset, size)?;

    Ok(())
}
