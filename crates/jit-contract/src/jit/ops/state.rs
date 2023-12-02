use crate::jit::{
    contract::BuilderContext,
    cursor::Current,
    gas::{build_memory_gas_check, memory_expansion_cost, memory_gas},
    ops::build_stack_pop,
    JitEvmEngineError,
};
use inkwell::AddressSpace;
use revm_primitives::Spec;

pub(crate) fn build_revert_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut Current<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let offset = build_stack_pop!(ctx, current);
    let size = build_stack_pop!(ctx, current);

    build_memory_gas_check!(ctx, current, offset, size);

    let book = current.book_ref();

    JitContractExecutionResult::build_exit_revert(ctx, book, offset, size)?;

    Ok(())
}

pub(crate) fn build_return_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut Current<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let offset = build_stack_pop!(ctx, current);
    let size = build_stack_pop!(ctx, current);

    build_memory_gas_check!(ctx, current, offset, size);

    let book = current.book_ref();

    JitContractExecutionResult::build_exit_return(ctx, book, offset, size)?;

    Ok(())
}
