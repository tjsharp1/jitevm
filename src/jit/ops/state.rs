use crate::jit::{
    context::{JitContractExecutionResult, JitContractResultCode},
    contract::BuilderContext,
    cursor::CurrentInstruction,
    gas::{build_memory_gas_check, const_cost, memory_expansion_cost, memory_gas},
    ops::{build_stack_check, build_stack_pop},
    JitEvmEngineError,
};
use inkwell::AddressSpace;
use revm_primitives::Spec;

pub(crate) fn build_revert_op<'a, 'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 2, 0);

    let book = current.book();
    let (book, offset) = build_stack_pop!(ctx, book);
    let (book, size) = build_stack_pop!(ctx, book);

    build_memory_gas_check!(ctx, current, book, offset, size);

    let block = current.block();

    JitContractExecutionResult::build_exit_revert(ctx, block, offset, size)?;

    Ok(())
}

pub(crate) fn build_return_op<'a, 'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 2, 0);

    let book = current.book();
    let (book, offset) = build_stack_pop!(ctx, book);
    let (book, size) = build_stack_pop!(ctx, book);

    build_memory_gas_check!(ctx, current, book, offset, size);

    let block = current.block();

    JitContractExecutionResult::build_exit_return(ctx, block, offset, size)?;

    Ok(())
}
