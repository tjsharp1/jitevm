use crate::jit::ops::{
    build_stack_check, build_stack_pop, build_stack_pop_vector, build_stack_push_vector,
};
use crate::jit::{
    context::JitEvmPtrs,
    contract::BuilderContext,
    cursor::CurrentInstruction,
    gas::{build_memory_gas_check, const_cost, memory_expansion_cost, memory_gas},
    JitEvmEngineError, EVM_JIT_STACK_ALIGN,
};
use inkwell::{values::BasicValue, AddressSpace};
use revm_primitives::Spec;

macro_rules! build_memory_limit_check {
    ($ctx:ident, $current:ident, $end_offset:ident) => {{
        let book = $current.book_ref();

        let limit = JitEvmPtrs::build_get_memory_limit($ctx, book.execution_context)?;
        let cmp = $ctx
            .builder
            .build_int_compare(IntPredicate::ULE, $end_offset, limit, "")?;

        let instruction_label = format!("i{}_enough_mem", $current.idx());
        let idx = format!("_{}", $current.idx());
        let next_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        let instruction_label = format!("i{}_memory_limit", $current.idx());
        let error_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        next_block.add_incoming($current.book_ref(), $current.block());
        error_block.add_incoming($current.book_ref(), $current.block());

        $ctx.builder.position_at_end(error_block.block);
        JitContractExecutionResult::build_exit_halt(
            $ctx,
            &error_block,
            JitContractResultCode::OutOfGasMemoryLimit,
        )?;

        $ctx.builder.position_at_end($current.block().block);
        $ctx.builder
            .build_conditional_branch(cmp, next_block.block, error_block.block)?;

        $ctx.builder.position_at_end(next_block.block);
        $current.update_current_block(next_block);
    }};
}

pub(crate) fn build_mstore_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 2, 0);

    let offset = build_stack_pop!(ctx, current);
    build_memory_gas_check!(ctx, current, offset, 32);

    let (vec0, vec1) = build_stack_pop_vector!(ctx, current);

    let shuffled = ctx
        .builder
        .build_shuffle_vector(vec0, vec1, ctx.types.swap_bytes, "")?;

    let offset = ctx
        .builder
        .build_int_cast(offset, ctx.types.type_ptrint, "")?;

    let const_32 = ctx.types.type_ptrint.const_int(32, false);
    let end_offset = ctx.builder.build_int_add(offset, const_32, "")?;
    build_memory_limit_check!(ctx, current, end_offset);

    let book = current.book_ref();
    let mem = ctx.builder.build_int_add(book.mem, offset, "")?;
    let dest_ptr = ctx.builder.build_int_to_ptr(
        mem,
        ctx.types.type_stackel.ptr_type(AddressSpace::default()),
        "",
    )?;

    ctx.builder
        .build_store(dest_ptr, shuffled)?
        .set_alignment(1)?;

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current
        .next()
        .add_incoming(current.book_ref(), current.block());
    Ok(())
}

pub(crate) fn build_mstore8_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 2, 0);

    let offset = build_stack_pop!(ctx, current);
    let value = build_stack_pop!(ctx, current);

    build_memory_gas_check!(ctx, current, offset, 1);

    let value = ctx.builder.build_int_cast(value, ctx.types.type_i8, "")?;
    let offset = ctx
        .builder
        .build_int_cast(offset, ctx.types.type_ptrint, "")?;

    let const_1 = ctx.types.type_ptrint.const_int(1, false);
    let end_offset = ctx.builder.build_int_add(offset, const_1, "")?;
    build_memory_limit_check!(ctx, current, end_offset);

    let book = current.book_ref();
    let mem = ctx.builder.build_int_add(book.mem, offset, "")?;
    let dest_ptr = ctx.builder.build_int_to_ptr(
        mem,
        ctx.types.type_i8.ptr_type(AddressSpace::default()),
        "",
    )?;

    ctx.builder.build_store(dest_ptr, value)?;

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current
        .next()
        .add_incoming(current.book_ref(), current.block());
    Ok(())
}

pub(crate) fn build_mload_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 1, 0);

    let offset = build_stack_pop!(ctx, current);

    build_memory_gas_check!(ctx, current, offset, 32);

    let offset = ctx
        .builder
        .build_int_cast(offset, ctx.types.type_ptrint, "")?;

    let const_32 = ctx.types.type_ptrint.const_int(32, false);
    let end_offset = ctx.builder.build_int_add(offset, const_32, "")?;
    build_memory_limit_check!(ctx, current, end_offset);

    let book = current.book_ref();
    let mem0 = ctx.builder.build_int_add(book.mem, offset, "")?;
    let mem1_offset = ctx
        .types
        .type_ptrint
        .const_int(EVM_JIT_STACK_ALIGN as u64, false);
    let mem1 = ctx.builder.build_int_add(mem0, mem1_offset, "")?;

    let mem0_ptr = ctx.builder.build_int_to_ptr(
        mem0,
        ctx.types.type_ivec.ptr_type(AddressSpace::default()),
        "",
    )?;
    let mem1_ptr = ctx.builder.build_int_to_ptr(
        mem1,
        ctx.types.type_ivec.ptr_type(AddressSpace::default()),
        "",
    )?;

    let vec0 = ctx
        .builder
        .build_load(ctx.types.type_ivec, mem0_ptr, "")?
        .into_vector_value();
    let vec1 = ctx
        .builder
        .build_load(ctx.types.type_ivec, mem1_ptr, "")?
        .into_vector_value();

    vec0.as_instruction_value()
        .ok_or(JitEvmEngineError::NoInstructionValue)?
        .set_alignment(1)?;
    vec1.as_instruction_value()
        .ok_or(JitEvmEngineError::NoInstructionValue)?
        .set_alignment(1)?;

    let shuffled = ctx
        .builder
        .build_shuffle_vector(vec0, vec1, ctx.types.swap_bytes, "")?;

    build_stack_push_vector!(ctx, current, shuffled);

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current
        .next()
        .add_incoming(current.book_ref(), current.block());
    Ok(())
}
