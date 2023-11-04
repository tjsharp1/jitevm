use crate::jit::ops::{
    build_stack_check, build_stack_pop, build_stack_pop_vector, build_stack_push_vector,
};
use crate::jit::{
    contract::BuilderContext,
    cursor::CurrentInstruction,
    gas::{build_memory_gas_check, const_cost, memory_expansion_cost, memory_gas},
    JitEvmEngineError, EVM_JIT_STACK_ALIGN,
};
use inkwell::{values::BasicValue, AddressSpace};
use revm_primitives::Spec;

pub(crate) fn build_mstore_op<'a, 'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 2, 0);

    let book = current.book();
    let (book, offset) = build_stack_pop!(ctx, book);
    build_memory_gas_check!(ctx, current, book, offset, 32);

    let book = current.book();
    let (book, vec0, vec1) = build_stack_pop_vector!(ctx, book);

    let shuffled = ctx
        .builder
        .build_shuffle_vector(vec0, vec1, ctx.types.swap_bytes, "")?;

    let casted = ctx
        .builder
        .build_int_cast(offset, ctx.types.type_ptrint, "")?;
    let mem = ctx.builder.build_int_add(book.mem, casted, "")?;
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
    current.next().add_incoming(&book, current.block());
    Ok(())
}

pub(crate) fn build_mstore8_op<'a, 'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 2, 0);

    let book = current.book();
    let (book, offset) = build_stack_pop!(ctx, book);
    let (book, value) = build_stack_pop!(ctx, book);

    build_memory_gas_check!(ctx, current, book, offset, 1);

    let book = current.book();

    let value_casted = ctx.builder.build_int_cast(value, ctx.types.type_i8, "")?;
    let offset_casted = ctx
        .builder
        .build_int_cast(offset, ctx.types.type_ptrint, "")?;

    let mem = ctx.builder.build_int_add(book.mem, offset_casted, "")?;
    let dest_ptr = ctx.builder.build_int_to_ptr(
        mem,
        ctx.types.type_i8.ptr_type(AddressSpace::default()),
        "",
    )?;

    ctx.builder.build_store(dest_ptr, value_casted)?;

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current.next().add_incoming(&book, current.block());
    Ok(())
}

pub(crate) fn build_mload_op<'a, 'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 1, 0);

    let book = current.book();
    let (book, offset) = build_stack_pop!(ctx, book);

    build_memory_gas_check!(ctx, current, book, offset, 32);

    let book = current.book();

    let casted = ctx
        .builder
        .build_int_cast(offset, ctx.types.type_ptrint, "")?;

    let mem0 = ctx.builder.build_int_add(book.mem, casted, "")?;
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

    let book = build_stack_push_vector!(ctx, book, shuffled);

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current.next().add_incoming(&book, current.block());
    Ok(())
}
