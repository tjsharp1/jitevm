use crate::jit::{
    contract::BuilderContext, cursor::Current, EvmOp, JitEvmEngineError, EVM_JIT_STACK_ALIGN,
    EVM_STACK_ELEMENT_SIZE,
};
use alloy_primitives::U256;
use inkwell::AddressSpace;

pub(crate) fn build_push_op<'ctx>(
    ctx: &BuilderContext<'ctx>,
    current: &mut Current<'_, 'ctx>,
    val: U256,
) -> Result<(), JitEvmEngineError> {
    let val = ctx
        .types
        .type_stackel
        .const_int_arbitrary_precision(&val.into_limbs());
    build_stack_push!(ctx, current, val);

    Ok(())
}

pub(crate) fn build_pop_op<'ctx>(
    ctx: &BuilderContext<'ctx>,
    current: &mut Current<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let _ = build_stack_pop!(ctx, current);

    Ok(())
}

macro_rules! build_stack_push {
    ($ctx:expr, $current:ident, $val:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);

        let book = $current.book_ref();
        let sp_ptr = $ctx.builder.build_int_to_ptr(
            book.sp,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        $ctx.builder.build_store(sp_ptr, $val)?;
        let sp = $ctx.builder.build_int_add(book.sp, sp_offset, "")?;

        $current.book_ref_mut().update_sp(sp);
    }};
}

macro_rules! build_stack_pop {
    ($ctx:expr, $current:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);

        let book = $current.book_ref();
        let sp = $ctx.builder.build_int_sub(book.sp, sp_offset, "")?;
        let sp_ptr = $ctx.builder.build_int_to_ptr(
            sp,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        let val = $ctx
            .builder
            .build_load($ctx.types.type_stackel, sp_ptr, "")?
            .into_int_value();

        $current.book_ref_mut().update_sp(sp);
        val
    }};
}

macro_rules! build_stack_push_vector {
    ($ctx:expr, $current:ident, $val:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);

        let book = $current.book_ref();
        let sp_ptr = $ctx.builder.build_int_to_ptr(
            book.sp,
            $ctx.types.type_rvec.ptr_type(AddressSpace::default()),
            "",
        )?;
        $ctx.builder.build_store(sp_ptr, $val)?;
        let sp = $ctx.builder.build_int_add(book.sp, sp_offset, "")?;

        $current.book_ref_mut().update_sp(sp);
    }};
}

macro_rules! build_stack_pop_vector {
    ($ctx:expr, $current:ident) => {{
        use crate::jit::{EVM_JIT_STACK_ALIGN, EVM_STACK_ELEMENT_SIZE};
        let sp0_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);
        let sp1_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_JIT_STACK_ALIGN as u64, false);

        let book = $current.book_ref();
        let sp0 = $ctx.builder.build_int_sub(book.sp, sp0_offset, "")?;
        let sp1 = $ctx.builder.build_int_sub(book.sp, sp1_offset, "")?;

        let sp0_ptr = $ctx.builder.build_int_to_ptr(
            sp0,
            $ctx.types.type_ivec.ptr_type(AddressSpace::default()),
            "",
        )?;
        let sp1_ptr = $ctx.builder.build_int_to_ptr(
            sp1,
            $ctx.types.type_ivec.ptr_type(AddressSpace::default()),
            "",
        )?;

        let vec0 = $ctx
            .builder
            .build_load($ctx.types.type_ivec, sp0_ptr, "")?
            .into_vector_value();
        let vec1 = $ctx
            .builder
            .build_load($ctx.types.type_ivec, sp1_ptr, "")?
            .into_vector_value();

        $current.book_ref_mut().update_sp(sp0);
        (vec0, vec1)
    }};
}

macro_rules! build_stack_inc {
    ($ctx:expr, $current:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);

        let book = $current.book_ref();
        let sp = $ctx.builder.build_int_add(book.sp, sp_offset, "")?;

        $current.book_ref_mut().update_sp(sp);
    }};
}

macro_rules! build_stack_write {
    ($ctx:expr, $current:ident, $idx:expr, $val:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let idx = $ctx
            .types
            .type_ptrint
            .const_int($idx * EVM_STACK_ELEMENT_SIZE, false);

        let book = $current.book_ref();
        let sp_int = $ctx.builder.build_int_sub(book.sp, idx, "")?;
        let sp_ptr = $ctx.builder.build_int_to_ptr(
            sp_int,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        $ctx.builder.build_store(sp_ptr, $val)?;
    }};
}

macro_rules! build_stack_read {
    ($ctx:expr, $current:ident, $idx:expr) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let idx = $ctx
            .types
            .type_ptrint
            .const_int($idx * EVM_STACK_ELEMENT_SIZE, false);

        let book = $current.book_ref();
        let sp_int = $ctx.builder.build_int_sub(book.sp, idx, "")?;
        let sp_ptr = $ctx.builder.build_int_to_ptr(
            sp_int,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        let val = $ctx
            .builder
            .build_load($ctx.types.type_stackel, sp_ptr, "")?
            .into_int_value();

        val
    }};
}

pub(crate) fn build_stack_swap_op<'ctx>(
    ctx: &BuilderContext<'ctx>,
    current: &mut Current<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let swap1 = EvmOp::Swap1.opcode();
    let swap16 = EvmOp::Swap16.opcode();
    let opcode = current.op().opcode();

    assert!(swap1 <= opcode && opcode <= swap16);

    let idx = (opcode - swap1) as u64 + 2;

    let a = build_stack_read!(ctx, current, 1);
    let b = build_stack_read!(ctx, current, idx);
    build_stack_write!(ctx, current, 1, b);
    build_stack_write!(ctx, current, idx, a);

    Ok(())
}

pub(crate) fn build_dup_op<'ctx>(
    ctx: &BuilderContext<'ctx>,
    current: &mut Current<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let dup1 = EvmOp::Dup1.opcode();
    let dup16 = EvmOp::Dup16.opcode();
    let opcode = current.op().opcode();

    assert!(dup1 <= opcode && opcode <= dup16);

    let idx = (opcode - dup1) as u64 + 1;

    let book = current.book_ref();
    let len_stackel = ctx
        .types
        .type_ptrint
        .const_int(EVM_STACK_ELEMENT_SIZE, false);
    let sp_src_offset = ctx
        .types
        .type_ptrint
        .const_int(idx * EVM_STACK_ELEMENT_SIZE, false);
    let src_int = ctx.builder.build_int_sub(book.sp, sp_src_offset, "")?;
    let src_ptr = ctx.builder.build_int_to_ptr(
        src_int,
        ctx.types.type_stackel.ptr_type(AddressSpace::default()),
        "",
    )?;
    let dst_ptr = ctx.builder.build_int_to_ptr(
        book.sp,
        ctx.types.type_stackel.ptr_type(AddressSpace::default()),
        "",
    )?;
    ctx.builder.build_memcpy(
        dst_ptr,
        EVM_JIT_STACK_ALIGN,
        src_ptr,
        EVM_JIT_STACK_ALIGN,
        len_stackel,
    )?;
    let sp = ctx.builder.build_int_add(book.sp, len_stackel, "")?;

    current.book_ref_mut().update_sp(sp);

    Ok(())
}

pub(crate) use build_stack_pop;
pub(crate) use build_stack_push;
