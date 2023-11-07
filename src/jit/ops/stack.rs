use crate::jit::{
    contract::BuilderContext,
    cursor::CurrentInstruction,
    gas::{build_gas_check, const_cost},
    EvmOp, JitEvmEngineError,
};
use alloy_primitives::U256;
use inkwell::AddressSpace;
use revm_primitives::Spec;

pub(crate) fn build_push_op<'a, 'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
    val: U256,
) -> Result<(), JitEvmEngineError> {
    build_gas_check!(ctx, current);
    build_stack_check!(ctx, current, 0, 1);

    let book = current.book();
    let val = ctx
        .types
        .type_stackel
        .const_int_arbitrary_precision(&val.into_limbs());
    let book = build_stack_push!(ctx, book, val);

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current.next().add_incoming(&book, current.block());
    Ok(())
}

pub(crate) fn build_pop_op<'a, 'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_gas_check!(ctx, current);
    build_stack_check!(ctx, current, 1, 0);

    let book = current.book();
    let (book, _) = build_stack_pop!(ctx, book);

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current.next().add_incoming(&book, current.block());
    Ok(())
}

macro_rules! build_stack_check {
    ($ctx:expr, $current:ident, $min_stack:literal, $growth:literal) => {{
        let min = $min_stack;
        build_stack_check!($ctx, $current, min, $growth);
    }};
    ($ctx:expr, $current:ident, $min_stack:ident, $growth:literal) => {{
        use crate::jit::{
            context::JitContractResultCode, contract::JitEvmEngineSimpleBlock,
            EVM_STACK_ELEMENT_SIZE,
        };
        use inkwell::{intrinsics::Intrinsic, IntPredicate};

        let expect = Intrinsic::find("llvm.expect").expect("expect intrinsic not found!");
        let expect_fn = expect
            .get_declaration(
                &$ctx.module,
                &[$ctx.types.type_bool.into(), $ctx.types.type_bool.into()],
            )
            .expect("Expect intrinsic declaration not found!");
        let const_true = $ctx.types.type_bool.const_int(1, false);

        if $min_stack > 0 {
            let book = $current.book();
            let min_stack = $min_stack * EVM_STACK_ELEMENT_SIZE;
            let const_min_stack = $ctx.types.type_i64.const_int(min_stack, false);
            let min_sp = $ctx
                .builder
                .build_int_add(book.sp_min, const_min_stack, "")?;
            let cmp = $ctx
                .builder
                .build_int_compare(IntPredicate::UGE, book.sp, min_sp, "")?;

            let instruction_label = format!("{:?}_{}_no_underfl", $current.op(), $current.idx());
            let idx = format!("_{}", $current.idx());
            let next_block = JitEvmEngineSimpleBlock::new(
                $ctx,
                $current.block().block,
                &instruction_label,
                &idx,
            )?;

            next_block.add_incoming(&book, $current.block());

            let underflow = u32::from(JitContractResultCode::StackUnderflow);
            let result_code = $ctx.types.type_i32.const_int(underflow as u64, false);
            $current.incoming_error(&book, $current.block(), result_code);

            $ctx.builder.position_at_end($current.block().block);
            let cmp = $ctx
                .builder
                .build_call(expect_fn, &[cmp.into(), const_true.into()], "")?
                .try_as_basic_value()
                .left()
                .ok_or(JitEvmEngineError::NoInstructionValue)?
                .into_int_value();
            $ctx.builder
                .build_conditional_branch(cmp, next_block.block, $current.error_block())?;
            $ctx.builder.position_at_end(next_block.block);

            $current.update_current_block(next_block);
        }

        if $growth > 0 {
            let book = $current.book();
            let need_stack = $growth * EVM_STACK_ELEMENT_SIZE;
            let const_need_stack = $ctx.types.type_i64.const_int(need_stack, false);
            let max_sp = $ctx
                .builder
                .build_int_sub(book.sp_max, const_need_stack, "")?;
            let cmp = $ctx
                .builder
                .build_int_compare(IntPredicate::ULE, book.sp, max_sp, "")?;

            let instruction_label = format!("{:?}_{}_no_ovf", $current.op(), $current.idx());
            let idx = format!("_{}", $current.idx());
            let next_block = JitEvmEngineSimpleBlock::new(
                $ctx,
                $current.block().block,
                &instruction_label,
                &idx,
            )?;

            next_block.add_incoming(&book, $current.block());

            let overflow = u32::from(JitContractResultCode::StackOverflow);
            let result_code = $ctx.types.type_i32.const_int(overflow as u64, false);
            $current.incoming_error(&book, $current.block(), result_code);

            $ctx.builder.position_at_end($current.block().block);
            let cmp = $ctx
                .builder
                .build_call(expect_fn, &[cmp.into(), const_true.into()], "")?
                .try_as_basic_value()
                .left()
                .ok_or(JitEvmEngineError::NoInstructionValue)?
                .into_int_value();
            $ctx.builder
                .build_conditional_branch(cmp, next_block.block, $current.error_block())?;
            $ctx.builder.position_at_end(next_block.block);

            $current.update_current_block(next_block);
        }
    }};
}

macro_rules! build_stack_push {
    ($ctx:expr, $book:ident, $val:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);

        let sp_ptr = $ctx.builder.build_int_to_ptr(
            $book.sp,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        $ctx.builder.build_store(sp_ptr, $val)?;
        let sp = $ctx.builder.build_int_add($book.sp, sp_offset, "")?;

        $book.update_sp(sp)
    }};
}

macro_rules! build_stack_pop {
    ($ctx:expr, $book:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);

        let sp = $ctx.builder.build_int_sub($book.sp, sp_offset, "")?;
        let sp_ptr = $ctx.builder.build_int_to_ptr(
            sp,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        let val = $ctx
            .builder
            .build_load($ctx.types.type_stackel, sp_ptr, "")?
            .into_int_value();

        ($book.update_sp(sp), val)
    }};
}

macro_rules! build_stack_push_vector {
    ($ctx:expr, $book:ident, $val:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);

        let sp_ptr = $ctx.builder.build_int_to_ptr(
            $book.sp,
            $ctx.types.type_rvec.ptr_type(AddressSpace::default()),
            "",
        )?;
        $ctx.builder.build_store(sp_ptr, $val)?;
        let sp = $ctx.builder.build_int_add($book.sp, sp_offset, "")?;

        $book.update_sp(sp)
    }};
}

macro_rules! build_stack_pop_vector {
    ($ctx:expr, $book:ident) => {{
        use crate::jit::{EVM_JIT_STACK_ALIGN, EVM_STACK_ELEMENT_SIZE};
        let sp0_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);
        let sp1_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_JIT_STACK_ALIGN as u64, false);

        let sp0 = $ctx.builder.build_int_sub($book.sp, sp0_offset, "")?;
        let sp1 = $ctx.builder.build_int_sub($book.sp, sp1_offset, "")?;

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

        ($book.update_sp(sp0), vec0, vec1)
    }};
}

macro_rules! build_stack_inc {
    ($ctx:expr, $book:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);
        let sp = $ctx.builder.build_int_add($book.sp, sp_offset, "")?;

        $book.update_sp(sp)
    }};
}

macro_rules! build_stack_write {
    ($ctx:expr, $book:ident, $idx:expr, $val:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let idx = $ctx
            .types
            .type_ptrint
            .const_int($idx * EVM_STACK_ELEMENT_SIZE, false);

        let sp_int = $ctx.builder.build_int_sub($book.sp, idx, "")?;
        let sp_ptr = $ctx.builder.build_int_to_ptr(
            sp_int,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        $ctx.builder.build_store(sp_ptr, $val)?;

        $book
    }};
}

macro_rules! build_stack_read {
    ($ctx:expr, $book:ident, $idx:expr) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let idx = $ctx
            .types
            .type_ptrint
            .const_int($idx * EVM_STACK_ELEMENT_SIZE, false);

        let sp_int = $ctx.builder.build_int_sub($book.sp, idx, "")?;
        let sp_ptr = $ctx.builder.build_int_to_ptr(
            sp_int,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        let val = $ctx
            .builder
            .build_load($ctx.types.type_stackel, sp_ptr, "")?
            .into_int_value();

        ($book, val)
    }};
}

pub(crate) fn build_stack_swap_op<'a, 'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let swap1 = EvmOp::Swap1.opcode();
    let swap16 = EvmOp::Swap16.opcode();
    let opcode = current.op().opcode();

    assert!(swap1 <= opcode && opcode <= swap16);

    let idx = (opcode - swap1) as u64 + 2;
    build_gas_check!(ctx, current);
    build_stack_check!(ctx, current, idx, 0);

    let book = current.book();
    let (book, a) = build_stack_read!(ctx, book, 1);
    let (book, b) = build_stack_read!(ctx, book, idx as u64);
    let book = build_stack_write!(ctx, book, 1, b);
    let book = build_stack_write!(ctx, book, idx, a);

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current.next().add_incoming(&book, current.block());

    Ok(())
}

pub(crate) fn build_dup_op<'a, 'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    use crate::jit::{EVM_JIT_STACK_ALIGN, EVM_STACK_ELEMENT_SIZE};

    let dup1 = EvmOp::Dup1.opcode();
    let dup16 = EvmOp::Dup16.opcode();
    let opcode = current.op().opcode();

    assert!(dup1 <= opcode && opcode <= dup16);

    let idx = (opcode - dup1) as u64 + 1;
    build_gas_check!(ctx, current);
    build_stack_check!(ctx, current, idx, 1);

    let book = current.book();
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

    let book = book.update_sp(sp);

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current.next().add_incoming(&book, current.block());
    Ok(())
}

pub(crate) use build_stack_check;
pub(crate) use build_stack_pop;
pub(crate) use build_stack_push;
