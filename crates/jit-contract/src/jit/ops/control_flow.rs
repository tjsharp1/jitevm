use crate::jit::ops::stack::build_stack_pop;
use crate::jit::{
    context::JitContractResultCode,
    contract::{BuilderContext, JitEvmEngineSimpleBlock},
    cursor::Current,
    JitContractExecutionResult, JitEvmEngineError,
};
use alloy_primitives::U256;
use inkwell::{AddressSpace, IntPredicate};

pub(crate) fn build_stop_op<'ctx>(
    ctx: &BuilderContext<'ctx>,
    current: &mut Current<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let book = current.book_ref();

    JitContractExecutionResult::build_exit_stop(ctx, book)?;

    Ok(())
}

pub(crate) fn build_jumpdest_op<'ctx>(
    _: &BuilderContext<'ctx>,
    _: &mut Current<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    Ok(())
}

pub(crate) fn build_jump_op<'ctx>(
    ctx: &BuilderContext<'ctx>,
    current: &mut Current<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let target = build_stack_pop!(ctx, current);

    let book = current.book_ref();
    let this = current.block();
    let jumpdests = current.jumpdests();
    let target2blockidx = current.target2blockidx();

    if jumpdests.is_empty() {
        return Err(JitEvmEngineError::NoValidJumpDestinations(current.idx()));
    } else {
        let blocks = current.blocks();
        let label = format!("invalid-jump-{}", current.idx());
        let idx = format!("jump-{}", current.idx());

        let error_block = JitEvmEngineSimpleBlock::new(ctx, this.block, &label, &idx)?;
        let code = u32::from(JitContractResultCode::InvalidJump);
        let code = ctx.types.type_i64.const_int(code as u64, false);
        JitContractExecutionResult::build_exit_halt(ctx, &error_block.book(), code)?;
        error_block.add_incoming(book, this);

        ctx.builder.position_at_end(this.block);

        let mut destinations = Vec::new();
        for (key, value) in target2blockidx {
            let t = ctx.types.type_i64.const_int(*key as u64, false);
            blocks[*value].add_incoming(book, this);
            destinations.push((t, blocks[*value].block));
        }

        let target = ctx.builder.build_int_cast(target, ctx.types.type_i64, "")?;

        ctx.builder
            .build_switch(target, error_block.block, &destinations)?;
    }

    Ok(())
}

pub(crate) fn build_jumpi_op<'ctx>(
    ctx: &BuilderContext<'ctx>,
    current: &mut Current<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let target = build_stack_pop!(ctx, current);
    let val = build_stack_pop!(ctx, current);

    let book = current.book_ref();
    let this = current.block();
    let next = current.next_block();
    let jumpdests = current.jumpdests();
    let target2blockidx = current.target2blockidx();

    if jumpdests.is_empty() {
        return Err(JitEvmEngineError::NoValidJumpDestinations(current.idx()));
    } else {
        let label = format!("jumpi-block-{}", current.idx());
        let idx = format!("jumpi-{}", current.idx());
        let jump_table = JitEvmEngineSimpleBlock::new(ctx, this.block, &label, &idx)?;

        ctx.builder.position_at_end(this.block);
        let cmp = ctx.builder.build_int_compare(
            IntPredicate::EQ,
            ctx.types.type_stackel.const_int(0, false),
            val,
            "",
        )?;
        ctx.builder
            .build_conditional_branch(cmp, next.block, jump_table.block)?;
        next.add_incoming(book, this);
        jump_table.add_incoming(book, this);

        let book = jump_table.book();
        let label = format!("invalid-jumpi-block-{}", current.idx());
        let idx = format!("jumpi-{}", current.idx());
        let error_block = JitEvmEngineSimpleBlock::new(ctx, jump_table.block, &label, &idx)?;
        let code = u32::from(JitContractResultCode::InvalidJump);
        let code = ctx.types.type_i64.const_int(code as u64, false);
        JitContractExecutionResult::build_exit_halt(ctx, &error_block.book(), code)?;
        error_block.add_incoming(&book, &jump_table);

        ctx.builder.position_at_end(jump_table.block);

        let blocks = current.blocks();
        let mut destinations = Vec::new();
        for (key, value) in target2blockidx {
            let t = ctx.types.type_i64.const_int(*key as u64, false);
            blocks[*value].add_incoming(&book, &jump_table);
            destinations.push((t, blocks[*value].block));
        }

        let target = ctx.builder.build_int_cast(target, ctx.types.type_i64, "")?;
        ctx.builder
            .build_switch(target, error_block.block, &destinations)?;
    }

    Ok(())
}

pub(crate) fn build_augmented_jump_op<'ctx>(
    ctx: &BuilderContext<'ctx>,
    current: &mut Current<'_, 'ctx>,
    val: U256,
) -> Result<(), JitEvmEngineError> {
    let book = current.book_ref();
    let this = current.block();
    let target2blockidx = current.target2blockidx();
    let jumpdests = current.jumpdests();
    let blocks = current.blocks();

    if jumpdests.is_empty() {
        return Err(JitEvmEngineError::NoValidJumpDestinations(current.idx()));
    } else {
        let jmp_i = target2blockidx[&(val.as_limbs()[0] as usize)];
        ctx.builder
            .build_unconditional_branch(blocks[jmp_i].block)?;
        blocks[jmp_i].add_incoming(book, this);
    }
    Ok(())
}

pub(crate) fn build_augmented_jumpi_op<'ctx>(
    ctx: &BuilderContext<'ctx>,
    current: &mut Current<'_, 'ctx>,
    val: U256,
) -> Result<(), JitEvmEngineError> {
    let condition = build_stack_pop!(ctx, current);

    let book = current.book_ref();
    let this = current.block();
    let next = current.next_block();
    let jumpdests = current.jumpdests();
    let target2blockidx = current.target2blockidx();
    let blocks = current.blocks();

    if jumpdests.is_empty() {
        return Err(JitEvmEngineError::NoValidJumpDestinations(current.idx()));
    } else {
        let jmp_i = target2blockidx[&(val.as_limbs()[0] as usize)];
        // ... and jump to there (conditionally)!
        let cmp = ctx.builder.build_int_compare(
            IntPredicate::EQ,
            ctx.types.type_stackel.const_int(0, false),
            condition,
            "",
        )?;
        ctx.builder
            .build_conditional_branch(cmp, next.block, blocks[jmp_i].block)?;
        next.add_incoming(book, this);
        blocks[jmp_i].add_incoming(book, this);
    }

    Ok(())
}
