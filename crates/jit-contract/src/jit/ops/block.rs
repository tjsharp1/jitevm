use crate::jit::{
    context::{JitContractExecutionResult, JitContractResultCode},
    contract::{BuilderContext, JitEvmEngineSimpleBlock},
    cursor::CurrentBlock,
    JitEvmEngineError, EVM_STACK_ELEMENT_SIZE,
};
use inkwell::IntPredicate;

pub(crate) fn insert_block_checks<'ctx>(
    ctx: &BuilderContext<'ctx>,
    current_block: &mut CurrentBlock<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let evm_block = current_block.evm_block();
    let mut err = ctx.types.type_bool.const_int(0, false);

    let sp = current_block.book_ref().sp;

    if evm_block.stack_min != 0 {
        let sp_min = current_block.book_ref().sp_min;

        let height = evm_block.stack_min * EVM_STACK_ELEMENT_SIZE as i64;
        let height_req = ctx.types.type_i64.const_int(height as u64, false);

        let min = ctx.builder.build_int_sub(sp_min, height_req, "")?;

        let cmp = ctx
            .builder
            .build_int_compare(IntPredicate::UGT, min, sp, "")?;

        err = ctx.builder.build_or(err, cmp, "")?;
    }

    if evm_block.stack_max != 0 {
        let sp_max = current_block.book_ref().sp_max;

        let height = evm_block.stack_max * EVM_STACK_ELEMENT_SIZE as i64;
        let height_req = ctx.types.type_i64.const_int(height as u64, false);

        let max = ctx.builder.build_int_sub(sp_max, height_req, "")?;

        let cmp = ctx
            .builder
            .build_int_compare(IntPredicate::ULT, max, sp, "")?;

        err = ctx.builder.build_or(err, cmp, "")?;
    }

    if evm_block.gas != 0 {
        let gas_remaining = current_block.book_ref().gas_remaining;

        let gas = ctx.types.type_i64.const_int(evm_block.gas, false);
        let cmp = ctx
            .builder
            .build_int_compare(IntPredicate::UGT, gas, gas_remaining, "")?;

        err = ctx.builder.build_or(err, cmp, "")?;
    }

    let err_block = build_error_block(ctx, current_block)?;

    let label = format!("{}-instructions", current_block.index());
    let idx = format!("_{}", current_block.index());
    let next_block = JitEvmEngineSimpleBlock::new(ctx, current_block.block().block, &label, &idx)?;

    err_block.add_incoming(current_block.book_ref(), current_block.block());
    next_block.add_incoming(current_block.book_ref(), current_block.block());

    ctx.builder.position_at_end(current_block.block().block);

    ctx.builder
        .build_conditional_branch(err, err_block.block, next_block.block)?;

    ctx.builder.position_at_end(next_block.block);
    current_block.insert_block(next_block.book(), next_block);

    let evm_block = current_block.evm_block();
    let gas = ctx.types.type_i64.const_int(evm_block.gas, false);
    let remaining = current_block.book_ref().gas_remaining;
    let remaining = ctx.builder.build_int_sub(remaining, gas, "")?;
    current_block.book_ref_mut().update_gas(remaining);

    Ok(())
}

fn build_error_block<'ctx>(
    ctx: &BuilderContext<'ctx>,
    current_block: &mut CurrentBlock<'_, 'ctx>,
) -> Result<JitEvmEngineSimpleBlock<'ctx>, JitEvmEngineError> {
    let label = format!("{}-head-error", current_block.index());
    let idx = format!("_{}", current_block.index());
    let err_block = JitEvmEngineSimpleBlock::new(ctx, current_block.block().block, &label, &idx)?;

    let code = u32::from(JitContractResultCode::StackUnderflow);
    let underflow_code = ctx.types.type_i64.const_int(code as u64, false);

    let code = u32::from(JitContractResultCode::StackOverflow);
    let overflow_code = ctx.types.type_i64.const_int(code as u64, false);

    let code = u32::from(JitContractResultCode::OutOfGasBasicOutOfGas);
    let oog_code = ctx.types.type_i64.const_int(code as u64, false);

    let mut oog = ctx.types.type_i64.const_int(0, false);
    let mut underflow = ctx.types.type_i64.const_int(0, false);
    let mut overflow = ctx.types.type_i64.const_int(0, false);

    let sp = current_block.book_ref().sp;
    let min = current_block.book_ref().sp_min;
    let max = current_block.book_ref().sp_max;
    let remaining = current_block.book_ref().gas_remaining;
    let stack_bytes = ctx.types.type_i64.const_int(EVM_STACK_ELEMENT_SIZE, false);

    let dist_bottom = ctx.builder.build_int_sub(sp, min, "")?;
    let dist_bottom = ctx
        .builder
        .build_int_unsigned_div(dist_bottom, stack_bytes, "")?;

    let dist_top = ctx.builder.build_int_sub(max, sp, "")?;
    let dist_top = ctx
        .builder
        .build_int_unsigned_div(dist_top, stack_bytes, "")?;

    let evm_block = current_block.evm_block();
    for check in evm_block.checks.iter().rev() {
        if evm_block.stack_min != 0 {
            let stack = ctx.types.type_i64.const_int(-check.low as u64, false);
            let check = ctx
                .builder
                .build_int_compare(IntPredicate::UGT, stack, dist_bottom, "")?;
            let count =
                ctx.builder
                    .build_int_cast_sign_flag(check, ctx.types.type_i64, false, "")?;

            underflow = ctx.builder.build_int_add(underflow, count, "")?;
        }

        if evm_block.stack_max != 0 {
            let stack = ctx.types.type_i64.const_int(check.high as u64, false);
            let check = ctx
                .builder
                .build_int_compare(IntPredicate::UGT, stack, dist_top, "")?;
            let count =
                ctx.builder
                    .build_int_cast_sign_flag(check, ctx.types.type_i64, false, "")?;

            overflow = ctx.builder.build_int_add(overflow, count, "")?;
        }

        if evm_block.gas != 0 {
            let gas = ctx.types.type_i64.const_int(check.gas, false);
            let check = ctx
                .builder
                .build_int_compare(IntPredicate::UGT, gas, remaining, "")?;
            let count =
                ctx.builder
                    .build_int_cast_sign_flag(check, ctx.types.type_i64, false, "")?;

            oog = ctx.builder.build_int_add(oog, count, "")?;
        }
    }

    let cmp = ctx
        .builder
        .build_int_compare(IntPredicate::UGE, oog, overflow, "")?;
    let code = ctx
        .builder
        .build_select(cmp, oog_code, overflow_code, "")?
        .into_int_value();
    let score = ctx
        .builder
        .build_select(cmp, oog, overflow, "")?
        .into_int_value();

    let cmp = ctx
        .builder
        .build_int_compare(IntPredicate::UGE, score, underflow, "")?;
    let code = ctx
        .builder
        .build_select(cmp, code, underflow_code, "")?
        .into_int_value();

    JitContractExecutionResult::build_exit_halt(ctx, &err_block.book(), code)?;

    Ok(err_block)
}
