use crate::jit::ops::{build_stack_check, build_stack_pop, build_stack_push};
use crate::jit::{
    contract::{BuilderContext, JitEvmEngineSimpleBlock},
    cursor::CurrentInstruction,
    gas::{build_gas_check, build_gas_check_exp, const_cost, exp_cost},
    EvmOp, JitEvmEngineError,
};
use inkwell::{AddressSpace, IntPredicate};
use revm_primitives::Spec;

pub(crate) fn iszero_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_gas_check!(ctx, current);
    build_stack_check!(ctx, current, 1, 0);

    let const_0 = ctx.types.type_stackel.const_int(0, false);
    let const_1 = ctx.types.type_stackel.const_int(1, false);

    let val = build_stack_pop!(ctx, current);
    let cmp = ctx
        .builder
        .build_int_compare(IntPredicate::EQ, const_0, val, "")?;

    let result = ctx
        .builder
        .build_select(cmp, const_1, const_0, "iszero")?
        .into_int_value();

    build_stack_push!(ctx, current, result);

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current
        .next()
        .add_incoming(current.book_ref(), current.block());
    Ok(())
}

pub(crate) fn build_byte_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_gas_check!(ctx, current);
    build_stack_check!(ctx, current, 2, 0);

    let i = build_stack_pop!(ctx, current);
    let x = build_stack_pop!(ctx, current);

    let bit_width = ctx.types.type_stackel.get_bit_width();
    let bytes = bit_width / 8;

    let const_0 = ctx.types.type_stackel.const_int(0, false);
    let const_8 = ctx.types.type_stackel.const_int(8, false);
    let const_248 = ctx.types.type_stackel.const_int(248, false);
    let bytes_width = ctx.types.type_stackel.const_int(bytes as u64, false);

    let cmp = ctx
        .builder
        .build_int_compare(IntPredicate::UGE, i, bytes_width, "")?;

    let bit_shift = ctx.builder.build_int_mul(i, const_8, "")?;

    let val = ctx.builder.build_left_shift(x, bit_shift, "")?;
    let val = ctx.builder.build_right_shift(val, const_248, false, "")?;

    let val = ctx.builder.build_select(cmp, const_0, val, "")?;

    build_stack_push!(ctx, current, val);
    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current
        .next()
        .add_incoming(current.book_ref(), current.block());
    Ok(())
}

pub(crate) fn build_arithmetic_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_gas_check!(ctx, current);
    build_stack_check!(ctx, current, 2, 0);

    let a = build_stack_pop!(ctx, current);
    let b = build_stack_pop!(ctx, current);
    let d = match current.op() {
        EvmOp::Add => ctx.builder.build_int_add(a, b, "")?,
        EvmOp::Sub => ctx.builder.build_int_sub(a, b, "")?,
        EvmOp::Mul => ctx.builder.build_int_mul(a, b, "")?,
        EvmOp::Div => ctx.builder.build_int_unsigned_div(a, b, "")?,
        EvmOp::Sdiv => ctx.builder.build_int_signed_div(a, b, "")?,
        EvmOp::Mod => ctx.builder.build_int_unsigned_rem(a, b, "")?,
        EvmOp::Smod => ctx.builder.build_int_signed_rem(a, b, "")?,
        EvmOp::Shl => ctx.builder.build_left_shift(b, a, "")?,
        EvmOp::Shr => ctx.builder.build_right_shift(b, a, false, "")?,
        EvmOp::Sar => ctx.builder.build_right_shift(b, a, true, "")?,
        EvmOp::And => ctx.builder.build_and(a, b, "")?,
        EvmOp::Or => ctx.builder.build_or(a, b, "")?,
        EvmOp::Xor => ctx.builder.build_xor(a, b, "")?,
        _ => panic!("Invalid opcode for arithmetic operations!"),
    };

    let d = match current.op() {
        EvmOp::Smod | EvmOp::Mod => {
            let const_0 = ctx.types.type_stackel.const_int(0, false);
            let cmp = ctx
                .builder
                .build_int_compare(IntPredicate::EQ, const_0, b, "")?;
            ctx.builder
                .build_select(cmp, const_0, d, "")?
                .into_int_value()
        }
        _ => d,
    };

    build_stack_push!(ctx, current, d);
    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current
        .next()
        .add_incoming(current.book_ref(), current.block());

    Ok(())
}

pub(crate) fn build_cmp_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_gas_check!(ctx, current);
    build_stack_check!(ctx, current, 2, 0);

    let a = build_stack_pop!(ctx, current);
    let b = build_stack_pop!(ctx, current);

    let predicate = match current.op() {
        EvmOp::Eq => IntPredicate::EQ,
        EvmOp::Lt => IntPredicate::ULT,
        EvmOp::Gt => IntPredicate::UGT,
        EvmOp::Slt => IntPredicate::SLT,
        EvmOp::Sgt => IntPredicate::SGT,
        _ => panic!("Invalid comparison opcode!"),
    };

    let cmp = ctx.builder.build_int_compare(predicate, a, b, "")?;
    let result = ctx
        .builder
        .build_int_cast_sign_flag(cmp, ctx.types.type_stackel, false, "")?;

    build_stack_push!(ctx, current, result);

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current
        .next()
        .add_incoming(current.book_ref(), current.block());

    Ok(())
}

pub(crate) fn build_signextend_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_gas_check!(ctx, current);
    build_stack_check!(ctx, current, 2, 0);

    let x = build_stack_pop!(ctx, current);

    let const_32 = ctx.types.type_stackel.const_int(32, false);
    let cmp = ctx
        .builder
        .build_int_compare(IntPredicate::UGE, x, const_32, "")?;

    let label = format!("i{}: Signextend / else", current.idx());
    let index = format!("_{}", current.idx());
    let else_block = JitEvmEngineSimpleBlock::new(ctx, current.block().block, &label, &index)?;
    ctx.builder.position_at_end(current.block().block);
    ctx.builder
        .build_conditional_branch(cmp, current.next().block, else_block.block)?;

    current
        .next()
        .add_incoming(current.book_ref(), current.block());
    else_block.add_incoming(current.book_ref(), current.block());

    ctx.builder.position_at_end(else_block.block);
    current.update_current_block(else_block);

    let y = build_stack_pop!(ctx, current);

    let const_1 = ctx.types.type_stackel.const_int(1, false);
    let const_7 = ctx.types.type_stackel.const_int(7, false);
    let const_8 = ctx.types.type_stackel.const_int(8, false);

    let x_8 = ctx.builder.build_int_mul(x, const_8, "")?;
    let bit_index = ctx.builder.build_int_add(x_8, const_7, "")?;
    let bit = ctx.builder.build_left_shift(const_1, bit_index, "")?;
    let mask = ctx.builder.build_int_sub(bit, const_1, "")?;

    let sign = ctx.builder.build_and(y, bit, "")?;
    let is_signed = ctx
        .builder
        .build_int_compare(IntPredicate::EQ, sign, bit, "")?;

    let not_mask = ctx.builder.build_not(mask, "")?;
    let extended = ctx.builder.build_or(y, not_mask, "")?;
    let unextended = ctx.builder.build_and(y, mask, "")?;

    let result = ctx
        .builder
        .build_select(is_signed, extended, unextended, "")?
        .into_int_value();

    build_stack_push!(ctx, current, result);

    current
        .next()
        .add_incoming(current.book_ref(), current.block());
    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    Ok(())
}

pub(crate) fn build_mod_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_gas_check!(ctx, current);
    build_stack_check!(ctx, current, 3, 0);

    let a = build_stack_pop!(ctx, current);
    let b = build_stack_pop!(ctx, current);
    let n = build_stack_pop!(ctx, current);

    let width = ctx.types.type_stackel.get_bit_width() * 2;
    let type_iup = ctx.context.custom_width_int_type(width);

    let a_up = ctx
        .builder
        .build_int_cast_sign_flag(a, type_iup, false, "")?;
    let b_up = ctx
        .builder
        .build_int_cast_sign_flag(b, type_iup, false, "")?;
    let n_up = ctx
        .builder
        .build_int_cast_sign_flag(n, type_iup, false, "")?;

    let result = match current.op() {
        EvmOp::Addmod => ctx.builder.build_int_add(a_up, b_up, "")?,
        EvmOp::Mulmod => ctx.builder.build_int_mul(a_up, b_up, "")?,
        _ => panic!("Invalid modular arithmetic opcode!"),
    };
    let result = ctx.builder.build_int_unsigned_rem(result, n_up, "")?;

    let result = ctx
        .builder
        .build_int_cast(result, ctx.types.type_stackel, "")?;

    let const_0 = ctx.types.type_stackel.const_int(0, false);
    let cmp = ctx
        .builder
        .build_int_compare(IntPredicate::EQ, n, const_0, "")?;

    let result = ctx.builder.build_select(cmp, const_0, result, "")?;

    build_stack_push!(ctx, current, result);
    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current
        .next()
        .add_incoming(current.book_ref(), current.block());
    Ok(())
}

macro_rules! op2_i256_exp_bit {
    ($ctx:ident, $accum:ident, $base:ident, $exp:ident, $mask:ident) => {{
        let one = $ctx.types.type_stackel.const_int(1, false);
        let zero = $ctx.types.type_stackel.const_int(0, false);

        let bit = $ctx.builder.build_and($exp, $mask, "")?;
        let is_zero_bit = $ctx
            .builder
            .build_int_compare(IntPredicate::EQ, bit, zero, "")?;
        $mask = $ctx.builder.build_right_shift($mask, one, false, "")?;

        $accum = $ctx.builder.build_int_mul($accum, $accum, "")?;
        let mul = $ctx
            .builder
            .build_select(is_zero_bit, one, $base, "")?
            .into_int_value();
        $accum = $ctx.builder.build_int_mul($accum, mul, "")?;
    }};
}

pub(crate) fn build_exp_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 2, 0);

    let const_1 = ctx.types.type_stackel.const_int(1, false);
    let zero = ctx.types.type_stackel.const_int(0, false);
    let index = format!("_{}", current.idx());

    // SETUP BLOCK
    let (a, exp, else_block, then_block) = {
        let a = build_stack_pop!(ctx, current);
        let exp = build_stack_pop!(ctx, current);

        let is_zero = ctx
            .builder
            .build_int_compare(IntPredicate::EQ, exp, zero, "exp_check")?;

        let else_label = format!("{}: Exp / else", current.idx());
        let then_label = format!("{}: Exp / then", current.idx());
        let else_block =
            JitEvmEngineSimpleBlock::new(ctx, current.block().block, &else_label, &index)?;
        let then_block =
            JitEvmEngineSimpleBlock::new(ctx, current.block().block, &then_label, &index)?;

        ctx.builder.position_at_end(current.block().block);
        ctx.builder
            .build_conditional_branch(is_zero, then_block.block, else_block.block)?;

        else_block.add_incoming(current.book_ref(), current.block());
        then_block.add_incoming(current.book_ref(), current.block());

        (a, exp, else_block, then_block)
    };

    //// THEN BLOCK
    {
        ctx.builder.position_at_end(then_block.block);
        current.update_current_block(then_block);

        // static gas only
        build_gas_check_exp!(ctx, current);

        build_stack_push!(ctx, current, const_1);

        let next = current.next();
        next.add_incoming(current.book_ref(), current.block());

        ctx.builder.build_unconditional_branch(next.block)?;
    }

    //// ELSE BLOCK
    ctx.builder.position_at_end(else_block.block);
    current.update_current_block(else_block);

    let const8 = ctx.types.type_i64.const_int(8, false);
    let const1 = ctx.types.type_i64.const_int(1, false);

    let msbyte = build_get_msbyte!(ctx, exp);
    build_gas_check_exp!(ctx, current, msbyte);

    let else_block = current.block();
    let else_book = current.book_ref();

    let bit = ctx.builder.build_int_mul(msbyte, const8, "")?;
    let shift = ctx.builder.build_int_sub(bit, const1, "")?;
    let shift_cast = ctx
        .builder
        .build_int_cast(shift, ctx.types.type_stackel, "")?;

    let mask_init = ctx.builder.build_left_shift(const_1, shift_cast, "")?;

    let accum_init = ctx
        .builder
        .build_bitcast(const_1, ctx.types.type_stackel, "")?
        .into_int_value();

    let loop_label = format!("{}: Exp / loop", current.idx());
    let loop_end_label = format!("{}: Exp / loop_end", current.idx());

    let loop_block = JitEvmEngineSimpleBlock::new(ctx, else_block.block, &loop_label, &index)?;
    let loop_end_block =
        JitEvmEngineSimpleBlock::new(ctx, loop_block.block, &loop_end_label, &index)?;

    let loop_book = loop_block.book();

    loop_block.add_incoming(else_book, else_block);
    loop_block.add_incoming(&loop_book, &loop_block);
    loop_end_block.add_incoming(&loop_book, &loop_block);

    ctx.builder.position_at_end(else_block.block);
    ctx.builder.build_unconditional_branch(loop_block.block)?;

    ctx.builder.position_at_end(loop_block.block);
    let mask_phi = ctx.builder.build_phi(ctx.types.type_stackel, "")?;
    let accum_phi = ctx.builder.build_phi(ctx.types.type_stackel, "")?;
    accum_phi.add_incoming(&[(&accum_init, else_block.block)]);
    mask_phi.add_incoming(&[(&mask_init, else_block.block)]);
    let mut mask = mask_phi.as_basic_value().into_int_value();
    let mut accum = accum_phi.as_basic_value().into_int_value();

    op2_i256_exp_bit!(ctx, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, accum, a, exp, mask);

    mask_phi.add_incoming(&[(&mask, loop_block.block)]);
    accum_phi.add_incoming(&[(&accum, loop_block.block)]);
    let cond = ctx
        .builder
        .build_int_compare(IntPredicate::EQ, mask, zero, "")?;
    ctx.builder
        .build_conditional_branch(cond, loop_end_block.block, loop_block.block)?;

    ctx.builder.position_at_end(loop_end_block.block);
    current.update_current_block(loop_end_block);

    let final_accum = ctx.builder.build_phi(ctx.types.type_stackel, "")?;
    final_accum.add_incoming(&[(&accum, loop_block.block)]);

    let final_basic = final_accum.as_basic_value().into_int_value();
    build_stack_push!(ctx, current, final_basic);

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current
        .next()
        .add_incoming(current.book_ref(), current.block());

    Ok(())
}

pub(crate) fn build_not_op<'ctx, SPEC: Spec>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_gas_check!(ctx, current);
    build_stack_check!(ctx, current, 1, 0);

    let a = build_stack_pop!(ctx, current);
    let d = ctx.builder.build_not(a, "")?;
    build_stack_push!(ctx, current, d);

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current
        .next()
        .add_incoming(current.book_ref(), current.block());
    Ok(())
}
