use crate::jit::stack::{build_stack_check, build_stack_pop, build_stack_push};
use crate::jit::{
    cursor::CurrentInstruction, EvmOp, JitEvmEngineError, JitEvmEngineSimpleBlock,
    OperationsContext,
};
use inkwell::{AddressSpace, IntPredicate};

pub(crate) fn iszero_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let book = current.book();
    build_stack_check!(ctx, current, book, 1, 0);
    let next = current.next();
    let this = current.block();

    let (book, val) = build_stack_pop!(ctx, book);
    let cmp = ctx.builder.build_int_compare(
        IntPredicate::EQ,
        ctx.types.type_stackel.const_int(0, false),
        val,
        "",
    )?;

    let push_0 = JitEvmEngineSimpleBlock::new(
        ctx,
        this.block,
        &format!(
            "Instruction #{}: {:?} / push 0",
            current.idx(),
            current.op()
        ),
        &format!("_{}_0", current.idx()),
    )?;
    let push_1 = JitEvmEngineSimpleBlock::new(
        ctx,
        push_0.block,
        &format!(
            "Instruction #{}: {:?} / push 1",
            current.idx(),
            current.op()
        ),
        &format!("_{}_1", current.idx()),
    )?;

    ctx.builder.position_at_end(this.block);
    ctx.builder
        .build_conditional_branch(cmp, push_1.block, push_0.block)?;
    push_0.add_incoming(&book, this);
    push_1.add_incoming(&book, this);

    ctx.builder.position_at_end(push_0.block);
    let const_0 = ctx.types.type_stackel.const_int(0, false);
    let book_0 = build_stack_push!(ctx, book, const_0);
    ctx.builder.build_unconditional_branch(next.block)?;
    next.add_incoming(&book_0, &push_0);

    ctx.builder.position_at_end(push_1.block);
    let const_1 = ctx.types.type_stackel.const_int(1, false);
    let book_1 = build_stack_push!(ctx, book, const_1);
    ctx.builder.build_unconditional_branch(next.block)?;
    next.add_incoming(&book_1, &push_1);

    Ok(())
}

pub(crate) fn build_byte_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let book = current.book();
    build_stack_check!(ctx, current, book, 2, 0);

    let (book, i) = build_stack_pop!(ctx, book);
    let (book, x) = build_stack_pop!(ctx, book);

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

    let book = build_stack_push!(ctx, book, val);
    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current.next().add_incoming(&book, current.block());
    Ok(())
}

pub(crate) fn build_arithmetic_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let book = current.book();
    build_stack_check!(ctx, current, book, 2, 0);

    let (book, a) = build_stack_pop!(ctx, book);
    let (book, b) = build_stack_pop!(ctx, book);
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

    let book = build_stack_push!(ctx, book, d);
    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current.next().add_incoming(&book, current.block());

    Ok(())
}

pub(crate) fn build_cmp_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
    predicate: IntPredicate,
) -> Result<(), JitEvmEngineError> {
    let book = current.book();
    build_stack_check!(ctx, current, book, 2, 0);
    let this = current.block();
    let next = current.next();

    let (book, a) = build_stack_pop!(ctx, book);
    let (book, b) = build_stack_pop!(ctx, book);
    let cmp = ctx.builder.build_int_compare(predicate, a, b, "")?;

    let push_0 = JitEvmEngineSimpleBlock::new(
        ctx,
        this.block,
        &format!(
            "Instruction #{}: {:?} / push 0",
            current.idx(),
            current.op()
        ),
        &format!("_{}_0", current.idx()),
    )?;
    let push_1 = JitEvmEngineSimpleBlock::new(
        ctx,
        push_0.block,
        &format!(
            "Instruction #{}: {:?} / push 1",
            current.idx(),
            current.op()
        ),
        &format!("_{}_1", current.idx()),
    )?;

    ctx.builder.position_at_end(this.block);
    ctx.builder
        .build_conditional_branch(cmp, push_1.block, push_0.block)?;
    push_0.add_incoming(&book, this);
    push_1.add_incoming(&book, this);

    ctx.builder.position_at_end(push_0.block);
    let const_0 = ctx.types.type_stackel.const_int(0, false);
    let book_0 = build_stack_push!(ctx, book, const_0);
    ctx.builder.build_unconditional_branch(next.block)?;
    next.add_incoming(&book_0, &push_0);

    ctx.builder.position_at_end(push_1.block);
    let const_1 = ctx.types.type_stackel.const_int(1, false);
    let book_1 = build_stack_push!(ctx, book, const_1);
    ctx.builder.build_unconditional_branch(next.block)?;
    next.add_incoming(&book_1, &push_1);

    Ok(())
}

pub(crate) fn build_signextend_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let book = current.book();
    build_stack_check!(ctx, current, book, 2, 0);
    let this = current.block();
    let next = current.next();

    let (book, x) = build_stack_pop!(ctx, book);

    let const_32 = ctx.types.type_stackel.const_int(32, false);
    let cmp = ctx
        .builder
        .build_int_compare(IntPredicate::UGE, x, const_32, "")?;

    let label = format!("Instruction #{}: Signextend / else", current.idx());
    let index = format!("_{}", current.idx());
    let else_block = JitEvmEngineSimpleBlock::new(ctx, this.block, &label, &index)?;
    ctx.builder.position_at_end(this.block);
    ctx.builder
        .build_conditional_branch(cmp, next.block, else_block.block)?;

    next.add_incoming(&book, this);
    else_block.add_incoming(&book, this);

    ctx.builder.position_at_end(else_block.block);

    let (book, y) = build_stack_pop!(ctx, book);

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

    build_stack_push!(ctx, book, result);
    ctx.builder.build_unconditional_branch(next.block)?;
    Ok(())
}

pub(crate) fn build_mod_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let book = current.book();
    build_stack_check!(ctx, current, book, 3, 0);

    let (book, a) = build_stack_pop!(ctx, book);
    let (book, b) = build_stack_pop!(ctx, book);
    let (book, n) = build_stack_pop!(ctx, book);

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

    let book = build_stack_push!(ctx, book, result);
    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current.next().add_incoming(&book, current.block());
    Ok(())
}

macro_rules! op2_i256_exp_bit {
    ($ctx:ident, $book:ident, $accum:ident, $base:ident, $exp:ident, $mask:ident) => {{
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

pub(crate) fn build_exp_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let book = current.book();
    build_stack_check!(ctx, current, book, 2, 0);
    let this = current.block();
    let next = current.next();

    let (book, a) = build_stack_pop!(ctx, book);
    let (book, exp) = build_stack_pop!(ctx, book);

    let const_1 = ctx.types.type_stackel.const_int(1, false);
    let zero = ctx.types.type_stackel.const_int(0, false);
    let is_zero = ctx
        .builder
        .build_int_compare(IntPredicate::EQ, exp, zero, "exp_check")?;

    let else_label = format!("Instruction #{}: Exp / else", current.idx());
    let then_label = format!("Instruction #{}: Exp / then", current.idx());
    let index = format!("_{}", current.idx());
    let else_block = JitEvmEngineSimpleBlock::new(ctx, this.block, &else_label, &index)?;
    let then_block = JitEvmEngineSimpleBlock::new(ctx, this.block, &then_label, &index)?;

    ctx.builder.position_at_end(this.block);
    ctx.builder
        .build_conditional_branch(is_zero, then_block.block, else_block.block)?;

    else_block.add_incoming(&book, this);
    then_block.add_incoming(&book, this);

    //// THEN BLOCK
    ctx.builder.position_at_end(then_block.block);

    let then_book = then_block.book();
    let then_book = build_stack_push!(ctx, then_book, const_1);

    next.add_incoming(&then_book, &then_block);

    ctx.builder.build_unconditional_branch(next.block)?;

    //// ELSE BLOCK
    ctx.builder.position_at_end(else_block.block);
    let else_book = else_block.book();

    let const8 = ctx.types.type_ptrint.const_int(8, false);
    let const1 = ctx.types.type_ptrint.const_int(1, false);
    let msbyte = build_get_msbyte!(ctx, exp);
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

    let loop_label = format!("Instruction #{}: Exp / loop", current.idx());
    let loop_end_label = format!("Instruction #{}: Exp / loop_end", current.idx());

    let loop_block = JitEvmEngineSimpleBlock::new(ctx, else_block.block, &loop_label, &index)?;
    let loop_end_block =
        JitEvmEngineSimpleBlock::new(ctx, loop_block.block, &loop_end_label, &index)?;

    let loop_book = loop_block.book();
    let loop_end_book = loop_end_block.book();

    loop_block.add_incoming(&else_book, &else_block);
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

    op2_i256_exp_bit!(ctx, loop_book, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, loop_book, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, loop_book, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, loop_book, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, loop_book, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, loop_book, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, loop_book, accum, a, exp, mask);
    op2_i256_exp_bit!(ctx, loop_book, accum, a, exp, mask);

    mask_phi.add_incoming(&[(&mask, loop_block.block)]);
    accum_phi.add_incoming(&[(&accum, loop_block.block)]);
    let cond = ctx
        .builder
        .build_int_compare(IntPredicate::EQ, mask, zero, "")?;
    ctx.builder
        .build_conditional_branch(cond, loop_end_block.block, loop_block.block)?;

    ctx.builder.position_at_end(loop_end_block.block);
    let final_accum = ctx.builder.build_phi(ctx.types.type_stackel, "")?;
    final_accum.add_incoming(&[(&accum, loop_block.block)]);

    let final_basic = final_accum.as_basic_value().into_int_value();
    build_stack_push!(ctx, loop_end_book, final_basic);
    ctx.builder.build_unconditional_branch(next.block)?;

    next.add_incoming(&loop_end_book, &loop_end_block);
    Ok(())
}

pub(crate) fn build_not_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let book = current.book();
    build_stack_check!(ctx, current, book, 1, 0);

    let (book, a) = build_stack_pop!(ctx, book);
    let d = ctx.builder.build_not(a, "")?;
    let book = build_stack_push!(ctx, book, d);

    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current.next().add_incoming(&book, current.block());
    Ok(())
}
