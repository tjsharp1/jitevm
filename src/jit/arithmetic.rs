macro_rules! llvmnativei256_iszero {
    ($ctx:ident, $book:ident, $this:expr, $next:expr, $op:expr, $i:expr) => {{
        // TODO: this can also be optimized...
        let (book, val) = build_stack_pop!($ctx, $book);
        let cmp = $ctx.builder.build_int_compare(
            IntPredicate::EQ,
            $ctx.types.type_stackel.const_int(0, false),
            val,
            "",
        )?;

        let push_0 = JitEvmEngineSimpleBlock::new(
            &$ctx,
            $this.block,
            &format!("Instruction #{}: {:?} / push 0", $i, $op),
            &format!("_{}_0", $i),
        )?;
        let push_1 = JitEvmEngineSimpleBlock::new(
            &$ctx,
            push_0.block,
            &format!("Instruction #{}: {:?} / push 1", $i, $op),
            &format!("_{}_1", $i),
        )?;

        $ctx.builder.position_at_end($this.block);
        $ctx.builder
            .build_conditional_branch(cmp, push_1.block, push_0.block)?;
        push_0.add_incoming(&book, &$this);
        push_1.add_incoming(&book, &$this);

        $ctx.builder.position_at_end(push_0.block);
        let const_0 = $ctx.types.type_stackel.const_int(0, false);
        let book_0 = build_stack_push!($ctx, book, const_0);
        $ctx.builder.build_unconditional_branch($next.block)?;
        $next.add_incoming(&book_0, &push_0);

        $ctx.builder.position_at_end(push_1.block);
        let const_1 = $ctx.types.type_stackel.const_int(1, false);
        let book_1 = build_stack_push!($ctx, book, const_1);
        $ctx.builder.build_unconditional_branch($next.block)?;
        $next.add_incoming(&book_1, &push_1);

        continue; // skip auto-generated jump to next instruction
    }};
}

macro_rules! llvmnativei256_op2 {
    ($ctx:ident, $book:ident, $fname:ident) => {{
        let (book, a) = build_stack_pop!($ctx, $book);
        let (book, b) = build_stack_pop!($ctx, book);
        let d = $ctx.builder.$fname(a, b, "")?;
        build_stack_push!($ctx, book, d)
    }};
}

macro_rules! llvmnativei256_lshift {
    ($ctx:ident, $book:ident, $fname:ident) => {{
        let (book, a) = build_stack_pop!($ctx, $book);
        let (book, b) = build_stack_pop!($ctx, book);
        let d = $ctx.builder.$fname(b, a, "")?;
        build_stack_push!($ctx, book, d)
    }};
}

macro_rules! llvmnativei256_rshift {
    ($ctx:ident, $book:ident, $fname:ident, $arith:expr) => {{
        let (book, a) = build_stack_pop!($ctx, $book);
        let (book, b) = build_stack_pop!($ctx, book);
        let d = $ctx.builder.$fname(b, a, $arith, "")?;
        build_stack_push!($ctx, book, d)
    }};
}

macro_rules! llvmnativei256_cmp {
    ($ctx:ident, $book:ident, $this:expr, $next:expr, $instructions:expr, $i:expr, $op:expr, $predicate:expr) => {{
        let (book, a) = build_stack_pop!($ctx, $book);
        let (book, b) = build_stack_pop!($ctx, book);
        let cmp = $ctx.builder.build_int_compare($predicate, a, b, "")?;

        let push_0 = JitEvmEngineSimpleBlock::new(
            &$ctx,
            $this.block,
            &format!("Instruction #{}: {:?} / push 0", $i, $op),
            &format!("_{}_0", $i),
        )?;
        let push_1 = JitEvmEngineSimpleBlock::new(
            &$ctx,
            push_0.block,
            &format!("Instruction #{}: {:?} / push 1", $i, $op),
            &format!("_{}_1", $i),
        )?;

        $ctx.builder.position_at_end($this.block);
        $ctx.builder
            .build_conditional_branch(cmp, push_1.block, push_0.block)?;
        push_0.add_incoming(&book, &$this);
        push_1.add_incoming(&book, &$this);

        $ctx.builder.position_at_end(push_0.block);
        let const_0 = $ctx.types.type_stackel.const_int(0, false);
        let book_0 = build_stack_push!($ctx, book, const_0);
        $ctx.builder.build_unconditional_branch($next.block)?;
        $next.add_incoming(&book_0, &push_0);

        $ctx.builder.position_at_end(push_1.block);
        let const_1 = $ctx.types.type_stackel.const_int(1, false);
        let book_1 = build_stack_push!($ctx, book, const_1);
        $ctx.builder.build_unconditional_branch($next.block)?;
        $next.add_incoming(&book_1, &push_1);

        continue; // skip auto-generated jump to next instruction
    }};
}

macro_rules! op_signextend {
    ($ctx:ident, $book:ident, $this:ident, $next:ident, $i:ident) => {{
        let (book, x) = build_stack_pop!($ctx, $book);

        let const_32 = $ctx.types.type_stackel.const_int(32, false);
        let cmp = $ctx
            .builder
            .build_int_compare(IntPredicate::UGE, x, const_32, "")?;

        let label = format!("Instruction #{}: Signextend / else", $i);
        let index = format!("_{}", $i);
        let else_block = JitEvmEngineSimpleBlock::new(&$ctx, $this.block, &label, &index)?;
        $ctx.builder.position_at_end($this.block);
        $ctx.builder
            .build_conditional_branch(cmp, $next.block, else_block.block)?;

        $next.add_incoming(&book, &$this);
        else_block.add_incoming(&book, &$this);

        $ctx.builder.position_at_end(else_block.block);

        let (book, y) = build_stack_pop!($ctx, book);

        let const_1 = $ctx.types.type_stackel.const_int(1, false);
        let const_7 = $ctx.types.type_stackel.const_int(7, false);
        let const_8 = $ctx.types.type_stackel.const_int(8, false);

        let x_8 = $ctx.builder.build_int_mul(x, const_8, "")?;
        let bit_index = $ctx.builder.build_int_add(x_8, const_7, "")?;
        let bit = $ctx.builder.build_left_shift(const_1, bit_index, "")?;
        let mask = $ctx.builder.build_int_sub(bit, const_1, "")?;

        let sign = $ctx.builder.build_and(y, bit, "")?;
        let is_signed = $ctx
            .builder
            .build_int_compare(IntPredicate::EQ, sign, bit, "")?;

        let not_mask = $ctx.builder.build_not(mask, "")?;
        let extended = $ctx.builder.build_or(y, not_mask, "")?;
        let unextended = $ctx.builder.build_and(y, mask, "")?;

        let result = $ctx
            .builder
            .build_select(is_signed, extended, unextended, "")?
            .into_int_value();

        build_stack_push!($ctx, book, result);
        $ctx.builder.build_unconditional_branch($next.block)?;

        continue; // skip auto-generated jump to next instruction
    }};
}

macro_rules! op2_i256_mod {
    ($ctx:ident, $book:ident, $fnname:ident) => {{
        let (book, a) = build_stack_pop!($ctx, $book);
        let (book, b) = build_stack_pop!($ctx, book);
        let (book, n) = build_stack_pop!($ctx, book);

        let width = $ctx.types.type_stackel.get_bit_width() * 2;
        let type_iup = $ctx.context.custom_width_int_type(width);

        let a_up = $ctx
            .builder
            .build_int_cast_sign_flag(a, type_iup, false, "")?;
        let b_up = $ctx
            .builder
            .build_int_cast_sign_flag(b, type_iup, false, "")?;
        let n_up = $ctx
            .builder
            .build_int_cast_sign_flag(n, type_iup, false, "")?;

        let result = $ctx.builder.$fnname(a_up, b_up, "")?;
        let result = $ctx.builder.build_int_unsigned_rem(result, n_up, "")?;

        let result = $ctx
            .builder
            .build_int_cast(result, $ctx.types.type_stackel, "")?;

        build_stack_push!($ctx, book, result)
    }};
}

macro_rules! op2_i256_exp {
    ($ctx:ident, $book:ident, $accum:ident, $base:ident, $exp:ident, $mask:ident, loop_bit) => {{
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
    ($ctx:ident, $book:ident, $this:ident, $next:ident, $i:ident) => {{
        let (book, a) = build_stack_pop!($ctx, $book);
        let (book, exp) = build_stack_pop!($ctx, book);

        let const_1 = $ctx.types.type_stackel.const_int(1, false);
        let zero = $ctx.types.type_stackel.const_int(0, false);
        let is_zero = $ctx
            .builder
            .build_int_compare(IntPredicate::EQ, exp, zero, "exp_check")?;

        let else_label = format!("Instruction #{}: Exp / else", $i);
        let then_label = format!("Instruction #{}: Exp / then", $i);
        let index = format!("_{}", $i);
        let else_block = JitEvmEngineSimpleBlock::new(&$ctx, $this.block, &else_label, &index)?;
        let then_block = JitEvmEngineSimpleBlock::new(&$ctx, $this.block, &then_label, &index)?;

        $ctx.builder.position_at_end($this.block);
        $ctx.builder
            .build_conditional_branch(is_zero, then_block.block, else_block.block)?;

        else_block.add_incoming(&book, &$this);
        then_block.add_incoming(&book, &$this);

        //// THEN BLOCK
        $ctx.builder.position_at_end(then_block.block);

        let then_book = then_block.book();
        let then_book = build_stack_push!($ctx, then_book, const_1);

        $next.add_incoming(&then_book, &then_block);

        $ctx.builder.build_unconditional_branch($next.block)?;

        //// ELSE BLOCK
        $ctx.builder.position_at_end(else_block.block);
        let else_book = else_block.book();

        let const8 = $ctx.types.type_ptrint.const_int(8, false);
        let const1 = $ctx.types.type_ptrint.const_int(1, false);
        let msbyte = build_get_msbyte!($ctx, exp);
        let bit = $ctx.builder.build_int_mul(msbyte, const8, "")?;
        let shift = $ctx.builder.build_int_sub(bit, const1, "")?;
        let shift_cast = $ctx
            .builder
            .build_int_cast(shift, $ctx.types.type_stackel, "")?;

        let mask_init = $ctx.builder.build_left_shift(const_1, shift_cast, "")?;

        let accum_init = $ctx
            .builder
            .build_bitcast(const_1, $ctx.types.type_stackel, "")?
            .into_int_value();

        let loop_label = format!("Instruction #{}: Exp / loop", $i);
        let loop_end_label = format!("Instruction #{}: Exp / loop_end", $i);

        let loop_block =
            JitEvmEngineSimpleBlock::new(&$ctx, else_block.block, &loop_label, &index)?;
        let loop_end_block =
            JitEvmEngineSimpleBlock::new(&$ctx, loop_block.block, &loop_end_label, &index)?;

        let loop_book = loop_block.book();
        let loop_end_book = loop_end_block.book();

        loop_block.add_incoming(&else_book, &else_block);
        loop_block.add_incoming(&loop_book, &loop_block);
        loop_end_block.add_incoming(&loop_book, &loop_block);

        $ctx.builder.position_at_end(else_block.block);
        $ctx.builder.build_unconditional_branch(loop_block.block)?;

        $ctx.builder.position_at_end(loop_block.block);
        let mask_phi = $ctx.builder.build_phi($ctx.types.type_stackel, "")?;
        let accum_phi = $ctx.builder.build_phi($ctx.types.type_stackel, "")?;
        accum_phi.add_incoming(&[(&accum_init, else_block.block)]);
        mask_phi.add_incoming(&[(&mask_init, else_block.block)]);
        let mut mask = mask_phi.as_basic_value().into_int_value();
        let mut accum = accum_phi.as_basic_value().into_int_value();

        op2_i256_exp!($ctx, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($ctx, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($ctx, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($ctx, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($ctx, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($ctx, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($ctx, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($ctx, loop_book, accum, a, exp, mask, loop_bit);

        mask_phi.add_incoming(&[(&mask, loop_block.block)]);
        accum_phi.add_incoming(&[(&accum, loop_block.block)]);
        let cond = $ctx
            .builder
            .build_int_compare(IntPredicate::EQ, mask, zero, "")?;
        $ctx.builder
            .build_conditional_branch(cond, loop_end_block.block, loop_block.block)?;

        $ctx.builder.position_at_end(loop_end_block.block);
        let final_accum = $ctx.builder.build_phi($ctx.types.type_stackel, "")?;
        final_accum.add_incoming(&[(&accum, loop_block.block)]);

        let final_basic = final_accum.as_basic_value().into_int_value();
        build_stack_push!($ctx, loop_end_book, final_basic);
        $ctx.builder.build_unconditional_branch($next.block)?;

        $next.add_incoming(&loop_end_book, &loop_end_block);
        continue;
    }};
}

macro_rules! llvmnativei256_op1 {
    ($ctx:ident, $book:ident, $fname:ident) => {{
        let (book, a) = build_stack_pop!($ctx, $book);
        let d = $ctx.builder.$fname(a, "")?;
        build_stack_push!($ctx, book, d)
    }};
}
