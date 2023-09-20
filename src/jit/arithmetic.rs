use crate::jit::stack::{build_stack_pop, build_stack_push};

macro_rules! op1_llvmnativei256_iszero {
    ($context:ident, $types:ident, $builder:ident, $book:ident, $this:expr, $next:expr, $op:expr, $i:expr) => {{
        // TODO: this can also be optimized...
        let (book, val) = build_stack_pop!($types, $builder, $book);
        let cmp = $builder.build_int_compare(
            IntPredicate::EQ,
            $types.type_stackel.const_int(0, false),
            val,
            "",
        )?;

        let push_0 = JitEvmEngineSimpleBlock::new(
            $context,
            &$builder,
            $this.block,
            &format!("Instruction #{}: {:?} / push 0", $i, $op),
            &format!("_{}_0", $i),
        )?;
        let push_1 = JitEvmEngineSimpleBlock::new(
            $context,
            &$builder,
            push_0.block,
            &format!("Instruction #{}: {:?} / push 1", $i, $op),
            &format!("_{}_1", $i),
        )?;

        $builder.position_at_end($this.block);
        $builder.build_conditional_branch(cmp, push_1.block, push_0.block)?;
        push_0.add_incoming(&book, &$this);
        push_1.add_incoming(&book, &$this);

        $builder.position_at_end(push_0.block);
        let const_0 = $types.type_stackel.const_int(0, false);
        let book_0 = build_stack_push!($types, $builder, book, const_0);
        $builder.build_unconditional_branch($next.block)?;
        $next.add_incoming(&book_0, &push_0);

        $builder.position_at_end(push_1.block);
        let const_1 = $types.type_stackel.const_int(1, false);
        let book_1 = build_stack_push!($types, $builder, book, const_1);
        $builder.build_unconditional_branch($next.block)?;
        $next.add_incoming(&book_1, &push_1);

        continue; // skip auto-generated jump to next instruction
    }};
}

macro_rules! llvmnativei256_op2 {
    ($types:ident, $builder:ident, $book:ident, $fname:ident) => {{
        let (book, a) = build_stack_pop!($types, $builder, $book);
        let (book, b) = build_stack_pop!($types, $builder, book);
        let d = $builder.$fname(a, b, "")?;
        build_stack_push!($types, $builder, book, d)
    }};
}

macro_rules! llvmnativei256_lshift {
    ($types:ident, $builder:ident, $book:ident, $fname:ident) => {{
        let (book, a) = build_stack_pop!($types, $builder, $book);
        let (book, b) = build_stack_pop!($types, $builder, book);
        let d = $builder.$fname(b, a, "")?;
        build_stack_push!($types, $builder, book, d)
    }};
}

macro_rules! llvmnativei256_rshift {
    ($types:ident, $builder:ident, $book:ident, $fname:ident, $arith:expr) => {{
        let (book, a) = build_stack_pop!($types, $builder, $book);
        let (book, b) = build_stack_pop!($types, $builder, book);
        let d = $builder.$fname(b, a, $arith, "")?;
        build_stack_push!($types, $builder, book, d)
    }};
}

macro_rules! llvmnativei256_compare {
    ($context:ident, $types:ident, $builder:ident, $book:ident, $this:expr, $next:expr, $instructions:expr, $i:expr, $op:expr, $predicate:expr) => {{
        let (book, a) = build_stack_pop!($types, $builder, $book);
        let (book, b) = build_stack_pop!($types, $builder, book);
        let cmp = $builder.build_int_compare($predicate, a, b, "")?;

        let push_0 = JitEvmEngineSimpleBlock::new(
            $context,
            &$builder,
            $this.block,
            &format!("Instruction #{}: {:?} / push 0", $i, $op),
            &format!("_{}_0", $i),
        )?;
        let push_1 = JitEvmEngineSimpleBlock::new(
            $context,
            &$builder,
            push_0.block,
            &format!("Instruction #{}: {:?} / push 1", $i, $op),
            &format!("_{}_1", $i),
        )?;

        $builder.position_at_end($this.block);
        $builder.build_conditional_branch(cmp, push_1.block, push_0.block)?;
        push_0.add_incoming(&book, &$this);
        push_1.add_incoming(&book, &$this);

        $builder.position_at_end(push_0.block);
        let const_0 = $types.type_stackel.const_int(0, false);
        let book_0 = build_stack_push!($types, $builder, book, const_0);
        $builder.build_unconditional_branch($next.block)?;
        $next.add_incoming(&book_0, &push_0);

        $builder.position_at_end(push_1.block);
        let const_1 = $types.type_stackel.const_int(1, false);
        let book_1 = build_stack_push!($types, $builder, book, const_1);
        $builder.build_unconditional_branch($next.block)?;
        $next.add_incoming(&book_1, &push_1);

        continue; // skip auto-generated jump to next instruction
    }};
}

macro_rules! op2_i256_exp {
    ($types:ident, $builder:ident, $book:ident, $accum:ident, $base:ident, $exp:ident, $mask:ident, loop_bit) => {{
        let one = $types.type_stackel.const_int(1, false);
        let zero = $types.type_stackel.const_int(0, false);

        let bit = $builder.build_and($exp, $mask, "")?;
        let is_zero_bit = $builder.build_int_compare(IntPredicate::EQ, bit, zero, "")?;
        $mask = $builder.build_right_shift($mask, one, false, "")?;

        $accum = $builder.build_int_mul($accum, $accum, "")?;
        let mul = $builder
            .build_select(is_zero_bit, one, $base, "")?
            .into_int_value();
        $accum = $builder.build_int_mul($accum, mul, "")?;
    }};
    ($context:ident, $types:ident, $builder:ident, $book:ident, $this:ident, $next:ident, $i:ident) => {{
        let (book, a) = build_stack_pop!($types, $builder, $book);
        let (book, exp) = build_stack_pop!($types, $builder, book);

        let const_1 = $types.type_stackel.const_int(1, false);
        let zero = $types.type_stackel.const_int(0, false);
        let is_zero = $builder.build_int_compare(IntPredicate::EQ, exp, zero, "exp_check")?;

        let else_label = format!("Instruction #{}: Exp / else", $i);
        let then_label = format!("Instruction #{}: Exp / then", $i);
        let index = format!("_{}", $i);
        let else_block =
            JitEvmEngineSimpleBlock::new($context, &$builder, $this.block, &else_label, &index)?;
        let then_block =
            JitEvmEngineSimpleBlock::new($context, &$builder, $this.block, &then_label, &index)?;

        $builder.position_at_end($this.block);
        $builder.build_conditional_branch(is_zero, then_block.block, else_block.block)?;

        else_block.add_incoming(&book, &$this);
        then_block.add_incoming(&book, &$this);

        //// THEN BLOCK
        $builder.position_at_end(then_block.block);

        let then_book = then_block.book();
        let then_book = build_stack_push!($types, $builder, then_book, const_1);

        $next.add_incoming(&then_book, &then_block);

        $builder.build_unconditional_branch($next.block)?;

        //// ELSE BLOCK
        $builder.position_at_end(else_block.block);
        let else_book = else_block.book();

        let const8 = $types.type_ptrint.const_int(8, false);
        let const1 = $types.type_ptrint.const_int(1, false);
        let msbyte = build_get_msbyte!($types, $builder, exp);
        let bit = $builder.build_int_mul(msbyte, const8, "")?;
        let shift = $builder.build_int_sub(bit, const1, "")?;
        let shift_cast = $builder.build_int_cast(shift, $types.type_stackel, "")?;

        let mask_init = $builder.build_left_shift(const_1, shift_cast, "")?;

        let accum_init = $builder
            .build_bitcast(const_1, $types.type_stackel, "")?
            .into_int_value();

        let loop_label = format!("Instruction #{}: Exp / loop", $i);
        let loop_end_label = format!("Instruction #{}: Exp / loop_end", $i);

        let loop_block = JitEvmEngineSimpleBlock::new(
            $context,
            &$builder,
            else_block.block,
            &loop_label,
            &index,
        )?;
        let loop_end_block = JitEvmEngineSimpleBlock::new(
            $context,
            &$builder,
            loop_block.block,
            &loop_end_label,
            &index,
        )?;

        let loop_book = loop_block.book();
        let loop_end_book = loop_end_block.book();

        loop_block.add_incoming(&else_book, &else_block);
        loop_block.add_incoming(&loop_book, &loop_block);
        loop_end_block.add_incoming(&loop_book, &loop_block);

        $builder.position_at_end(else_block.block);
        $builder.build_unconditional_branch(loop_block.block)?;

        $builder.position_at_end(loop_block.block);
        let mask_phi = $builder.build_phi($types.type_stackel, "")?;
        let accum_phi = $builder.build_phi($types.type_stackel, "")?;
        accum_phi.add_incoming(&[(&accum_init, else_block.block)]);
        mask_phi.add_incoming(&[(&mask_init, else_block.block)]);
        let mut mask = mask_phi.as_basic_value().into_int_value();
        let mut accum = accum_phi.as_basic_value().into_int_value();

        op2_i256_exp!($types, $builder, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($types, $builder, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($types, $builder, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($types, $builder, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($types, $builder, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($types, $builder, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($types, $builder, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($types, $builder, loop_book, accum, a, exp, mask, loop_bit);

        mask_phi.add_incoming(&[(&mask, loop_block.block)]);
        accum_phi.add_incoming(&[(&accum, loop_block.block)]);
        let cond = $builder.build_int_compare(IntPredicate::EQ, mask, zero, "")?;
        $builder.build_conditional_branch(cond, loop_end_block.block, loop_block.block)?;

        $builder.position_at_end(loop_end_block.block);
        let final_accum = $builder.build_phi($types.type_stackel, "")?;
        final_accum.add_incoming(&[(&accum, loop_block.block)]);

        let final_basic = final_accum.as_basic_value().into_int_value();
        build_stack_push!($types, $builder, loop_end_book, final_basic);
        $builder.build_unconditional_branch($next.block)?;

        $next.add_incoming(&loop_end_book, &loop_end_block);
        continue;
    }};
}

macro_rules! llvmnativei256_op1 {
    ($types:ident, $builder:ident, $book:ident, $fname:ident) => {{
        let (book, a) = build_stack_pop!($types, $builder, $book);
        let d = $builder.$fname(a, "")?;
        build_stack_push!($types, $builder, book, d)
    }};
}
