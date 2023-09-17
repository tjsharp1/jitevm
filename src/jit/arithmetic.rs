use super::JitEvmEngineSimpleBlock;

macro_rules! op1_llvmnativei256_iszero {
    ($self:ident, $book:ident, $this:expr, $next:expr, $op:expr, $i:expr) => {{
        // TODO: this can also be optimized...
        let (book, val) = $self.build_stack_pop($book)?;
        let cmp = $self.builder.build_int_compare(
            IntPredicate::EQ,
            $self.type_stackel.const_int(0, false),
            val,
            "",
        )?;

        let push_0 = JitEvmEngineSimpleBlock::new(
            &$self,
            $this.block,
            &format!("Instruction #{}: {:?} / push 0", $i, $op),
            &format!("_{}_0", $i),
        )?;
        let push_1 = JitEvmEngineSimpleBlock::new(
            &$self,
            push_0.block,
            &format!("Instruction #{}: {:?} / push 1", $i, $op),
            &format!("_{}_1", $i),
        )?;

        $self.builder.position_at_end($this.block);
        $self
            .builder
            .build_conditional_branch(cmp, push_1.block, push_0.block)?;
        push_0.add_incoming(&book, &$this);
        push_1.add_incoming(&book, &$this);

        $self.builder.position_at_end(push_0.block);
        let book_0 = $self.build_stack_push(book, $self.type_stackel.const_int(0, false))?;
        $self.builder.build_unconditional_branch($next.block)?;
        $next.add_incoming(&book_0, &push_0);

        $self.builder.position_at_end(push_1.block);
        let book_1 = $self.build_stack_push(book, $self.type_stackel.const_int(1, false))?;
        $self.builder.build_unconditional_branch($next.block)?;
        $next.add_incoming(&book_1, &push_1);

        continue; // skip auto-generated jump to next instruction
    }};
}

macro_rules! op2_llvmnativei256_operation {
    ($self:ident, $book:ident, $fname:ident) => {{
        let (book, a) = $self.build_stack_pop($book)?;
        let (book, b) = $self.build_stack_pop(book)?;
        let d = $self.builder.$fname(a, b, "")?;
        let book = $self.build_stack_push(book, d)?;
        book
    }};
}

macro_rules! op2_llvmnativei256_compare_operation {
    ($self:ident, $book:ident, $this:expr, $next:expr, $instructions:expr, $i:expr, $op:expr, $predicate:expr) => {{
        let (book, a) = $self.build_stack_pop($book)?;
        let (book, b) = $self.build_stack_pop(book)?;
        let cmp = $self.builder.build_int_compare($predicate, a, b, "")?;

        let push_0 = JitEvmEngineSimpleBlock::new(
            &$self,
            $this.block,
            &format!("Instruction #{}: {:?} / push 0", $i, $op),
            &format!("_{}_0", $i),
        )?;
        let push_1 = JitEvmEngineSimpleBlock::new(
            &$self,
            push_0.block,
            &format!("Instruction #{}: {:?} / push 1", $i, $op),
            &format!("_{}_1", $i),
        )?;

        $self.builder.position_at_end($this.block);
        $self
            .builder
            .build_conditional_branch(cmp, push_1.block, push_0.block)?;
        push_0.add_incoming(&book, &$this);
        push_1.add_incoming(&book, &$this);

        $self.builder.position_at_end(push_0.block);
        let book_0 = $self.build_stack_push(book, $self.type_stackel.const_int(0, false))?;
        $self.builder.build_unconditional_branch($next.block)?;
        $next.add_incoming(&book_0, &push_0);

        $self.builder.position_at_end(push_1.block);
        let book_1 = $self.build_stack_push(book, $self.type_stackel.const_int(1, false))?;
        $self.builder.build_unconditional_branch($next.block)?;
        $next.add_incoming(&book_1, &push_1);

        continue; // skip auto-generated jump to next instruction
    }};
}

macro_rules! op2_i256_exp {
    ($self:ident, $book:ident, $accum:ident, $base:ident, $exp:ident, $mask:ident, loop_bit) => {{
        let one = $self.type_stackel.const_int(1, false);
        let zero = $self.type_stackel.const_int(0, false);

        let bit = $self.builder.build_and($exp, $mask, "")?;
        let is_zero_bit = $self
            .builder
            .build_int_compare(IntPredicate::EQ, bit, zero, "")?;
        $mask = $self.builder.build_right_shift($mask, one, false, "")?;

        $accum = $self.builder.build_int_mul($accum, $accum, "")?;
        let mul = $self
            .builder
            .build_select(is_zero_bit, one, $base, "")?
            .into_int_value();
        $accum = $self.builder.build_int_mul($accum, mul, "")?;
    }};
    ($self:ident, $book:ident, $this:ident, $next:ident, $i:ident) => {{
        let (book, a) = $self.build_stack_pop($book)?;
        let (book, exp) = $self.build_stack_pop(book)?;

        let const_1 = $self.type_stackel.const_int(1, false);
        let zero = $self.type_stackel.const_int(0, false);
        let is_zero = $self
            .builder
            .build_int_compare(IntPredicate::EQ, exp, zero, "exp_check")?;

        let else_label = format!("Instruction #{}: Exp / else", $i);
        let then_label = format!("Instruction #{}: Exp / then", $i);
        let index = format!("_{}", $i);
        let else_block = JitEvmEngineSimpleBlock::new(&$self, $this.block, &else_label, &index)?;
        let then_block = JitEvmEngineSimpleBlock::new(&$self, $this.block, &then_label, &index)?;

        $self.builder.position_at_end($this.block);
        $self
            .builder
            .build_conditional_branch(is_zero, then_block.block, else_block.block)?;

        else_block.add_incoming(&book, &$this);
        then_block.add_incoming(&book, &$this);

        //// THEN BLOCK
        $self.builder.position_at_end(then_block.block);

        let then_book = then_block.book();
        let then_book = $self.build_stack_push(then_book, const_1)?;

        $next.add_incoming(&then_book, &then_block);

        $self.builder.build_unconditional_branch($next.block)?;

        //// ELSE BLOCK
        $self.builder.position_at_end(else_block.block);
        let else_book = else_block.book();

        let const8 = $self.type_ptrint.const_int(8, false);
        let const1 = $self.type_ptrint.const_int(1, false);
        let msbyte = $self.build_get_msbyte(exp)?;
        let bit = $self.builder.build_int_mul(msbyte, const8, "")?;
        let shift = $self.builder.build_int_sub(bit, const1, "")?;
        let shift_cast = $self
            .builder
            .build_int_cast(shift, $self.type_stackel, "")?;

        let mask_init = $self.builder.build_left_shift(const_1, shift_cast, "")?;

        let accum_init = $self
            .builder
            .build_bitcast(const_1, $self.type_stackel, "")?
            .into_int_value();

        let loop_label = format!("Instruction #{}: Exp / loop", $i);
        let loop_end_label = format!("Instruction #{}: Exp / loop_end", $i);

        let loop_block =
            JitEvmEngineSimpleBlock::new(&$self, else_block.block, &loop_label, &index)?;
        let loop_end_block =
            JitEvmEngineSimpleBlock::new(&$self, loop_block.block, &loop_end_label, &index)?;

        let loop_book = loop_block.book();
        let loop_end_book = loop_end_block.book();

        loop_block.add_incoming(&else_book, &else_block);
        loop_block.add_incoming(&loop_book, &loop_block);
        loop_end_block.add_incoming(&loop_book, &loop_block);

        $self.builder.position_at_end(else_block.block);
        $self.builder.build_unconditional_branch(loop_block.block)?;

        $self.builder.position_at_end(loop_block.block);
        let mask_phi = $self.builder.build_phi($self.type_stackel, "")?;
        let accum_phi = $self.builder.build_phi($self.type_stackel, "")?;
        accum_phi.add_incoming(&[(&accum_init, else_block.block)]);
        mask_phi.add_incoming(&[(&mask_init, else_block.block)]);
        let mut mask = mask_phi.as_basic_value().into_int_value();
        let mut accum = accum_phi.as_basic_value().into_int_value();

        op2_i256_exp!($self, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($self, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($self, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($self, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($self, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($self, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($self, loop_book, accum, a, exp, mask, loop_bit);
        op2_i256_exp!($self, loop_book, accum, a, exp, mask, loop_bit);

        mask_phi.add_incoming(&[(&mask, loop_block.block)]);
        accum_phi.add_incoming(&[(&accum, loop_block.block)]);
        let cond = $self
            .builder
            .build_int_compare(IntPredicate::EQ, mask, zero, "")?;
        $self
            .builder
            .build_conditional_branch(cond, loop_end_block.block, loop_block.block)?;

        $self.builder.position_at_end(loop_end_block.block);
        let final_accum = $self.builder.build_phi($self.type_stackel, "")?;
        final_accum.add_incoming(&[(&accum, loop_block.block)]);

        $self.build_stack_push(loop_end_book, final_accum.as_basic_value().into_int_value())?;
        $self.builder.build_unconditional_branch($next.block)?;

        $next.add_incoming(&loop_end_book, &loop_end_block);
    }};
}

macro_rules! op1_llvmnativei256_operation {
    ($self:ident, $book:ident, $fname:ident) => {{
        let (book, a) = $self.build_stack_pop($book)?;
        let d = $self.builder.$fname(a, "")?;
        let book = $self.build_stack_push(book, d)?;
        book
    }};
}
