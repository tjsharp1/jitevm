macro_rules! build_jump {
    ($ctx:ident, $book:ident, $code:ident, $this:ident, $end:ident, $instructions:ident, $error_jumpdest:ident, $i:ident, $op:ident) => {{
		let (book, target) = build_stack_pop!($ctx, $book);

		if $code.jumpdests.is_empty() {
			// there are no valid jump targets, this Jump has to fail!
			// TODO: should this be the error block?
			$ctx.builder.build_unconditional_branch($end.block)?;
			$end.add_incoming(&book, &$this);
		} else {
			let mut jump_table: Vec<JitEvmEngineSimpleBlock<'_>> = Vec::new();
			for (j, jmp_i) in $code.jumpdests.iter().enumerate() {
				let jmp_target = $code.opidx2target[jmp_i];
				jump_table.push(JitEvmEngineSimpleBlock::new(
					&$ctx,
					if j == 0 {
						$this.block
					} else {
						jump_table[j - 1].block
					},
					&format!(
						"instruction #{}: {:?} / to Jumpdest #{} at op #{} to byte #{}",
						$i, $op, j, jmp_i, jmp_target
					),
					&format!("_{}_{}", $i, j),
				)?);
			}

			$ctx.builder.position_at_end($this.block);
			$ctx.builder
				.build_unconditional_branch(jump_table[0].block)?;
			jump_table[0].add_incoming(&book, &$this);

			for (j, jmp_i) in $code.jumpdests.iter().enumerate() {
				let jmp_target = $code.opidx2target[jmp_i];
				let jmp_target = jmp_target.as_u64(); // REMARK: assumes that code cannot exceed 2^64 instructions, probably ok ;)
				$ctx.builder.position_at_end(jump_table[j].block);
				let cmp = $ctx.builder.build_int_compare(
					IntPredicate::EQ,
					$ctx.types.type_stackel.const_int(jmp_target, false),
					target,
					"",
				)?;
				if j + 1 == $code.jumpdests.len() {
					$ctx.builder.build_conditional_branch(
						cmp,
						$instructions[*jmp_i].block,
						$error_jumpdest.block,
					)?;
					$instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
					$error_jumpdest.add_incoming(&book, &jump_table[j]);
				} else {
					$ctx.builder.build_conditional_branch(
						cmp,
						$instructions[*jmp_i].block,
						jump_table[j + 1].block,
					)?;
					$instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
					jump_table[j + 1].add_incoming(&book, &jump_table[j]);
				}
			}
		}

		continue; // skip auto-generated jump to next instruction
	}};
}

macro_rules! build_jumpi {
    ($ctx:ident, $book:ident, $code:ident, $this:ident, $next:ident, $end:ident, $instructions:ident, $error_jumpdest:ident, $i:ident, $op:ident) => {{
		let (book, target) = build_stack_pop!($ctx, $book);
		let (book, val) = build_stack_pop!($ctx, book);

		if $code.jumpdests.is_empty() {
			// there are no valid jump targets, this Jumpi has to fail!
			$ctx.builder.build_unconditional_branch($end.block)?;
			$end.add_incoming(&book, &$this);
		} else {
			let mut jump_table: Vec<JitEvmEngineSimpleBlock<'_>> = Vec::new();
			for (j, jmp_i) in $code.jumpdests.iter().enumerate() {
				let jmp_target = $code.opidx2target[jmp_i];
				jump_table.push(JitEvmEngineSimpleBlock::new(
					&$ctx,
					if j == 0 {
						$this.block
					} else {
						jump_table[j - 1].block
					},
					&format!(
						"instruction #{}: {:?} / to Jumpdest #{} at op #{} to byte #{}",
						$i, $op, j, jmp_i, jmp_target
					),
					&format!("_{}_{}", $i, j),
				)?);
			}

			$ctx.builder.position_at_end($this.block);
			let cmp = $ctx.builder.build_int_compare(
				IntPredicate::EQ,
				$ctx.types.type_stackel.const_int(0, false),
				val,
				"",
			)?;
			$ctx.builder.build_conditional_branch(
				cmp,
				$next.block,
				jump_table[0].block,
			)?;
			$next.add_incoming(&book, &$this);
			jump_table[0].add_incoming(&book, &$this);

			for (j, jmp_i) in $code.jumpdests.iter().enumerate() {
				let jmp_target = $code.opidx2target[jmp_i];
				let jmp_target = jmp_target.as_u64(); // REMARK: assumes that code cannot exceed 2^64 instructions, probably ok ;)
				$ctx.builder.position_at_end(jump_table[j].block);
				let cmp = $ctx.builder.build_int_compare(
					IntPredicate::EQ,
					$ctx.types.type_stackel.const_int(jmp_target, false),
					target,
					"",
				)?;
				if j + 1 == $code.jumpdests.len() {
					$ctx.builder.build_conditional_branch(
						cmp,
						$instructions[*jmp_i].block,
						$error_jumpdest.block,
					)?;
					$instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
					$error_jumpdest.add_incoming(&book, &jump_table[j]);
				} else {
					$ctx.builder.build_conditional_branch(
						cmp,
						$instructions[*jmp_i].block,
						jump_table[j + 1].block,
					)?;
					$instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
					jump_table[j + 1].add_incoming(&book, &jump_table[j]);
				}
			}
		}

		continue; // skip auto-generated jump to next instruction
	}};
}

macro_rules! build_augmented_jump {
    ($ctx:ident, $book:ident, $code:ident, $this:ident, $end:ident, $instructions:ident, $val:ident) => {{
        if $code.jumpdests.is_empty() {
            // there are no valid jump targets, this Jump has to fail!
            $ctx.builder.build_unconditional_branch($end.block)?;
            $end.add_incoming(&$book, &$this);
        } else {
            // retrieve the corresponding jump target (panic if not a valid jump target) ...
            let jmp_i = $code.target2opidx[$val];
            // ... and jump to there!
            $ctx.builder
                .build_unconditional_branch($instructions[jmp_i].block)?;
            $instructions[jmp_i].add_incoming(&$book, &$this);
        }

        continue; // skip auto-generated jump to next instruction
    }};
}

macro_rules! build_augmented_jumpi {
    ($ctx:ident, $book:ident, $code:ident, $this:ident, $next:ident, $end:ident, $instructions:ident, $val:ident) => {{
        let (book, condition) = build_stack_pop!($ctx, $book);

        if $code.jumpdests.is_empty() {
            // there are no valid jump targets, this Jumpi has to fail!
            $ctx.builder.build_unconditional_branch($end.block)?;
            $end.add_incoming(&book, &$this);
        } else {
            // retrieve the corresponding jump target (panic if not a valid jump target) ...
            let jmp_i = $code.target2opidx[$val];
            // ... and jump to there (conditionally)!
            let cmp = $ctx.builder.build_int_compare(
                IntPredicate::EQ,
                $ctx.types.type_stackel.const_int(0, false),
                condition,
                "",
            )?;
            $ctx.builder
                .build_conditional_branch(cmp, $next.block, $instructions[jmp_i].block)?;
            $next.add_incoming(&book, &$this);
            $instructions[jmp_i].add_incoming(&book, &$this);
        }

        continue; // skip auto-generated jump to next instruction
    }};
}
