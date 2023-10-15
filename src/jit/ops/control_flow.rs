use crate::jit::ops::stack::{build_stack_check, build_stack_pop};
use crate::jit::{
    context::JitContractResultCode,
    contract::{JitEvmEngineSimpleBlock, OperationsContext},
    cursor::CurrentInstruction,
    JitContractExecutionResult, JitEvmEngineError,
};
use inkwell::{AddressSpace, IntPredicate};
use primitive_types::U256;

pub(crate) fn build_stop_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    JitContractExecutionResult::build_exit_success(
        &ctx,
        current.block(),
        JitContractResultCode::SuccessStop,
    )?;
    Ok(())
}

pub(crate) fn build_jumpdest_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let book = current.book();
    ctx.builder
        .build_unconditional_branch(current.next().block)?;
    current.next().add_incoming(&book, current.block());
    Ok(())
}

pub(crate) fn build_jump_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 1, 0);

    let book = current.book();
    let code = current.code();
    let this = current.block();

    let (book, target) = build_stack_pop!(ctx, book);

    if code.jumpdests.is_empty() {
        return Err(JitEvmEngineError::NoValidJumpDestinations(current.idx()));
    } else {
        let mut jump_table: Vec<JitEvmEngineSimpleBlock<'_>> = Vec::new();
        for (j, jmp_i) in code.jumpdests.iter().enumerate() {
            let jmp_target = code.opidx2target[jmp_i];
            jump_table.push(JitEvmEngineSimpleBlock::new(
                &ctx,
                if j == 0 {
                    this.block
                } else {
                    jump_table[j - 1].block
                },
                &format!(
                    "instruction #{}: {:?} / to Jumpdest #{} at op #{} to byte #{}",
                    current.idx(),
                    current.op(),
                    j,
                    jmp_i,
                    jmp_target
                ),
                &format!("_{}_{}", current.idx(), j),
            )?);
        }

        ctx.builder.position_at_end(this.block);
        ctx.builder
            .build_unconditional_branch(jump_table[0].block)?;
        jump_table[0].add_incoming(&book, this);

        let invalid_jump = u32::from(JitContractResultCode::InvalidJump);
        let invalid_jump = ctx.types.type_i32.const_int(invalid_jump as u64, false);

        let instructions = current.instructions();
        for (j, jmp_i) in code.jumpdests.iter().enumerate() {
            let jmp_target = code.opidx2target[jmp_i];
            let jmp_target = jmp_target.as_u64(); // REMARK: assumes that code cannot exceed 2^64 instructions, probably ok ;)
            ctx.builder.position_at_end(jump_table[j].block);
            let cmp = ctx.builder.build_int_compare(
                IntPredicate::EQ,
                ctx.types.type_stackel.const_int(jmp_target, false),
                target,
                "",
            )?;
            if j + 1 == code.jumpdests.len() {
                ctx.builder.build_conditional_branch(
                    cmp,
                    instructions[*jmp_i].block,
                    current.error_block(),
                )?;
                instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
                current.incoming_error(&book, &jump_table[j], invalid_jump);
            } else {
                ctx.builder.build_conditional_branch(
                    cmp,
                    instructions[*jmp_i].block,
                    jump_table[j + 1].block,
                )?;
                instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
                jump_table[j + 1].add_incoming(&book, &jump_table[j]);
            }
        }
    }

    Ok(())
}

pub(crate) fn build_jumpi_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 2, 0);

    let book = current.book();
    let code = current.code();
    let this = current.block();
    let next = current.next();

    let (book, target) = build_stack_pop!(ctx, book);
    let (book, val) = build_stack_pop!(ctx, book);

    if code.jumpdests.is_empty() {
        return Err(JitEvmEngineError::NoValidJumpDestinations(current.idx()));
    } else {
        let mut jump_table: Vec<JitEvmEngineSimpleBlock<'_>> = Vec::new();
        for (j, jmp_i) in code.jumpdests.iter().enumerate() {
            let jmp_target = code.opidx2target[jmp_i];
            jump_table.push(JitEvmEngineSimpleBlock::new(
                ctx,
                if j == 0 {
                    this.block
                } else {
                    jump_table[j - 1].block
                },
                &format!(
                    "instruction #{}: {:?} / to Jumpdest #{} at op #{} to byte #{}",
                    current.idx(),
                    current.op(),
                    j,
                    jmp_i,
                    jmp_target
                ),
                &format!("_{}_{}", current.idx(), j),
            )?);
        }

        ctx.builder.position_at_end(this.block);
        let cmp = ctx.builder.build_int_compare(
            IntPredicate::EQ,
            ctx.types.type_stackel.const_int(0, false),
            val,
            "",
        )?;
        ctx.builder
            .build_conditional_branch(cmp, next.block, jump_table[0].block)?;
        next.add_incoming(&book, this);
        jump_table[0].add_incoming(&book, this);

        let invalid_jump = u32::from(JitContractResultCode::InvalidJump);
        let invalid_jump = ctx.types.type_i32.const_int(invalid_jump as u64, false);
        let instructions = current.instructions();
        for (j, jmp_i) in code.jumpdests.iter().enumerate() {
            let jmp_target = code.opidx2target[jmp_i];
            let jmp_target = jmp_target.as_u64(); // REMARK: assumes that code cannot exceed 2^64 instructions, probably ok ;)
            ctx.builder.position_at_end(jump_table[j].block);
            let cmp = ctx.builder.build_int_compare(
                IntPredicate::EQ,
                ctx.types.type_stackel.const_int(jmp_target, false),
                target,
                "",
            )?;
            if j + 1 == code.jumpdests.len() {
                ctx.builder.build_conditional_branch(
                    cmp,
                    instructions[*jmp_i].block,
                    current.error_block(),
                )?;
                instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
                current.incoming_error(&book, &jump_table[j], invalid_jump);
            } else {
                ctx.builder.build_conditional_branch(
                    cmp,
                    instructions[*jmp_i].block,
                    jump_table[j + 1].block,
                )?;
                instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
                jump_table[j + 1].add_incoming(&book, &jump_table[j]);
            }
        }
    }

    Ok(())
}

pub(crate) fn build_augmented_jump_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &CurrentInstruction<'a, 'ctx>,
    val: U256,
) -> Result<(), JitEvmEngineError> {
    let book = current.book();
    let code = current.code();
    let this = current.block();
    let instructions = current.instructions();

    if code.jumpdests.is_empty() {
        return Err(JitEvmEngineError::NoValidJumpDestinations(current.idx()));
    } else {
        // TODO: need to insert a run-time check if the target is a valid jump-dest!!
        // retrieve the corresponding jump target (panic if not a valid jump target) ...
        let jmp_i = code.target2opidx[&val];
        // ... and jump to there!
        ctx.builder
            .build_unconditional_branch(instructions[jmp_i].block)?;
        instructions[jmp_i].add_incoming(&book, this);
    }
    Ok(())
}

pub(crate) fn build_augmented_jumpi_op<'a, 'ctx>(
    ctx: &OperationsContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
    val: U256,
) -> Result<(), JitEvmEngineError> {
    build_stack_check!(ctx, current, 1, 0);

    let book = current.book();
    let code = current.code();
    let this = current.block();
    let next = current.next();
    let instructions = current.instructions();

    let (book, condition) = build_stack_pop!(ctx, book);

    if code.jumpdests.is_empty() {
        return Err(JitEvmEngineError::NoValidJumpDestinations(current.idx()));
    } else {
        // TODO: need to insert a run-time check if the target is a valid jump-dest!!
        // retrieve the corresponding jump target (panic if not a valid jump target) ...
        let jmp_i = code.target2opidx[&val];
        // ... and jump to there (conditionally)!
        let cmp = ctx.builder.build_int_compare(
            IntPredicate::EQ,
            ctx.types.type_stackel.const_int(0, false),
            condition,
            "",
        )?;
        ctx.builder
            .build_conditional_branch(cmp, next.block, instructions[jmp_i].block)?;
        next.add_incoming(&book, this);
        instructions[jmp_i].add_incoming(&book, this);
    }

    Ok(())
}
