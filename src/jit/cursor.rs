use crate::jit::{
    context::{JitContractExecutionResult, JitContractResultCode, TransactionContext},
    contract::{BuilderContext, JitEvmEngineBookkeeping, JitEvmEngineSimpleBlock},
    EvmOp, IndexedEvmCode, JitEvmEngineError, EVM_STACK_ELEMENT_SIZE, EVM_STACK_SIZE,
};
use inkwell::AddressSpace;
use inkwell::{basic_block::BasicBlock, values::IntValue};

pub trait LendingIteratorLifetime<'this, ImplicitBounds: Sealed = Bounds<&'this Self>> {
    type Item;
}

mod sealed {
    pub trait Sealed: Sized {}
    pub struct Bounds<T>(T);
    impl<T> Sealed for Bounds<T> {}
}
use sealed::{Bounds, Sealed};

pub trait LendingIterator: for<'this> LendingIteratorLifetime<'this> {
    fn next(&mut self) -> Option<<Self as LendingIteratorLifetime<'_>>::Item>;
}

pub(crate) struct CurrentInstruction<'a, 'ctx> {
    idx: usize,
    current_block: Option<JitEvmEngineSimpleBlock<'ctx>>,
    render: &'a InstructionCursor<'ctx>,
}

impl<'a, 'ctx> CurrentInstruction<'a, 'ctx> {
    pub fn step(&mut self) -> bool {
        self.current_block = None;
        self.idx += 1;
        self.idx < self.render.instructions.len()
    }

    pub fn idx(&self) -> usize {
        self.idx
    }

    pub fn block(&self) -> &JitEvmEngineSimpleBlock<'ctx> {
        self.current_block
            .as_ref()
            .unwrap_or(&self.render.instructions[self.idx])
    }

    pub fn update_current_block(&mut self, block: JitEvmEngineSimpleBlock<'ctx>) {
        self.current_block = Some(block);
    }

    pub fn next(&self) -> &JitEvmEngineSimpleBlock<'ctx> {
        let ops_len = self.render.instructions.len();

        if self.idx + 1 == ops_len {
            &self.render.end_block
        } else {
            &self.render.instructions[self.idx + 1]
        }
    }

    pub fn op(&self) -> EvmOp {
        self.render.code.code.ops[self.idx]
    }

    pub fn book(&self) -> JitEvmEngineBookkeeping<'ctx> {
        self.block().book()
    }

    pub fn code(&self) -> &IndexedEvmCode {
        &self.render.code
    }

    pub fn instructions(&self) -> &Vec<JitEvmEngineSimpleBlock<'ctx>> {
        &self.render.instructions
    }

    pub fn incoming_error(
        &self,
        book: &JitEvmEngineBookkeeping<'ctx>,
        block: &JitEvmEngineSimpleBlock<'ctx>,
        code: IntValue<'ctx>,
    ) {
        self.render.error_block.add_incoming(book, block);
        self.render
            .error_block
            .phi_error
            .expect("Should only be called on an error block.")
            .add_incoming(&[(&code, block.block)]);
    }

    pub fn error_block(&self) -> BasicBlock<'ctx> {
        self.render.error_block.block
    }
}

pub(crate) struct Iter<'a, 'ctx> {
    init: bool,
    current: CurrentInstruction<'a, 'ctx>,
}

impl<'a, 'ctx> Iter<'a, 'ctx> {
    pub fn new(render: &'a InstructionCursor<'ctx>) -> Self {
        Self {
            init: true,
            current: CurrentInstruction {
                idx: 0,
                current_block: None,
                render: &render,
            },
        }
    }
}

impl<'this, 'a, 'ctx> LendingIteratorLifetime<'this> for Iter<'a, 'ctx> {
    type Item = &'this mut CurrentInstruction<'a, 'ctx>;
}

impl<'a, 'ctx> LendingIterator for Iter<'a, 'ctx> {
    fn next(&mut self) -> Option<<Self as LendingIteratorLifetime<'_>>::Item> {
        if self.init {
            self.init = false;
            Some(&mut self.current)
        } else {
            if self.current.step() {
                Some(&mut self.current)
            } else {
                None
            }
        }
    }
}

pub(crate) struct InstructionCursor<'ctx> {
    code: IndexedEvmCode,
    instructions: Vec<JitEvmEngineSimpleBlock<'ctx>>,
    end_block: JitEvmEngineSimpleBlock<'ctx>,
    error_block: JitEvmEngineSimpleBlock<'ctx>,
}

impl<'ctx> InstructionCursor<'ctx> {
    pub fn new(
        ctx: &BuilderContext<'ctx>,
        code: IndexedEvmCode,
    ) -> Result<Self, JitEvmEngineError> {
        let ops_len = code.code.ops.len();
        if ops_len == 0 {
            return Err(JitEvmEngineError::NoInstructionsToRender);
        }

        // SETUP JIT'ED CONTRACT FUNCTION

        let executecontract_fn_type = ctx.types.type_void.fn_type(
            &[
                ctx.types.type_ptrint.into(),
                ctx.types.type_ptrint.into(),
                ctx.types.type_i64.into(),
            ],
            false,
        );
        let function = ctx
            .module
            .add_function("executecontract", executecontract_fn_type, None);

        // SETUP HANDLER

        let setup_block = ctx.context.append_basic_block(function, "setup");
        ctx.builder.position_at_end(setup_block);

        let setup_book = {
            let execution_context = function.get_nth_param(0).unwrap().into_int_value();
            let execution_context_ptr = ctx.builder.build_int_to_ptr(
                execution_context,
                ctx.types.type_ptrint.ptr_type(AddressSpace::default()),
                "ctx_ptr",
            )?;
            let sp_int = ctx
                .builder
                .build_load(ctx.types.type_ptrint, execution_context_ptr, "sp_int")?
                .into_int_value();
            let max_offset = EVM_STACK_SIZE as u64 * EVM_STACK_ELEMENT_SIZE;
            let sp_max = ctx.builder.build_int_add(
                sp_int,
                ctx.types.type_ptrint.const_int(max_offset, false),
                "sp_max",
            )?;
            let mem_offset = ctx.builder.build_int_add(
                execution_context,
                ctx.types.type_ptrint.size_of(),
                "mem_offset",
            )?;
            let mem_ptr = ctx.builder.build_int_to_ptr(
                mem_offset,
                ctx.types.type_ptrint.ptr_type(AddressSpace::default()),
                "mem_ptr",
            )?;

            let init_gas = function.get_nth_param(2).unwrap().into_int_value();
            let gas_limit = TransactionContext::gas_limit(&ctx, execution_context)?;
            let gas_remaining = ctx.builder.build_int_sub(gas_limit, init_gas, "")?;
            let gas_refund = ctx.types.type_i64.const_int(0, false);

            let mem = ctx
                .builder
                .build_load(ctx.types.type_ptrint, mem_ptr, "mem")?
                .into_int_value();
            let mem_size = ctx.types.type_i64.const_int(0, false);
            let mem_gas = ctx.types.type_i64.const_int(0, false);

            JitEvmEngineBookkeeping {
                execution_context: execution_context,
                sp_min: sp_int,
                sp_max: sp_max,
                gas_remaining,
                gas_refund,
                sp: sp_int,
                mem,
                mem_size,
                mem_gas,
            }
        };

        // INSTRUCTIONS

        let mut instructions: Vec<JitEvmEngineSimpleBlock<'_>> = Vec::new();
        for i in 0..ops_len {
            let block_before = if i == 0 {
                setup_block
            } else {
                instructions[i - 1].block
            };
            let label = format!("{}_{:?}", i, code.code.ops[i]);
            instructions.push(JitEvmEngineSimpleBlock::new(
                &ctx,
                block_before,
                &label,
                &format!("_{}", i),
            )?);
        }

        ctx.builder.position_at_end(setup_block);
        ctx.builder
            .build_unconditional_branch(instructions[0].block)?;
        instructions[0].phi_setup_block(&setup_book, &setup_block);

        // END HANDLER

        let end_block =
            JitEvmEngineSimpleBlock::new(&ctx, instructions[ops_len - 1].block, &"end", &"-end")?;

        JitContractExecutionResult::build_exit_success(
            &ctx,
            &end_block,
            JitContractResultCode::SuccessStop,
        )?;

        // ERROR-JUMPDEST HANDLER

        let error_block = JitEvmEngineSimpleBlock::error_block(
            &ctx,
            end_block.block,
            &"error-jumpdest",
            &"-error-jumpdest",
        )?;
        JitContractExecutionResult::build_exit_halt(&ctx, &error_block)?;

        Ok(InstructionCursor {
            code,
            instructions,
            end_block,
            error_block,
        })
    }

    pub fn iter<'a>(&'a self) -> Iter<'a, 'ctx> {
        Iter::new(self)
    }
}
