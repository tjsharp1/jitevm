use crate::jit::{
    context::JitEvmPtrs,
    contract::BuilderContext,
    cursor::CurrentInstruction,
    gas::build_gas_check,
    ops::{build_stack_check, build_stack_push},
    JitEvmEngineError,
};
use alloy_primitives::{Address, B256, U256};
use inkwell::{context::Context, targets::TargetData, types::StructType, AddressSpace};

#[repr(C)]
#[derive(Clone, Debug)]
pub struct BlockContext {
    number: U256,
    coinbase: U256,
    timestamp: U256,
    difficulty: U256,
    prevrandao: U256,
    basefee: U256,
    gas_limit: U256,
}

impl BlockContext {
    pub fn new() -> BlockContext {
        BlockContext {
            number: U256::ZERO,
            coinbase: U256::ZERO,
            timestamp: U256::ZERO,
            difficulty: U256::ZERO,
            prevrandao: U256::ZERO,
            basefee: U256::ZERO,
            gas_limit: U256::ZERO,
        }
    }

    pub fn set_number(&mut self, number: U256) {
        self.number = number;
    }

    pub fn set_coinbase(&mut self, coinbase: Address) {
        self.coinbase = coinbase.into_word().into();
    }

    pub fn set_timestamp(&mut self, timestamp: U256) {
        self.timestamp = timestamp;
    }

    pub fn set_difficulty(&mut self, difficulty: U256) {
        self.difficulty = difficulty;
    }

    pub fn set_prevrandao(&mut self, prevrandao: B256) {
        self.prevrandao = prevrandao.into();
    }

    pub fn set_basefee(&mut self, basefee: U256) {
        self.basefee = basefee;
    }

    pub fn set_gas_limit(&mut self, gas_limit: U256) {
        self.gas_limit = gas_limit;
    }
}

impl<'ctx> BlockContext {
    pub fn llvm_struct_type(ctx: &'ctx Context, _: &TargetData) -> StructType<'ctx> {
        let u256_type = ctx.custom_width_int_type(256);

        let fields = vec![
            u256_type.into(), // number
            u256_type.into(), // coinbase
            u256_type.into(), // timestamp
            u256_type.into(), // difficulty
            u256_type.into(), // randao
            u256_type.into(), // basefee
            u256_type.into(), // gas
        ];
        ctx.struct_type(&fields, false)
    }

    pub(crate) fn build_get_number<'a>(
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        build_gas_check!(ctx, current);
        build_stack_check!(ctx, current, 0, 1);

        let book = current.book();
        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, book.execution_context)?;
        let ptr = ctx.builder.build_pointer_cast(
            ptr,
            ctx.types.block_context.ptr_type(AddressSpace::default()),
            "",
        )?;

        let ptr = ctx
            .builder
            .build_struct_gep(ctx.types.block_context, ptr, 0, "get_number")?;

        let value = ctx
            .builder
            .build_load(ctx.types.type_stackel, ptr, "load_number")?
            .into_int_value();
        let book = build_stack_push!(ctx, book, value);

        ctx.builder
            .build_unconditional_branch(current.next().block)?;
        current.next().add_incoming(&book, current.block());
        Ok(())
    }

    pub(crate) fn build_get_coinbase<'a>(
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        build_gas_check!(ctx, current);
        build_stack_check!(ctx, current, 0, 1);

        let book = current.book();
        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, book.execution_context)?;
        let ptr = ctx.builder.build_pointer_cast(
            ptr,
            ctx.types.block_context.ptr_type(AddressSpace::default()),
            "",
        )?;

        let ptr = ctx
            .builder
            .build_struct_gep(ctx.types.block_context, ptr, 1, "get_coinbase")?;

        let value = ctx
            .builder
            .build_load(ctx.types.type_stackel, ptr, "load_coinbase")?
            .into_int_value();
        let book = build_stack_push!(ctx, book, value);

        ctx.builder
            .build_unconditional_branch(current.next().block)?;
        current.next().add_incoming(&book, current.block());
        Ok(())
    }

    pub(crate) fn build_get_timestamp<'a>(
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        build_gas_check!(ctx, current);
        build_stack_check!(ctx, current, 0, 1);

        let book = current.book();
        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, book.execution_context)?;
        let ptr = ctx.builder.build_pointer_cast(
            ptr,
            ctx.types.block_context.ptr_type(AddressSpace::default()),
            "",
        )?;

        let ptr = ctx
            .builder
            .build_struct_gep(ctx.types.block_context, ptr, 2, "get_timestamp")?;

        let value = ctx
            .builder
            .build_load(ctx.types.type_stackel, ptr, "load_timestamp")?
            .into_int_value();

        let book = build_stack_push!(ctx, book, value);

        ctx.builder
            .build_unconditional_branch(current.next().block)?;
        current.next().add_incoming(&book, current.block());
        Ok(())
    }

    pub(crate) fn build_get_difficulty<'a>(
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        build_gas_check!(ctx, current);
        build_stack_check!(ctx, current, 0, 1);

        let book = current.book();
        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, book.execution_context)?;
        let ptr = ctx.builder.build_pointer_cast(
            ptr,
            ctx.types.block_context.ptr_type(AddressSpace::default()),
            "",
        )?;

        let ptr =
            ctx.builder
                .build_struct_gep(ctx.types.block_context, ptr, 3, "get_difficulty")?;

        let value = ctx
            .builder
            .build_load(ctx.types.type_stackel, ptr, "load_difficulty")?
            .into_int_value();

        let book = build_stack_push!(ctx, book, value);

        ctx.builder
            .build_unconditional_branch(current.next().block)?;
        current.next().add_incoming(&book, current.block());
        Ok(())
    }

    pub(crate) fn build_get_randao<'a>(
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        build_gas_check!(ctx, current);
        build_stack_check!(ctx, current, 0, 1);

        let book = current.book();
        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, book.execution_context)?;
        let ptr = ctx.builder.build_pointer_cast(
            ptr,
            ctx.types.block_context.ptr_type(AddressSpace::default()),
            "",
        )?;

        let ptr = ctx
            .builder
            .build_struct_gep(ctx.types.block_context, ptr, 4, "get_randao")?;

        let value = ctx
            .builder
            .build_load(ctx.types.type_stackel, ptr, "load_randao")?
            .into_int_value();
        let book = build_stack_push!(ctx, book, value);

        ctx.builder
            .build_unconditional_branch(current.next().block)?;
        current.next().add_incoming(&book, current.block());
        Ok(())
    }

    pub(crate) fn build_get_basefee<'a>(
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        build_gas_check!(ctx, current);
        build_stack_check!(ctx, current, 0, 1);

        let book = current.book();
        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, book.execution_context)?;
        let ptr = ctx.builder.build_pointer_cast(
            ptr,
            ctx.types.block_context.ptr_type(AddressSpace::default()),
            "",
        )?;

        let ptr = ctx
            .builder
            .build_struct_gep(ctx.types.block_context, ptr, 5, "get_basefee")?;

        let value = ctx
            .builder
            .build_load(ctx.types.type_stackel, ptr, "load_basefee")?
            .into_int_value();
        let book = build_stack_push!(ctx, book, value);

        ctx.builder
            .build_unconditional_branch(current.next().block)?;
        current.next().add_incoming(&book, current.block());
        Ok(())
    }

    pub(crate) fn build_get_gas_limit<'a>(
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        build_gas_check!(ctx, current);
        build_stack_check!(ctx, current, 0, 1);

        let book = current.book();
        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, book.execution_context)?;
        let ptr = ctx.builder.build_pointer_cast(
            ptr,
            ctx.types.block_context.ptr_type(AddressSpace::default()),
            "",
        )?;

        let ptr = ctx
            .builder
            .build_struct_gep(ctx.types.block_context, ptr, 6, "get_gas")?;

        let value = ctx
            .builder
            .build_load(ctx.types.type_stackel, ptr, "load_gas")?
            .into_int_value();
        let book = build_stack_push!(ctx, book, value);

        ctx.builder
            .build_unconditional_branch(current.next().block)?;
        current.next().add_incoming(&book, current.block());
        Ok(())
    }
}
