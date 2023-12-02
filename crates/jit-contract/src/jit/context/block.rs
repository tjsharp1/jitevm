use crate::jit::{
    context::JitEvmPtrs, contract::BuilderContext, cursor::Current, ops::build_stack_push,
    JitEvmEngineError,
};
use alloy_primitives::{Address, B256, U256};
use inkwell::{context::Context, targets::TargetData, types::StructType, AddressSpace};

#[derive(Clone, Debug)]
pub struct BlockConfig {
    pub number: U256,
    pub coinbase: Address,
    pub timestamp: U256,
    pub difficulty: U256,
    pub prevrandao: B256,
    pub basefee: U256,
    pub gas_limit: U256,
}

impl Default for BlockConfig {
    fn default() -> BlockConfig {
        BlockConfig {
            number: U256::ZERO,
            coinbase: Address::ZERO,
            timestamp: U256::from(1),
            gas_limit: U256::MAX,
            basefee: U256::ZERO,
            difficulty: U256::ZERO,
            prevrandao: B256::ZERO,
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug)]
pub struct BlockContext {
    pub number: U256,
    pub coinbase: U256,
    pub timestamp: U256,
    pub difficulty: U256,
    pub prevrandao: U256,
    pub basefee: U256,
    pub gas_limit: U256,
}

impl BlockContext {
    pub fn from_config(cfg: BlockConfig) -> BlockContext {
        let BlockConfig {
            number,
            coinbase,
            timestamp,
            gas_limit,
            basefee,
            difficulty,
            prevrandao,
        } = cfg;

        BlockContext {
            number,
            coinbase: coinbase.into_word().into(),
            timestamp,
            difficulty,
            prevrandao: prevrandao.into(),
            basefee,
            gas_limit,
        }
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
        current: &mut Current<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book_ref();
        let ptr = JitEvmPtrs::build_get_block_context_ptr(ctx, book.execution_context)?;
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
        build_stack_push!(ctx, current, value);

        Ok(())
    }

    pub(crate) fn build_get_coinbase<'a>(
        ctx: &BuilderContext<'ctx>,
        current: &mut Current<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book_ref();
        let ptr = JitEvmPtrs::build_get_block_context_ptr(ctx, book.execution_context)?;
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
        build_stack_push!(ctx, current, value);

        Ok(())
    }

    pub(crate) fn build_get_timestamp<'a>(
        ctx: &BuilderContext<'ctx>,
        current: &mut Current<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book_ref();
        let ptr = JitEvmPtrs::build_get_block_context_ptr(ctx, book.execution_context)?;
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

        build_stack_push!(ctx, current, value);

        Ok(())
    }

    pub(crate) fn build_get_randao<'a>(
        ctx: &BuilderContext<'ctx>,
        current: &mut Current<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book_ref();
        let ptr = JitEvmPtrs::build_get_block_context_ptr(ctx, book.execution_context)?;
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
        build_stack_push!(ctx, current, value);

        Ok(())
    }

    pub(crate) fn build_get_basefee<'a>(
        ctx: &BuilderContext<'ctx>,
        current: &mut Current<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book_ref();
        let ptr = JitEvmPtrs::build_get_block_context_ptr(ctx, book.execution_context)?;
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
        build_stack_push!(ctx, current, value);

        Ok(())
    }

    pub(crate) fn build_get_gas_limit<'a>(
        ctx: &BuilderContext<'ctx>,
        current: &mut Current<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book_ref();
        let ptr = JitEvmPtrs::build_get_block_context_ptr(ctx, book.execution_context)?;
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
        build_stack_push!(ctx, current, value);

        Ok(())
    }
}
