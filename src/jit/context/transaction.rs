use crate::jit::{
    context::JitEvmPtrs,
    contract::BuilderContext,
    cursor::CurrentInstruction,
    gas::{build_gas_check, const_cost},
    ops::{build_stack_check, build_stack_push},
    JitEvmEngineError,
};
use alloy_primitives::{Address, U256};
use inkwell::{
    context::Context, targets::TargetData, types::StructType, values::IntValue, AddressSpace,
};
use revm_primitives::Spec;

#[derive(Clone, Debug)]
pub struct TransactionConfig {
    pub caller: Address,
    pub gas_price: U256,
    pub priority_fee: U256,
    // TODO: call or create
    pub transact_to: Address,
    pub value: U256,
    pub chain_id: u64,
    pub nonce: u64,
    pub gas_limit: u64,
    // TODO: access lists...
    // TODO: EIP-4844 blob hashes & gas
}

impl Default for TransactionConfig {
    fn default() -> TransactionConfig {
        TransactionConfig {
            caller: Address::ZERO,
            gas_price: U256::ZERO,
            priority_fee: U256::ZERO,
            transact_to: Address::ZERO,
            value: U256::ZERO,
            chain_id: 1,
            nonce: 0,
            gas_limit: u64::MAX,
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug)]
pub struct TransactionContext {
    pub origin: U256,
    pub caller: U256,
    pub gas_price: U256,
    pub priority_fee: U256,
    pub transact_to: U256,
    pub value: U256,
    pub chain_id: u64,
    pub nonce: u64,
    pub gas_limit: u64,
}

impl TransactionContext {
    pub fn from_config(cfg: TransactionConfig) -> TransactionContext {
        let TransactionConfig {
            caller,
            gas_price,
            priority_fee,
            transact_to,
            value,
            chain_id,
            nonce,
            gas_limit,
        } = cfg;

        TransactionContext {
            origin: caller.into_word().into(),
            caller: caller.into_word().into(),
            gas_price,
            priority_fee,
            transact_to: transact_to.into_word().into(),
            value,
            chain_id,
            nonce,
            gas_limit,
        }
    }

    pub fn caller_address(&self) -> Address {
        Address::from_word(self.caller.into())
    }

    pub fn contract_address(&self) -> Address {
        Address::from_word(self.transact_to.into())
    }
}

impl<'ctx> TransactionContext {
    pub fn llvm_struct_type(ctx: &'ctx Context, _: &TargetData) -> StructType<'ctx> {
        let u256_type = ctx.custom_width_int_type(256);
        let i64_type = ctx.i64_type();

        let fields = vec![
            u256_type.into(), // origin
            u256_type.into(), // caller
            u256_type.into(), // gas_price
            u256_type.into(), // priority_fee
            u256_type.into(), // transact_to
            u256_type.into(), // value
            i64_type.into(),  // chain_id
            i64_type.into(),  // nonce
            i64_type.into(),  // gas_limit
        ];
        ctx.struct_type(&fields, false)
    }

    pub(crate) fn gas_limit<'a>(
        ctx: &BuilderContext<'ctx>,
        execution_context: IntValue<'ctx>,
    ) -> Result<IntValue<'ctx>, JitEvmEngineError> {
        let tx_context_ptr =
            JitEvmPtrs::build_get_transaction_context_ptr(&ctx, execution_context)?;
        let ptr = ctx.builder.build_pointer_cast(
            tx_context_ptr,
            ctx.types
                .transaction_context
                .ptr_type(AddressSpace::default()),
            "tx_ctx_ptr",
        )?;

        let ptr = ctx.builder.build_struct_gep(
            ctx.types.transaction_context,
            ptr,
            8,
            "get_tx_gas_limit",
        )?;

        Ok(ctx
            .builder
            .build_load(ctx.types.type_i64, ptr, "load_tx_gas_limit")?
            .into_int_value())
    }

    pub(crate) fn build_get_codesize<'a, SPEC: Spec>(
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        build_gas_check!(ctx, current);
        build_stack_check!(ctx, current, 0, 1);

        let size = current.code().len as u64;
        let value = ctx.types.type_stackel.const_int(size, false);

        let book = current.book();
        let book = build_stack_push!(ctx, book, value);

        ctx.builder
            .build_unconditional_branch(current.next().block)?;
        current.next().add_incoming(&book, current.block());

        Ok(())
    }

    pub(crate) fn build_get_callvalue<'a, SPEC: Spec>(
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        build_gas_check!(ctx, current);
        build_stack_check!(ctx, current, 0, 1);

        let book = current.book();
        let ptr = JitEvmPtrs::build_get_transaction_context_ptr(ctx, book.execution_context)?;
        let ptr = ctx.builder.build_pointer_cast(
            ptr,
            ctx.types
                .transaction_context
                .ptr_type(AddressSpace::default()),
            "",
        )?;

        let ptr =
            ctx.builder
                .build_struct_gep(ctx.types.transaction_context, ptr, 5, "get_value")?;

        let value = ctx
            .builder
            .build_load(ctx.types.type_stackel, ptr, "load_value")?
            .into_int_value();

        let book = build_stack_push!(ctx, book, value);

        ctx.builder
            .build_unconditional_branch(current.next().block)?;
        current.next().add_incoming(&book, current.block());

        Ok(())
    }

    pub(crate) fn build_get_caller<'a, SPEC: Spec>(
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        build_gas_check!(ctx, current);
        build_stack_check!(ctx, current, 0, 1);

        let book = current.book();
        let ptr = JitEvmPtrs::build_get_transaction_context_ptr(ctx, book.execution_context)?;
        let ptr = ctx.builder.build_pointer_cast(
            ptr,
            ctx.types
                .transaction_context
                .ptr_type(AddressSpace::default()),
            "",
        )?;

        let ptr =
            ctx.builder
                .build_struct_gep(ctx.types.transaction_context, ptr, 1, "get_caller")?;

        let value = ctx
            .builder
            .build_load(ctx.types.type_stackel, ptr, "load_caller")?
            .into_int_value();

        let book = build_stack_push!(ctx, book, value);

        ctx.builder
            .build_unconditional_branch(current.next().block)?;
        current.next().add_incoming(&book, current.block());

        Ok(())
    }

    pub(crate) fn build_get_origin<'a, SPEC: Spec>(
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        build_gas_check!(ctx, current);
        build_stack_check!(ctx, current, 0, 1);

        let book = current.book();
        let ptr = JitEvmPtrs::build_get_transaction_context_ptr(ctx, book.execution_context)?;
        let ptr = ctx.builder.build_pointer_cast(
            ptr,
            ctx.types
                .transaction_context
                .ptr_type(AddressSpace::default()),
            "",
        )?;

        let ptr =
            ctx.builder
                .build_struct_gep(ctx.types.transaction_context, ptr, 0, "get_origin")?;

        let value = ctx
            .builder
            .build_load(ctx.types.type_stackel, ptr, "load_origin")?
            .into_int_value();

        let book = build_stack_push!(ctx, book, value);

        ctx.builder
            .build_unconditional_branch(current.next().block)?;
        current.next().add_incoming(&book, current.block());

        Ok(())
    }
}
