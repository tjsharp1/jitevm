use crate::jit::{context::JitEvmPtrs, contract::BuilderContext, JitEvmEngineError};
use alloy_primitives::{Address, U256};
use inkwell::{
    context::Context, targets::TargetData, types::StructType, values::IntValue, AddressSpace,
};

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
    pub fn llvm_struct_type(ctx: &'ctx Context, target_data: &TargetData) -> StructType<'ctx> {
        let u256_type = ctx.custom_width_int_type(256);
        let i64_type = ctx.i64_type();
        let ptr_type = ctx
            .ptr_sized_int_type(target_data, None)
            .ptr_type(AddressSpace::default());

        let fields = vec![
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
            7,
            "get_tx_gas_limit",
        )?;

        Ok(ctx
            .builder
            .build_load(ctx.types.type_i64, ptr, "load_tx_gas_limit")?
            .into_int_value())
    }
}
