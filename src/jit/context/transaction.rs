use crate::jit::{
    context::JitEvmPtrs, contract::BuilderContext, cursor::CurrentInstruction, JitEvmEngineError,
};
use alloy_primitives::{Address, U256};
use inkwell::{
    context::Context, targets::TargetData, types::StructType, values::IntValue, AddressSpace,
};

#[repr(C)]
#[derive(Clone, Debug)]
pub struct TransactionContext {
    address: U256,
    caller: U256,
    gas_price: U256,
    priority_fee: U256,
    transact_to: U256,
    value: U256,
    chain_id: u64,
    nonce: u64,
    gas_limit: u64,
    data: usize,
    len: usize,
}

impl Default for TransactionContext {
    fn default() -> TransactionContext {
        TransactionContext {
            address: U256::ZERO,
            caller: U256::ZERO,
            gas_price: U256::ZERO,
            priority_fee: U256::ZERO,
            transact_to: U256::ZERO,
            value: U256::ZERO,
            chain_id: 1,
            nonce: 0,
            gas_limit: u64::MAX,
            data: 0,
            len: 0,
        }
    }
}

impl TransactionContext {
    pub fn contract_address(&self) -> Address {
        Address::from_word(self.address.into())
    }

    pub fn set_contract_address(&mut self, address: Address) {
        self.address = address.into_word().into();
    }

    pub fn set_gas_limit(&mut self, gas_limit: u64) {
        self.gas_limit = gas_limit;
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
            u256_type.into(), // address
            u256_type.into(), // caller
            u256_type.into(), // gas_price
            u256_type.into(), // priority_fee
            u256_type.into(), // transact_to
            u256_type.into(), // value
            i64_type.into(),  // chain_id
            i64_type.into(),  // nonce
            i64_type.into(),  // gas_limit
            ptr_type.into(),  // data
            i64_type.into(),  // len
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
}
