use crate::constants::*;
use crate::jit::{contract::BuilderContext, error::JitEvmEngineError, tracing::TraceData};
use alloy_primitives::{Address, B256, U256};
use inkwell::{
    context::Context,
    targets::TargetData,
    types::StructType,
    values::{IntValue, PointerValue},
    AddressSpace,
};
use revm_primitives::{db::Database, Bytes, Spec, State};

mod block;
mod result;
mod state;
mod transaction;

pub(in crate::jit) use block::BlockContext;
pub(in crate::jit) use result::{JitContractExecutionResult, JitContractResultCode};
pub(in crate::jit) use transaction::TransactionContext;

pub use block::BlockConfig;
pub use result::{ExecutionResult, Halt, Success};
pub use state::DBBox;
use state::EVMState;
pub use transaction::TransactionConfig;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub(in crate::jit) struct JitEvmPtrs {
    // WARNING: if you change anything here (adding fields is ok), then you need to change:
    //           - LLVM instructions in "setup" block of "executecontract" function
    //           - JitContractBuilder::callback_sload, JitContractBuilder::callback_sstore, ...
    //           - possibly other code! => try not to change this!
    pub stack: usize,
    pub memory: usize,
    pub block_context: usize,
    pub transaction_context: usize,
    pub calldata: usize,
    pub calldatalen: usize,
    pub evm_state: usize,
    pub trace_data: usize,
}

impl<'ctx> JitEvmPtrs {
    pub fn llvm_struct_type(ctx: &'ctx Context, target_data: &TargetData) -> StructType<'ctx> {
        let ptr_type = ctx
            .ptr_sized_int_type(target_data, None)
            .ptr_type(AddressSpace::default());

        let fields = vec![
            ptr_type.into(), // stack
            ptr_type.into(), // memory
            ptr_type.into(), // block_context
            ptr_type.into(), // transaction_context
            ptr_type.into(), // calldata
            ptr_type.into(), // calldatalen
            ptr_type.into(), // evm_state
            ptr_type.into(), // trace_data
        ];
        ctx.struct_type(&fields, false)
    }

    pub fn build_get_transaction_context_ptr(
        ctx: &BuilderContext<'ctx>,
        execution_context: IntValue<'ctx>,
    ) -> Result<PointerValue<'ctx>, JitEvmEngineError> {
        let ptr = ctx.builder.build_int_to_ptr(
            execution_context,
            ctx.types
                .execution_context
                .ptr_type(AddressSpace::default()),
            "",
        )?;

        let offset = ctx.builder.build_struct_gep(
            ctx.types.execution_context,
            ptr,
            3,
            "get_transaction_context",
        )?;

        Ok(ctx
            .builder
            .build_load(
                ctx.types
                    .transaction_context
                    .ptr_type(AddressSpace::default()),
                offset,
                "transaction_context_ptr",
            )?
            .into_pointer_value())
    }

    pub fn build_get_block_context_ptr(
        ctx: &BuilderContext<'ctx>,
        execution_context: IntValue<'ctx>,
    ) -> Result<PointerValue<'ctx>, JitEvmEngineError> {
        let ptr = ctx.builder.build_int_to_ptr(
            execution_context,
            ctx.types
                .execution_context
                .ptr_type(AddressSpace::default()),
            "",
        )?;

        let offset = ctx.builder.build_struct_gep(
            ctx.types.execution_context,
            ptr,
            2,
            "get_block_context",
        )?;

        Ok(ctx
            .builder
            .build_load(
                ctx.types.block_context.ptr_type(AddressSpace::default()),
                offset,
                "block_context_ptr",
            )?
            .into_pointer_value())
    }

    pub fn build_get_calldata_ptr(
        ctx: &BuilderContext<'ctx>,
        execution_context: IntValue<'ctx>,
    ) -> Result<PointerValue<'ctx>, JitEvmEngineError> {
        let ptr = ctx.builder.build_int_to_ptr(
            execution_context,
            ctx.types
                .execution_context
                .ptr_type(AddressSpace::default()),
            "",
        )?;

        let offset =
            ctx.builder
                .build_struct_gep(ctx.types.execution_context, ptr, 4, "get_calldata")?;

        Ok(ctx
            .builder
            .build_load(
                ctx.types.type_ptrint.ptr_type(AddressSpace::default()),
                offset,
                "calldata_ptr",
            )?
            .into_pointer_value())
    }

    pub fn build_get_calldatalen(
        ctx: &BuilderContext<'ctx>,
        execution_context: IntValue<'ctx>,
    ) -> Result<IntValue<'ctx>, JitEvmEngineError> {
        let ptr = ctx.builder.build_int_to_ptr(
            execution_context,
            ctx.types
                .execution_context
                .ptr_type(AddressSpace::default()),
            "",
        )?;

        let offset =
            ctx.builder
                .build_struct_gep(ctx.types.execution_context, ptr, 5, "get_calldatalen")?;

        Ok(ctx
            .builder
            .build_load(ctx.types.type_i64, offset, "calldatalen")?
            .into_int_value())
    }
}

impl JitEvmPtrs {
    pub fn from_raw(ptr: usize) -> JitEvmPtrs {
        unsafe { *(ptr as *mut _) }
    }

    pub fn from_context<SPEC: Spec>(ctx: &mut JitEvmExecutionContext<SPEC>) -> JitEvmPtrs {
        JitEvmPtrs {
            stack: ctx.stack.as_mut_ptr() as usize,
            memory: ctx.memory.as_mut_ptr() as usize,
            block_context: &mut ctx.block_context as *mut _ as usize,
            transaction_context: &mut ctx.transaction_context as *mut _ as usize,
            calldata: ctx.calldata.as_ptr() as usize,
            calldatalen: ctx.calldata.len(),
            evm_state: &mut ctx.evm_state as *mut _ as usize,
            trace_data: &mut ctx.trace_data as *mut _ as usize,
        }
    }

    pub fn sload(&self, key: &U256) -> (U256, u64) {
        let evm_state: &mut EVMState = unsafe { &mut *(self.evm_state as *mut _) };
        let tx_context: &TransactionContext = unsafe { &*(self.transaction_context as *const _) };
        let address = tx_context.contract_address();

        evm_state.sload(address, *key)
    }

    pub fn sstore(&self, key: U256, value: U256) -> (u64, i64) {
        let evm_state: &mut EVMState = unsafe { &mut *(self.evm_state as *mut _) };
        let tx_context: &TransactionContext = unsafe { &*(self.transaction_context as *const _) };
        let address = tx_context.contract_address();

        evm_state.sstore(address, key, value)
    }

    pub fn mem_slice(&self, ptr: usize, size: usize) -> &[u8] {
        unsafe { std::slice::from_raw_parts((self.memory + ptr) as *const u8, size) }
    }

    pub fn stack_slice(&self, size: usize) -> &[U256] {
        unsafe { std::slice::from_raw_parts(self.stack as *const U256, size) }
    }

    pub fn stack_mut(&self, sp: usize, offset: usize) -> &mut U256 {
        unsafe { &mut *((sp - offset * EVM_STACK_ELEMENT_SIZE as usize) as *mut _) }
    }

    pub fn push_trace_data(&self, data: TraceData) {
        let trace_data: &mut Vec<TraceData> = unsafe { &mut *(self.trace_data as *mut _) };
        trace_data.push(data);
    }
}

#[derive(Debug, Default)]
pub struct JitEvmExecutionContextBuilder<SPEC: Spec> {
    spec: SPEC,
    block: Option<BlockConfig>,
    transaction: Option<TransactionConfig>,
    calldata: Option<Bytes>,
}

impl<SPEC: Spec> JitEvmExecutionContextBuilder<SPEC> {
    pub fn with_block_config(mut self, cfg: BlockConfig) -> Self {
        self.block = Some(cfg);
        self
    }

    pub fn with_transaction_config(mut self, cfg: TransactionConfig) -> Self {
        self.transaction = Some(cfg);
        self
    }

    pub fn with_calldata(mut self, data: Bytes) -> Self {
        self.calldata = Some(data);
        self
    }

    pub fn build_with_db<DB: Database>(self, db: &DB) -> JitEvmExecutionContext<SPEC> {
        let mut evm_state = EVMState::with_db::<DB, SPEC>(db);

        let cfg = self.block.unwrap_or_default();
        let block_context = BlockContext::from_config(cfg);

        let cfg = self.transaction.unwrap_or_default();
        let transaction_context = TransactionContext::from_config(cfg);

        // preload caller and contract accounts
        evm_state.load_account(transaction_context.caller_address());
        evm_state.load_account(transaction_context.contract_address());

        let calldata = self.calldata.unwrap_or_default();

        JitEvmExecutionContext {
            stack: vec![U256::ZERO; EVM_STACK_SIZE],
            memory: vec![0u8; EVM_MEMORY_BYTES],
            block_context,
            transaction_context,
            calldata,
            evm_state,
            trace_data: Vec::new(),
            _data: Default::default(),
        }
    }
}

#[derive(Debug)]
pub struct JitEvmExecutionContext<SPEC: Spec> {
    pub stack: Vec<U256>,
    pub memory: Vec<u8>,
    block_context: BlockContext,
    transaction_context: TransactionContext,
    calldata: Bytes,
    evm_state: EVMState,
    trace_data: Vec<TraceData>,
    _data: std::marker::PhantomData<SPEC>,
}

impl<SPEC: Spec> JitEvmExecutionContext<SPEC> {
    pub fn calldata(&self) -> &[u8] {
        &self.calldata
    }

    pub fn set_number(&mut self, num: U256) {
        self.block_context.number = num;
    }

    pub fn set_coinbase(&mut self, coinbase: Address) {
        self.block_context.coinbase = coinbase.into_word().into();
    }

    pub fn set_timestamp(&mut self, ts: U256) {
        self.block_context.timestamp = ts;
    }

    pub fn set_prevrandao(&mut self, randao: B256) {
        self.block_context.prevrandao = randao.into();
    }

    pub fn set_basefee(&mut self, basefee: U256) {
        self.block_context.basefee = basefee;
    }

    pub fn set_block_gas_limit(&mut self, gas_limit: U256) {
        self.block_context.gas_limit = gas_limit;
    }

    pub fn set_callvalue(&mut self, value: U256) {
        self.transaction_context.value = value;
    }

    pub fn set_caller(&mut self, caller: Address) {
        self.transaction_context.caller = caller.into_word().into();
    }

    pub fn set_calldata(&mut self, data: Bytes) {
        self.calldata = data;
    }
}

impl<SPEC: Spec> JitEvmExecutionContext<SPEC> {
    pub fn builder(spec: SPEC) -> JitEvmExecutionContextBuilder<SPEC> {
        JitEvmExecutionContextBuilder {
            spec,
            block: None,
            transaction: None,
            calldata: None,
        }
    }

    pub fn trace_data(&self) -> Vec<TraceData> {
        self.trace_data.clone()
    }

    pub fn final_state(self) -> State {
        let JitEvmExecutionContext { evm_state, .. } = self;
        evm_state.state
    }
}
