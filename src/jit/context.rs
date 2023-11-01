use crate::constants::*;
use crate::jit::{contract::BuilderContext, error::JitEvmEngineError};
use alloy_primitives::U256;
use inkwell::{
    context::Context,
    targets::TargetData,
    types::StructType,
    values::{IntValue, PointerValue},
    AddressSpace,
};
use revm_primitives::{db::Database, State};

mod block;
mod result;
mod state;
mod transaction;

pub use block::BlockContext;
pub use result::{ExecutionResult, Halt, Success};
pub(crate) use result::{JitContractExecutionResult, JitContractResultCode};
pub use state::DBBox;
use state::EVMState;
pub use transaction::TransactionContext;

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
    pub evm_state: usize,
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
            ptr_type.into(), // evm_state
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
}

impl JitEvmPtrs {
    pub fn from_raw(ptr: usize) -> JitEvmPtrs {
        unsafe { *(ptr as *mut _) }
    }

    pub fn from_context(ctx: &mut JitEvmExecutionContext) -> JitEvmPtrs {
        JitEvmPtrs {
            stack: ctx.stack.as_mut_ptr() as usize,
            memory: ctx.memory.as_mut_ptr() as usize,
            block_context: &mut ctx.block_context as *mut _ as usize,
            transaction_context: &mut ctx.transaction_context as *mut _ as usize,
            evm_state: &mut ctx.evm_state as *mut _ as usize,
        }
    }

    pub fn sload(&self, key: &U256) -> (U256, bool) {
        let evm_state: &mut EVMState = unsafe { &mut *(self.evm_state as *mut _) };
        let tx_context: &TransactionContext = unsafe { &*(self.transaction_context as *const _) };
        let address = tx_context.contract_address();

        evm_state.sload(address, *key)
    }

    pub fn sstore(&self, key: U256, value: U256) -> (U256, U256, U256, bool) {
        let evm_state: &mut EVMState = unsafe { &mut *(self.evm_state as *mut _) };
        let tx_context: &TransactionContext = unsafe { &*(self.transaction_context as *const _) };
        let address = tx_context.contract_address();

        evm_state.sstore(address, key, value)
    }

    pub fn mem_slice(&self, ptr: usize, size: usize) -> &[u8] {
        unsafe { std::slice::from_raw_parts((self.memory + ptr) as *const u8, size) }
    }

    pub fn stack(&self, sp: usize, offset: usize) -> &U256 {
        unsafe { &*((sp - offset * EVM_STACK_ELEMENT_SIZE as usize) as *const _) }
    }

    pub fn stack_mut(&self, sp: usize, offset: usize) -> &mut U256 {
        unsafe { &mut *((sp - offset * EVM_STACK_ELEMENT_SIZE as usize) as *mut _) }
    }
}

#[derive(Debug)]
pub struct JitEvmExecutionContext {
    pub stack: Vec<U256>,
    pub memory: Vec<u8>,
    pub block_context: BlockContext,
    pub transaction_context: TransactionContext,
    pub evm_state: EVMState,
}

impl JitEvmExecutionContext {
    pub fn new_with_db<DB: Database>(db: &DB) -> Self {
        let evm_state = EVMState::with_db(db);

        Self {
            stack: vec![U256::ZERO; EVM_STACK_SIZE],
            memory: vec![0u8; EVM_MEMORY_BYTES],
            block_context: BlockContext::new(),
            transaction_context: Default::default(),
            evm_state,
        }
    }

    pub fn final_state(self) -> State {
        let JitEvmExecutionContext { evm_state, .. } = self;
        evm_state.state
    }
}
