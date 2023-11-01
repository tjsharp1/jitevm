use crate::constants::*;
use crate::jit::{
    contract::{BuilderContext, JitEvmEngineSimpleBlock},
    cursor::CurrentInstruction,
    error::JitEvmEngineError,
    gas::build_gas_check,
    ops::{build_stack_check, build_stack_push},
};
use alloy_primitives::{Address, B160, B256, U256};
use bytes::Bytes;
use core::marker::PhantomData;
use inkwell::{
    context::Context,
    targets::TargetData,
    types::StructType,
    values::{IntValue, PointerValue},
    AddressSpace,
};
use revm_primitives::{db::Database, State};
use std::collections::HashMap;

mod state;
pub use state::DBBox;
use state::EVMState;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Success {
    Stop,
    Return,
    SelfDestruct,
}

impl From<JitContractResultCode> for Success {
    fn from(code: JitContractResultCode) -> Success {
        use JitContractResultCode::*;
        use Success::*;

        match code {
            SuccessStop => Stop,
            SuccessReturn => Return,
            SuccessSelfDestruct => SelfDestruct,
            o => panic!("{:?} is not a success code!", o),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Halt {
    OutOfGasBasicOutOfGas,
    OutOfGasMemoryLimit,
    OutOfGasMemory,
    OutOfGasPrecompile,
    OutOfGasInvalidOperand,
    OpcodeNotFound,
    InvalidFEOpcode,
    InvalidJump,
    NotActivated,
    StackUnderflow,
    StackOverflow,
    OutOfOffset,
    CreateCollision,
    PrecompileError,
    NonceOverflow,
    CreateContractSizeLimit,
    CreateContractStartingWithEF,
    CreateInitcodeSizeLimit,
    OverflowPayment,
    StateChangeDuringStaticCall,
    CallNotAllowedInsideStatic,
    OutOfFund,
    CallTooDeep,
}

impl From<JitContractResultCode> for Halt {
    fn from(code: JitContractResultCode) -> Halt {
        use JitContractResultCode::*;

        match code {
            OutOfGasBasicOutOfGas => Halt::OutOfGasBasicOutOfGas,
            OutOfGasMemoryLimit => Halt::OutOfGasMemoryLimit,
            OutOfGasMemory => Halt::OutOfGasMemory,
            OutOfGasPrecompile => Halt::OutOfGasPrecompile,
            OutOfGasInvalidOperand => Halt::OutOfGasInvalidOperand,
            OpcodeNotFound => Halt::OpcodeNotFound,
            InvalidFEOpcode => Halt::InvalidFEOpcode,
            InvalidJump => Halt::InvalidJump,
            NotActivated => Halt::NotActivated,
            StackUnderflow => Halt::StackUnderflow,
            StackOverflow => Halt::StackOverflow,
            OutOfOffset => Halt::OutOfOffset,
            CreateCollision => Halt::CreateCollision,
            PrecompileError => Halt::PrecompileError,
            NonceOverflow => Halt::NonceOverflow,
            CreateContractSizeLimit => Halt::CreateContractSizeLimit,
            CreateContractStartingWithEF => Halt::CreateContractStartingWithEF,
            CreateInitcodeSizeLimit => Halt::CreateInitcodeSizeLimit,
            OverflowPayment => Halt::OverflowPayment,
            StateChangeDuringStaticCall => Halt::StateChangeDuringStaticCall,
            CallNotAllowedInsideStatic => Halt::CallNotAllowedInsideStatic,
            OutOfFund => Halt::OutOfFund,
            CallTooDeep => Halt::CallTooDeep,
            o => panic!("{:?} is not a Halt error code!", o),
        }
    }
}

// TODO: all the extra stuff....
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExecutionResult {
    /// Returned successfully
    Success {
        reason: Success,
        gas_used: u64,
        //gas_refunded: u64,
        //logs: Vec<Log>,
        //output: Output,
    },
    /// Reverted by `REVERT` opcode that doesn't spend all gas.
    Revert { gas_used: u64, output: Bytes },
    /// Reverted for various reasons and spend all gas.
    Halt {
        reason: Halt,
        /// Halting will spend all the gas, and will be equal to gas_limit.
        gas_used: u64,
    },
}

impl From<JitContractExecutionResult> for ExecutionResult {
    fn from(result: JitContractExecutionResult) -> ExecutionResult {
        let JitContractExecutionResult {
            result_code,
            gas_used,
            gas_refunded,
        } = result;

        let code = JitContractResultCode::from(result_code);

        if code.is_success() {
            let reason = code.into();

            ExecutionResult::Success { reason, gas_used }
        } else if code.is_revert() {
            // TODO: revert instruction...
            ExecutionResult::Revert {
                gas_used,
                output: Bytes::new(),
            }
        } else {
            let reason = code.into();

            ExecutionResult::Halt { reason, gas_used }
        }
    }
}

#[derive(Debug, PartialEq)]
pub(in crate::jit) enum JitContractResultCode {
    SuccessStop,
    SuccessReturn,
    SuccessSelfDestruct,
    // TODO: handle the below return codes...
    Revert,
    OutOfGasBasicOutOfGas,
    OutOfGasMemoryLimit,
    OutOfGasMemory,
    OutOfGasPrecompile,
    OutOfGasInvalidOperand,
    OpcodeNotFound,
    InvalidFEOpcode,
    InvalidJump,
    NotActivated,
    StackUnderflow,
    StackOverflow,
    OutOfOffset,
    CreateCollision,
    PrecompileError,
    NonceOverflow,
    CreateContractSizeLimit,
    CreateContractStartingWithEF,
    CreateInitcodeSizeLimit,
    OverflowPayment,
    StateChangeDuringStaticCall,
    CallNotAllowedInsideStatic,
    OutOfFund,
    CallTooDeep,
}

impl JitContractResultCode {
    pub fn is_success(&self) -> bool {
        use JitContractResultCode::*;

        match self {
            SuccessStop | SuccessReturn | SuccessSelfDestruct => true,
            _ => false,
        }
    }

    pub fn is_revert(&self) -> bool {
        *self == JitContractResultCode::Revert
    }
}

impl From<u32> for JitContractResultCode {
    fn from(val: u32) -> JitContractResultCode {
        use JitContractResultCode::*;

        match val {
            0 => SuccessStop,
            1 => SuccessReturn,
            2 => SuccessSelfDestruct,
            3 => Revert,
            4 => OutOfGasBasicOutOfGas,
            5 => OutOfGasMemoryLimit,
            6 => OutOfGasMemory,
            7 => OutOfGasPrecompile,
            8 => OutOfGasInvalidOperand,
            9 => OpcodeNotFound,
            10 => InvalidFEOpcode,
            11 => InvalidJump,
            12 => NotActivated,
            13 => StackUnderflow,
            14 => StackOverflow,
            15 => OutOfOffset,
            16 => CreateCollision,
            17 => PrecompileError,
            18 => NonceOverflow,
            19 => CreateContractSizeLimit,
            20 => CreateContractStartingWithEF,
            21 => CreateInitcodeSizeLimit,
            22 => OverflowPayment,
            23 => StateChangeDuringStaticCall,
            24 => CallNotAllowedInsideStatic,
            25 => OutOfFund,
            26 => CallTooDeep,
            o => unimplemented!("No JitContractResultCode for {}", o),
        }
    }
}

impl From<JitContractResultCode> for u32 {
    fn from(result: JitContractResultCode) -> u32 {
        use JitContractResultCode::*;

        match result {
            SuccessStop => 0,
            SuccessReturn => 1,
            SuccessSelfDestruct => 2,
            Revert => 3,
            OutOfGasBasicOutOfGas => 4,
            OutOfGasMemoryLimit => 5,
            OutOfGasMemory => 6,
            OutOfGasPrecompile => 7,
            OutOfGasInvalidOperand => 8,
            OpcodeNotFound => 9,
            InvalidFEOpcode => 10,
            InvalidJump => 11,
            NotActivated => 12,
            StackUnderflow => 13,
            StackOverflow => 14,
            OutOfOffset => 15,
            CreateCollision => 16,
            PrecompileError => 17,
            NonceOverflow => 18,
            CreateContractSizeLimit => 19,
            CreateContractStartingWithEF => 20,
            CreateInitcodeSizeLimit => 21,
            OverflowPayment => 22,
            StateChangeDuringStaticCall => 23,
            CallNotAllowedInsideStatic => 24,
            OutOfFund => 25,
            CallTooDeep => 26,
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub(in crate::jit) struct JitContractExecutionResult {
    result_code: u32,
    gas_used: u64,
    // TODO: figure out gas refunds
    gas_refunded: u64,
    // TODO: finish the below, revert will have output as well, all remaining will have gas_used
    // logs: Vec<Log>,
    // output: Output::Call(Bytes) or Output::Create(Bytes, Option<Address>)
}

impl JitContractExecutionResult {
    pub fn new() -> JitContractExecutionResult {
        JitContractExecutionResult {
            result_code: 0,
            gas_used: 0,
            gas_refunded: 0,
        }
    }
}

impl<'ctx> JitContractExecutionResult {
    pub fn llvm_struct_type(ctx: &'ctx Context, _: &TargetData) -> StructType<'ctx> {
        let type_i32 = ctx.i32_type();
        let type_i64 = ctx.i64_type();

        let fields = vec![
            type_i32.into(), // result_code
            type_i64.into(), // gas_used
            type_i64.into(), // gas_refunded
        ];
        ctx.struct_type(&fields, false)
    }

    pub fn build_exit_halt(
        ctx: &BuilderContext<'ctx>,
        block: &JitEvmEngineSimpleBlock<'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = block.book();

        let function = ctx
            .module
            .get_function("executecontract")
            .expect("Function should be there");
        let execution_result = function.get_nth_param(1).unwrap().into_int_value();
        let ptr = ctx.builder.build_int_to_ptr(
            execution_result,
            ctx.types.execution_result.ptr_type(AddressSpace::default()),
            "ctx_ptr",
        )?;

        let result_code_ptr = ctx.builder.build_struct_gep(
            ctx.types.execution_result,
            ptr,
            0,
            "result_code_offset",
        )?;

        // TODO: JitEvmEngineError for this?
        let error_code = block
            .phi_error
            .expect("Should be an error block")
            .as_basic_value()
            .into_int_value();
        ctx.builder.build_store(result_code_ptr, error_code)?;

        let gas_used_ptr =
            ctx.builder
                .build_struct_gep(ctx.types.execution_result, ptr, 1, "gas_used_offset")?;

        let gas_limit = TransactionContext::gas_limit(&ctx, book.execution_context)?;
        let gas_used = ctx
            .builder
            .build_int_sub(gas_limit, book.gas_remaining, "calc_gas_used")?;
        ctx.builder.build_store(gas_used_ptr, gas_used)?;

        ctx.builder.build_return(None)?;
        Ok(())
    }

    pub fn build_exit_success(
        ctx: &BuilderContext<'ctx>,
        block: &JitEvmEngineSimpleBlock<'ctx>,
        code: JitContractResultCode,
    ) -> Result<(), JitEvmEngineError> {
        let book = block.book();

        let function = ctx
            .module
            .get_function("executecontract")
            .expect("Function should be there");
        let execution_result = function.get_nth_param(1).unwrap().into_int_value();
        let ptr = ctx.builder.build_int_to_ptr(
            execution_result,
            ctx.types.execution_result.ptr_type(AddressSpace::default()),
            "ctx_ptr",
        )?;

        let result_code = ctx.builder.build_struct_gep(
            ctx.types.execution_result,
            ptr,
            0,
            "result_code_offset",
        )?;

        let code = ctx.types.type_i32.const_int(u32::from(code) as u64, false);
        ctx.builder.build_store(result_code, code)?;

        let gas_used_ptr =
            ctx.builder
                .build_struct_gep(ctx.types.execution_result, ptr, 1, "gas_used_offset")?;

        let gas_limit = TransactionContext::gas_limit(&ctx, book.execution_context)?;
        let gas_used = ctx
            .builder
            .build_int_sub(gas_limit, book.gas_remaining, "calc_gas_used")?;
        ctx.builder.build_store(gas_used_ptr, gas_used)?;

        //TODO: gas_remaining calc.... get this when we start gas accounting!
        let gas_refund_ptr = ctx.builder.build_struct_gep(
            ctx.types.execution_result,
            ptr,
            2,
            "gas_refund_offset",
        )?;
        let weird_number = ctx.types.type_i64.const_int(43, false);
        ctx.builder.build_store(gas_refund_ptr, weird_number)?;

        ctx.builder.build_return(None)?;
        Ok(())
    }
}

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

    /////////////////////////////TODO!!!///////////////////////////////////////////////
    pub fn sload(&self, key: &U256) -> (U256, bool) {
        let evm_state: &mut EVMState = unsafe { &mut *(self.evm_state as *mut _) };
        let tx_context: &TransactionContext = unsafe { &*(self.transaction_context as *const _) };
        // TODO: maybe put in EVMState as current account?
        let address = tx_context.contract_address();

        evm_state.sload(address, *key)
    }

    pub fn sstore(&self, key: U256, value: U256) -> (U256, U256, U256, bool) {
        let evm_state: &mut EVMState = unsafe { &mut *(self.evm_state as *mut _) };
        let tx_context: &TransactionContext = unsafe { &*(self.transaction_context as *const _) };
        // TODO: maybe put in EVMState as current account?
        let address = tx_context.contract_address();

        evm_state.sstore(address, key, value)
    }
    /////////////////////////////TODO!!!///////////////////////////////////////////////

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
