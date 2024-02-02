use crate::jit::{
    context::TransactionContext,
    contract::{BuilderContext, JitEvmEngineBookkeeping},
    JitEvmEngineError,
};
use inkwell::{
    context::Context, targets::TargetData, types::StructType, values::IntValue, AddressSpace,
};
use revm_primitives::Bytes;

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
        gas_refunded: u64,
        //logs: Vec<Log>,
        output: Bytes,
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
            return_ptr,
            return_len,
        } = result;

        let code = JitContractResultCode::from(result_code);

        if code.is_success() {
            let reason = code.into();

            let output = if return_len == 0 {
                Bytes::new()
            } else {
                // SAFETY: memory for JIT sub-context this result came from must still be live.
                let ptr = return_ptr as *const u8;
                let slice = unsafe { std::slice::from_raw_parts(ptr, return_len) };
                Bytes::copy_from_slice(slice)
            };

            ExecutionResult::Success {
                reason,
                gas_used: gas_used - gas_refunded,
                gas_refunded,
                output,
            }
        } else if code.is_revert() {
            let output = if return_len == 0 {
                Bytes::new()
            } else {
                // SAFETY: memory for JIT sub-context this result came from must still be live.
                let ptr = return_ptr as *const u8;
                let slice = unsafe { std::slice::from_raw_parts(ptr, return_len) };
                Bytes::copy_from_slice(slice)
            };

            ExecutionResult::Revert { gas_used, output }
        } else {
            let reason = code.into();

            ExecutionResult::Halt { reason, gas_used }
        }
    }
}

// TODO: handle all the below return codes...
#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum JitContractResultCode {
    SuccessStop,
    SuccessReturn,
    SuccessSelfDestruct,
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

        matches!(self, SuccessStop | SuccessReturn | SuccessSelfDestruct)
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
pub(crate) struct JitContractExecutionResult {
    result_code: u32,
    gas_used: u64,
    gas_refunded: u64,
    return_ptr: usize,
    return_len: usize,
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
            return_ptr: 0,
            return_len: 0,
        }
    }
}

impl<'ctx> JitContractExecutionResult {
    pub fn llvm_struct_type(ctx: &'ctx Context, target: &TargetData) -> StructType<'ctx> {
        let type_i32 = ctx.i32_type();
        let type_i64 = ctx.i64_type();
        let type_ptr = ctx.ptr_sized_int_type(target, None);

        let fields = vec![
            type_i32.into(), // result_code
            type_i64.into(), // gas_used
            type_i64.into(), // gas_refunded
            type_ptr.into(), // return ptr
            type_i64.into(), // return data len
            type_i64.into(), // trace data vec
            type_i64.into(), // trace data len
        ];
        ctx.struct_type(&fields, false)
    }

    pub fn build_exit_halt(
        ctx: &BuilderContext<'ctx>,
        book: &JitEvmEngineBookkeeping<'ctx>,
        code: IntValue<'ctx>,
    ) -> Result<(), JitEvmEngineError> {
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

        ctx.builder.build_store(result_code_ptr, code)?;

        let gas_used_ptr =
            ctx.builder
                .build_struct_gep(ctx.types.execution_result, ptr, 1, "gas_used_offset")?;

        let gas_limit = TransactionContext::gas_limit(ctx, book.execution_context)?;
        ctx.builder.build_store(gas_used_ptr, gas_limit)?;

        ctx.builder.build_return(None)?;
        Ok(())
    }

    pub fn build_exit_return(
        ctx: &BuilderContext<'ctx>,
        book: &JitEvmEngineBookkeeping<'ctx>,
        offset: IntValue<'ctx>,
        size: IntValue<'ctx>,
    ) -> Result<(), JitEvmEngineError> {
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

        let code = JitContractResultCode::SuccessReturn;
        let code = ctx.types.type_i32.const_int(u32::from(code) as u64, false);
        ctx.builder.build_store(result_code, code)?;

        let gas_used_ptr =
            ctx.builder
                .build_struct_gep(ctx.types.execution_result, ptr, 1, "gas_used_offset")?;

        let gas_limit = TransactionContext::gas_limit(ctx, book.execution_context)?;
        let gas_used = ctx
            .builder
            .build_int_sub(gas_limit, book.gas_remaining, "calc_gas_used")?;
        ctx.builder.build_store(gas_used_ptr, gas_used)?;

        let gas_refund_ptr = ctx.builder.build_struct_gep(
            ctx.types.execution_result,
            ptr,
            2,
            "gas_refund_offset",
        )?;
        ctx.builder.build_store(gas_refund_ptr, book.gas_refund)?;

        let return_data_ptr = ctx.builder.build_struct_gep(
            ctx.types.execution_result,
            ptr,
            3,
            "return_ptr_offset",
        )?;

        let offset = ctx.builder.build_int_cast(offset, ctx.types.type_i64, "")?;
        let return_data = ctx
            .builder
            .build_int_add(book.mem, offset, "return_data_mem")?;
        ctx.builder.build_store(return_data_ptr, return_data)?;

        let return_len_ptr = ctx.builder.build_struct_gep(
            ctx.types.execution_result,
            ptr,
            4,
            "return_len_offset",
        )?;
        ctx.builder.build_store(return_len_ptr, size)?;

        // TODO: push-1 on the calling stack?

        ctx.builder.build_return(None)?;
        Ok(())
    }

    pub fn build_exit_stop(
        ctx: &BuilderContext<'ctx>,
        book: &JitEvmEngineBookkeeping<'ctx>,
    ) -> Result<(), JitEvmEngineError> {
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

        let code = JitContractResultCode::SuccessStop;
        let code = ctx.types.type_i32.const_int(u32::from(code) as u64, false);
        ctx.builder.build_store(result_code, code)?;

        let gas_used_ptr =
            ctx.builder
                .build_struct_gep(ctx.types.execution_result, ptr, 1, "gas_used_offset")?;

        let gas_limit = TransactionContext::gas_limit(ctx, book.execution_context)?;
        let gas_used = ctx
            .builder
            .build_int_sub(gas_limit, book.gas_remaining, "calc_gas_used")?;
        ctx.builder.build_store(gas_used_ptr, gas_used)?;

        let gas_refund_ptr = ctx.builder.build_struct_gep(
            ctx.types.execution_result,
            ptr,
            2,
            "gas_refund_offset",
        )?;
        ctx.builder.build_store(gas_refund_ptr, book.gas_refund)?;

        ctx.builder.build_return(None)?;
        Ok(())
    }

    pub fn build_exit_revert(
        ctx: &BuilderContext<'ctx>,
        book: &JitEvmEngineBookkeeping<'ctx>,
        offset: IntValue<'ctx>,
        size: IntValue<'ctx>,
    ) -> Result<(), JitEvmEngineError> {
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

        let code = JitContractResultCode::Revert;
        let code = ctx.types.type_i32.const_int(u32::from(code) as u64, false);
        ctx.builder.build_store(result_code, code)?;

        let gas_used_ptr =
            ctx.builder
                .build_struct_gep(ctx.types.execution_result, ptr, 1, "gas_used_offset")?;

        let gas_limit = TransactionContext::gas_limit(ctx, book.execution_context)?;
        let gas_used = ctx
            .builder
            .build_int_sub(gas_limit, book.gas_remaining, "calc_gas_used")?;
        ctx.builder.build_store(gas_used_ptr, gas_used)?;

        let return_data_ptr = ctx.builder.build_struct_gep(
            ctx.types.execution_result,
            ptr,
            3,
            "return_ptr_offset",
        )?;

        let offset = ctx.builder.build_int_cast(offset, ctx.types.type_i64, "")?;
        let return_data = ctx
            .builder
            .build_int_add(book.mem, offset, "return_data_mem")?;
        ctx.builder.build_store(return_data_ptr, return_data)?;

        let return_len_ptr = ctx.builder.build_struct_gep(
            ctx.types.execution_result,
            ptr,
            4,
            "return_len_offset",
        )?;
        ctx.builder.build_store(return_len_ptr, size)?;

        // TODO: push-0 on the calling stack?

        ctx.builder.build_return(None)?;
        Ok(())
    }
}
