use crate::constants::*;
use crate::jit::{
    cursor::CurrentInstruction,
    error::JitEvmEngineError,
    stack::{build_stack_check, build_stack_push},
    JitEvmEngineBookkeeping, OperationsContext,
};
use inkwell::{
    context::Context, targets::TargetData, types::StructType, values::PointerValue, AddressSpace,
};
use primitive_types::{H160, H256, U256};
use std::collections::HashMap;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub(in crate::jit) struct JitEvmPtrs {
    // WARNING: if you change anything here (adding fields is ok), then you need to change:
    //           - LLVM instructions in "setup" block of "executecontract" function
    //           - JitContractBuilder::callback_sload, JitContractBuilder::callback_sstore, ...
    //           - possibly other code! => try not to change this!
    // TODO: these are really all pointers
    pub stack: usize,
    pub memory: usize,
    pub storage: usize,
    pub block_context: usize,
    // TODO: also tx_context..
}

impl<'ctx> JitEvmPtrs {
    pub fn llvm_struct_type(ctx: &'ctx Context, target_data: &TargetData) -> StructType<'ctx> {
        let ptr_type = ctx
            .ptr_sized_int_type(target_data, None)
            .ptr_type(AddressSpace::default());

        let fields = vec![
            ptr_type.into(), // stack
            ptr_type.into(), // memory
            ptr_type.into(), // storage
            ptr_type.into(), // block_context
        ];
        ctx.struct_type(&fields, false)
    }

    pub fn build_get_block_context_ptr(
        ctx: &OperationsContext<'ctx>,
        book: &JitEvmEngineBookkeeping<'ctx>,
    ) -> Result<PointerValue<'ctx>, JitEvmEngineError> {
        let ptr = ctx.builder.build_int_to_ptr(
            book.execution_context,
            ctx.types
                .execution_context
                .ptr_type(AddressSpace::default()),
            "",
        )?;

        let offset = ctx.builder.build_struct_gep(
            ctx.types.execution_context,
            ptr,
            3,
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
            storage: &mut ctx.storage as *mut _ as usize,
            block_context: &mut ctx.block_context as *mut _ as usize,
        }
    }

    pub fn storage_get(&self, key: &U256) -> Option<&U256> {
        let storage: &mut HashMap<U256, U256> = unsafe { &mut *(self.storage as *mut _) };

        storage.get(key)
    }

    pub fn storage_insert(&self, key: U256, value: U256) -> Option<U256> {
        let storage: &mut HashMap<U256, U256> = unsafe { &mut *(self.storage as *mut _) };
        storage.insert(key, value)
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

#[repr(C)]
#[derive(Clone, Debug, Default)]
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
    pub fn set_number(&mut self, number: U256) {
        self.number = number;
    }

    pub fn set_coinbase(&mut self, coinbase: H160) {
        let mut tmp = [0u8; 32];
        tmp[12..].copy_from_slice(coinbase.as_bytes());
        self.coinbase = U256::from_big_endian(&tmp);
    }

    pub fn set_timestamp(&mut self, timestamp: U256) {
        self.timestamp = timestamp;
    }

    pub fn set_difficulty(&mut self, difficulty: U256) {
        self.difficulty = difficulty;
    }

    pub fn set_prevrandao(&mut self, prevrandao: H256) {
        self.prevrandao = U256::from_big_endian(prevrandao.as_bytes());
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
        ctx: &OperationsContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book();
        build_stack_check!(ctx, current, book, 0, 1);

        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, &book)?;
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
        ctx: &OperationsContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book();
        build_stack_check!(ctx, current, book, 0, 1);

        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, &book)?;
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
        ctx: &OperationsContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book();
        build_stack_check!(ctx, current, book, 0, 1);

        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, &book)?;
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
        ctx: &OperationsContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book();
        build_stack_check!(ctx, current, book, 0, 1);

        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, &book)?;
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
        ctx: &OperationsContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book();
        build_stack_check!(ctx, current, book, 0, 1);

        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, &book)?;
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
        ctx: &OperationsContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book();
        build_stack_check!(ctx, current, book, 0, 1);

        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, &book)?;
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
        ctx: &OperationsContext<'ctx>,
        current: &mut CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book();
        build_stack_check!(ctx, current, book, 0, 1);

        let ptr = JitEvmPtrs::build_get_block_context_ptr(&ctx, &book)?;
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

#[derive(Debug, Clone)]
pub struct JitEvmExecutionContext {
    pub stack: Vec<U256>,
    pub memory: Vec<u8>,
    pub storage: HashMap<U256, U256>,
    pub block_context: BlockContext,
}

impl JitEvmExecutionContext {
    pub fn new() -> Self {
        Self {
            stack: vec![U256::zero(); EVM_STACK_SIZE],
            memory: vec![0u8; EVM_MEMORY_BYTES],
            storage: HashMap::<U256, U256>::new(),
            block_context: Default::default(),
        }
    }
}
