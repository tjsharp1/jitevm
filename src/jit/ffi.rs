use crate::jit::{
    cursor::CurrentInstruction,
    error::JitEvmEngineError,
    stack::{build_stack_inc, build_stack_pop},
    types::JitTypes,
    JitEvmEngineBookkeeping, JitEvmPtrs, OperationsContext,
};
use hex_literal::hex;
use inkwell::{
    execution_engine::ExecutionEngine,
    module::Module,
    values::{FunctionValue, IntValue},
    AddressSpace,
};
use primitive_types::U256;
use sha3::{Digest, Keccak256};

// TODO: finish with this
pub const KECCAK_EMPTY: [u8; 32] =
    hex!("c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470");

pub struct HostFunctions<'ctx> {
    cb_print_u64: FunctionValue<'ctx>,
    cb_print_u256: FunctionValue<'ctx>,
    callback_sha3_func: FunctionValue<'ctx>,
    callback_sload_func: FunctionValue<'ctx>,
    callback_sstore_func: FunctionValue<'ctx>,
}

impl<'ctx> HostFunctions<'ctx> {
    pub fn new(
        types: JitTypes<'ctx>,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine,
    ) -> HostFunctions<'ctx> {
        let cb_type = types
            .type_void
            .fn_type(&[types.type_ptrint.into(), types.type_bool.into()], false);
        let cb_print_u64 = module.add_function("callback_print_u64", cb_type, None);
        execution_engine.add_global_mapping(&cb_print_u64, callback_print_u64 as usize);

        let cb_print_u256 = module.add_function("callback_print_u256", cb_type, None);
        execution_engine.add_global_mapping(&cb_print_u256, callback_print_u256 as usize);

        let callback_sha3_func = {
            // SHA3
            let cb_type = types.type_void.fn_type(
                &[
                    types.type_ptrint.into(),
                    types.type_ptrint.into(),
                    types.type_ptrint.into(),
                    types.type_ptrint.into(),
                ],
                false,
            );
            let cb_func = module.add_function("callback_sha3", cb_type, None);
            execution_engine.add_global_mapping(&cb_func, callback_sha3 as usize);
            cb_func
        };

        let callback_sload_func = {
            // SLOAD
            let cb_type = types
                .type_retval
                .fn_type(&[types.type_ptrint.into(), types.type_ptrint.into()], false);
            let cb_func = module.add_function("callback_sload", cb_type, None);
            execution_engine.add_global_mapping(&cb_func, callback_sload as usize);
            cb_func
        };

        let callback_sstore_func = {
            // SSTORE
            let cb_type = types
                .type_retval
                .fn_type(&[types.type_ptrint.into(), types.type_ptrint.into()], false);
            let cb_func = module.add_function("callback_sstore", cb_type, None);
            execution_engine.add_global_mapping(&cb_func, callback_sstore as usize);
            cb_func
        };

        HostFunctions {
            cb_print_u64,
            cb_print_u256,
            callback_sha3_func,
            callback_sload_func,
            callback_sstore_func,
        }
    }

    pub fn build_print_u64(
        &self,
        c: &OperationsContext<'ctx>,
        val: IntValue<'ctx>,
        hex: bool,
    ) -> Result<(), JitEvmEngineError> {
        let ptr = c.builder.build_alloca(c.types.type_stackel, "")?;
        c.builder.build_store(ptr, val)?;

        let hex = c.types.type_bool.const_int(hex as u64, false);
        c.builder
            .build_call(self.cb_print_u64, &[ptr.into(), hex.into()], "")?;

        Ok(())
    }

    pub fn build_print_u256(
        &self,
        c: &OperationsContext<'ctx>,
        val: IntValue<'ctx>,
        hex: bool,
    ) -> Result<(), JitEvmEngineError> {
        let ptr = c.builder.build_alloca(c.types.type_stackel, "")?;
        c.builder.build_store(ptr, val)?;

        let hex = c.types.type_bool.const_int(hex as u64, false);
        c.builder
            .build_call(self.cb_print_u256, &[ptr.into(), hex.into()], "")?;

        Ok(())
    }

    pub(crate) fn build_sha3<'a>(
        &self,
        ctx: &OperationsContext<'ctx>,
        current: &CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book();

        let (book, offset) = build_stack_pop!(ctx, book);
        let (book, size) = build_stack_pop!(ctx, book);

        let offset = ctx
            .builder
            .build_int_cast(offset, ctx.types.type_ptrint, "")?;
        let size = ctx
            .builder
            .build_int_cast(size, ctx.types.type_ptrint, "")?;

        ctx.builder.build_call(
            self.callback_sha3_func,
            &[
                book.execution_context.into(),
                book.sp.into(),
                offset.into(),
                size.into(),
            ],
            "",
        )?;
        let book = build_stack_inc!(ctx, book);

        jump_next!(book, ctx, current);
        Ok(())
    }

    pub(crate) fn build_sload<'a>(
        &self,
        ctx: &OperationsContext<'ctx>,
        current: &CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book();

        let _retval = ctx
            .builder
            .build_call(
                self.callback_sload_func,
                &[book.execution_context.into(), book.sp.into()],
                "",
            )?
            .try_as_basic_value()
            .left()
            .ok_or(JitEvmEngineError::NoInstructionValue)?
            .into_int_value();

        jump_next!(book, ctx, current);
        Ok(())
    }

    pub(crate) fn build_sstore<'a>(
        &self,
        ctx: &OperationsContext<'ctx>,
        current: &CurrentInstruction<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book();

        let _retval = ctx
            .builder
            .build_call(
                self.callback_sstore_func,
                &[book.execution_context.into(), book.sp.into()],
                "",
            )?
            .try_as_basic_value()
            .left()
            .ok_or(JitEvmEngineError::NoInstructionValue)?
            .into_int_value();
        let (book, _) = build_stack_pop!(ctx, book);
        let (book, _) = build_stack_pop!(ctx, book);

        jump_next!(book, ctx, current);
        Ok(())
    }
}

pub extern "C" fn callback_sha3(exectx: usize, sp: usize, ptr: usize, size: usize) {
    let rawptrs = JitEvmPtrs::from_raw(exectx);

    let hash = Keccak256::digest(rawptrs.mem_slice(ptr, size));

    *rawptrs.stack_mut(sp, 0) = U256::from(hash.as_slice());
}

pub extern "C" fn callback_print_u64(ptr: usize, hex: bool) {
    let item: &u64 = unsafe { &*(ptr as *const _) };
    if hex {
        println!("U64 value 0x{:x}", item);
    } else {
        println!("U64 value {}", item);
    }
}

pub extern "C" fn callback_print_u256(ptr: usize, hex: bool) {
    let item: &U256 = unsafe { &*(ptr as *const _) };
    if hex {
        println!("U256 value 0x{:x}", item);
    } else {
        println!("U256 value {}", item);
    }
}

pub extern "C" fn callback_sload(exectx: usize, sp: usize) -> u64 {
    let rawptrs = JitEvmPtrs::from_raw(exectx);

    let key: &mut U256 = rawptrs.stack_mut(sp, 1);

    match rawptrs.storage_get(key) {
        Some(value) => {
            *key = *value;
        }
        None => {
            *key = U256::zero();
        }
    }

    0
}

pub extern "C" fn callback_sstore(exectx: usize, sp: usize) -> u64 {
    let rawptrs = JitEvmPtrs::from_raw(exectx);

    let key: &U256 = rawptrs.stack(sp, 1);
    let value: &U256 = rawptrs.stack(sp, 2);

    rawptrs.storage_insert(*key, *value);

    0
}
