use crate::jit::{
    error::JitEvmEngineError, types::JitTypes, JitContractBuilder, JitEvmEngineBookkeeping,
    JitEvmPtrs,
};
use hex_literal::hex;
use inkwell::{
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    types::{IntType, VoidType},
    values::{FunctionValue, IntValue},
};
use primitive_types::U256;
use sha3::{Digest, Keccak256};

pub const KECCAK_EMPTY: [u8; 32] =
    hex!("c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470");

pub struct HostFunctions<'ctx> {
    types: JitTypes<'ctx>,
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
            types,
            cb_print_u64,
            cb_print_u256,
            callback_sha3_func,
            callback_sload_func,
            callback_sstore_func,
        }
    }

    pub fn build_print_u64(
        &self,
        c: &JitContractBuilder<'ctx>,
        val: IntValue<'ctx>,
        hex: bool,
    ) -> Result<(), JitEvmEngineError> {
        let ptr = c.builder.build_alloca(self.types.type_stackel, "")?;
        c.builder.build_store(ptr, val)?;

        let hex = self.types.type_bool.const_int(hex as u64, false);
        c.builder
            .build_call(self.cb_print_u64, &[ptr.into(), hex.into()], "")?;

        Ok(())
    }

    pub fn build_print_u256(
        &self,
        c: JitContractBuilder<'ctx>,
        val: IntValue<'ctx>,
        hex: bool,
    ) -> Result<(), JitEvmEngineError> {
        let ptr = c.builder.build_alloca(self.types.type_stackel, "")?;
        c.builder.build_store(ptr, val)?;

        let hex = self.types.type_bool.const_int(hex as u64, false);
        c.builder
            .build_call(self.cb_print_u256, &[ptr.into(), hex.into()], "")?;

        Ok(())
    }

    pub fn build_sha3(
        &self,
        c: &JitContractBuilder<'ctx>,
        book: JitEvmEngineBookkeeping<'ctx>,
        offset: IntValue<'ctx>,
        size: IntValue<'ctx>,
    ) -> Result<JitEvmEngineBookkeeping<'ctx>, JitEvmEngineError> {
        let offset = c
            .builder
            .build_int_cast(offset, self.types.type_ptrint, "")?;
        let size = c.builder.build_int_cast(size, self.types.type_ptrint, "")?;

        c.builder.build_call(
            self.callback_sha3_func,
            &[
                book.execution_context.into(),
                book.sp.into(),
                offset.into(),
                size.into(),
            ],
            "",
        )?;

        Ok(book)
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
