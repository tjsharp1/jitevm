use crate::jit::{
    context::JitEvmPtrs,
    contract::BuilderContext,
    cursor::Current,
    error::JitEvmEngineError,
    gas::{
        build_sha3_gas_check, build_sload_gas_check, build_sstore_gas_check, memory_expansion_cost,
        memory_gas, sha3_gas,
    },
    ops::{build_stack_inc, build_stack_pop},
    types::JitTypes,
};
use alloy_primitives::U256;
use inkwell::{
    execution_engine::ExecutionEngine,
    module::Module,
    values::{FunctionValue, IntValue},
    AddressSpace,
};
use revm_primitives::Spec;
use sha3::{Digest, Keccak256};

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
            .fn_type(&[types.type_i64.into(), types.type_bool.into()], false);
        let cb_print_u64 = module.add_function("callback_print_u64", cb_type, None);
        execution_engine.add_global_mapping(&cb_print_u64, callback_print_u64 as usize);

        let cb_type = types
            .type_void
            .fn_type(&[types.type_ptrint.into(), types.type_bool.into()], false);
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
        c: &BuilderContext<'ctx>,
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
        c: &BuilderContext<'ctx>,
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

    pub(crate) fn build_sha3<'a, SPEC: Spec>(
        &self,
        ctx: &BuilderContext<'ctx>,
        current: &mut Current<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let offset = build_stack_pop!(ctx, current);
        let size = build_stack_pop!(ctx, current);
        build_sha3_gas_check!(ctx, current, offset, size);

        let book = current.book_ref();

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
        build_stack_inc!(ctx, current);

        Ok(())
    }

    pub(crate) fn build_sload<'a>(
        &self,
        ctx: &BuilderContext<'ctx>,
        current: &mut Current<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book_ref();
        let gas = ctx
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

        build_sload_gas_check!(ctx, current, gas);

        Ok(())
    }

    pub(crate) fn build_sstore<'a>(
        &self,
        ctx: &BuilderContext<'ctx>,
        current: &mut Current<'a, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let book = current.book_ref();
        let _ = ctx
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

        let gas = build_stack_pop!(ctx, current);
        let refund = build_stack_pop!(ctx, current);

        build_sstore_gas_check!(ctx, current, gas, refund);

        Ok(())
    }
}

pub extern "C" fn callback_sha3(exectx: usize, sp: usize, ptr: usize, size: usize) {
    let rawptrs = JitEvmPtrs::from_raw(exectx);

    let hash = Keccak256::digest(rawptrs.mem_slice(ptr, size));

    *rawptrs.stack_mut(sp, 0) = U256::try_from_be_slice(hash.as_slice()).expect("No bytes");
}

pub extern "C" fn callback_print_u64(item: u64, hex: bool) {
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

    let (val, gas) = rawptrs.sload(key);
    *key = val;

    gas
}

pub extern "C" fn callback_sstore(exectx: usize, sp: usize) -> u64 {
    let rawptrs = JitEvmPtrs::from_raw(exectx);

    let key: &mut U256 = rawptrs.stack_mut(sp, 1);
    let value: &mut U256 = rawptrs.stack_mut(sp, 2);

    let (gas, refund) = rawptrs.sstore(*key, *value);

    *key = U256::from(gas);
    *value = U256::from(refund);

    0
}
