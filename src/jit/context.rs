use crate::constants::*;
use inkwell::{context::Context, targets::TargetData, AddressSpace};
use primitive_types::U256;
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
    // TODO: block_context & tx_context....
}

// TODO: something like this, but for block_context & tx_context accessing state
//impl<'ctx> JitEvmPtrs {
//    /// Generate llvm type definition mirroring JitEvmPtrs layout.
//    pub fn build_llvm_struct(target: &TargetData, context: &'ctx Context) -> inkwell::types::StructType<'ctx> {
//	    context.struct_type(&[
//		    context.ptr_sized_int_type(target, None).ptr_type(AddressSpace::Generic).into(), // stack
//		    context.ptr_sized_int_type(target, None).ptr_type(AddressSpace::Generic).into(), // memory
//		    context.ptr_sized_int_type(target, None).ptr_type(AddressSpace::Generic).into(), // storage
//		], false)
//	}
//}

impl JitEvmPtrs {
    pub fn from_raw(ptr: usize) -> JitEvmPtrs {
        unsafe { *(ptr as *mut _) }
    }

    pub fn from_context(ctx: &mut JitEvmExecutionContext) -> JitEvmPtrs {
        JitEvmPtrs {
            stack: ctx.stack.as_mut_ptr() as usize,
            memory: ctx.memory.as_mut_ptr() as usize,
            storage: &mut ctx.storage as *mut _ as usize,
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

    // TODO: panics on stack bounds
    pub fn stack(&self, sp: usize, offset: usize) -> &U256 {
        unsafe { &*((sp - offset * EVM_STACK_ELEMENT_SIZE as usize) as *const _) }
    }

    pub fn stack_mut(&self, sp: usize, offset: usize) -> &mut U256 {
        unsafe { &mut *((sp - offset * EVM_STACK_ELEMENT_SIZE as usize) as *mut _) }
    }
}

#[derive(Debug, Clone)]
pub struct JitEvmExecutionContext {
    pub stack: Vec<U256>,
    pub memory: Vec<u8>,
    pub storage: HashMap<U256, U256>,
}

impl JitEvmExecutionContext {
    pub fn new() -> Self {
        Self {
            stack: vec![U256::zero(); EVM_STACK_SIZE],
            memory: vec![0u8; EVM_MEMORY_BYTES],
            storage: HashMap::<U256, U256>::new(),
        }
    }
}
