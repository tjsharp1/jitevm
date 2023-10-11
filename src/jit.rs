use crate::constants::*;
use crate::{
    code::{EvmOp, IndexedEvmCode},
    jit::context::BlockContext,
};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::values::{IntValue, PhiValue};
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;

#[cfg(test)]
mod test;

mod cursor;
#[macro_use]
mod jump;
#[macro_use]
mod bytes;
mod arithmetic;
mod context;
mod error;
mod ffi;
mod memory;
#[macro_use]
mod stack;
mod types;

use cursor::LendingIterator;
use ffi::*;

pub use context::JitEvmExecutionContext;
use context::JitEvmPtrs;
pub use error::JitEvmEngineError;
use types::JitTypes;

pub type JitEvmCompiledContract = unsafe extern "C" fn(usize) -> u64;

#[derive(Debug, Copy, Clone)]
pub struct JitEvmEngineBookkeeping<'ctx> {
    pub execution_context: IntValue<'ctx>,
    pub sp_min: IntValue<'ctx>,
    pub sp_max: IntValue<'ctx>,
    pub gas_remaining: IntValue<'ctx>,
    pub sp: IntValue<'ctx>,
    pub mem: IntValue<'ctx>,
}

impl<'ctx> JitEvmEngineBookkeeping<'ctx> {
    pub fn update_sp(&self, sp: IntValue<'ctx>) -> Self {
        Self { sp, ..*self }
    }

    pub fn update_gas(&self, gas_remaining: IntValue<'ctx>) -> Self {
        Self {
            gas_remaining,
            ..*self
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct JitEvmEngineSimpleBlock<'ctx> {
    pub block: BasicBlock<'ctx>,
    pub phi_execution_context: PhiValue<'ctx>,
    pub phi_sp_min: PhiValue<'ctx>,
    pub phi_sp_max: PhiValue<'ctx>,
    pub phi_gas_remaining: PhiValue<'ctx>,
    pub phi_sp: PhiValue<'ctx>,
    pub phi_mem: PhiValue<'ctx>,
}

impl<'ctx> JitEvmEngineSimpleBlock<'ctx> {
    pub fn new(
        ctx: &OperationsContext<'ctx>,
        block_before: BasicBlock<'ctx>,
        name: &str,
        suffix: &str,
    ) -> Result<Self, JitEvmEngineError> {
        let block = ctx.context.insert_basic_block_after(block_before, name);
        ctx.builder.position_at_end(block);
        let phi_execution_context = ctx
            .builder
            .build_phi(ctx.types.type_i64, &format!("execution_context{}", suffix))?;
        let phi_sp_min = ctx
            .builder
            .build_phi(ctx.types.type_i64, &format!("sp_min{}", suffix))?;
        let phi_sp_max = ctx
            .builder
            .build_phi(ctx.types.type_i64, &format!("sp_max{}", suffix))?;
        let phi_gas_remaining = ctx
            .builder
            .build_phi(ctx.types.type_i64, &format!("gas_remaining{}", suffix))?;
        let phi_sp = ctx
            .builder
            .build_phi(ctx.types.type_i64, &format!("sp{}", suffix))?;
        let phi_mem = ctx
            .builder
            .build_phi(ctx.types.type_i64, &format!("mem{}", suffix))?;

        Ok(Self {
            block,
            phi_execution_context,
            phi_sp_min,
            phi_sp_max,
            phi_gas_remaining,
            phi_sp,
            phi_mem,
        })
    }

    pub fn book(&self) -> JitEvmEngineBookkeeping<'ctx> {
        JitEvmEngineBookkeeping {
            execution_context: self.phi_execution_context.as_basic_value().into_int_value(),
            sp_min: self.phi_sp_min.as_basic_value().into_int_value(),
            sp_max: self.phi_sp_max.as_basic_value().into_int_value(),
            gas_remaining: self.phi_gas_remaining.as_basic_value().into_int_value(),
            sp: self.phi_sp.as_basic_value().into_int_value(),
            mem: self.phi_mem.as_basic_value().into_int_value(),
        }
    }

    pub fn phi_setup_block(&self, book: &JitEvmEngineBookkeeping<'ctx>, prev: &BasicBlock<'ctx>) {
        self.phi_execution_context
            .add_incoming(&[(&book.execution_context, *prev)]);
        self.phi_sp_min.add_incoming(&[(&book.sp_min, *prev)]);
        self.phi_sp_max.add_incoming(&[(&book.sp_max, *prev)]);
        self.phi_gas_remaining
            .add_incoming(&[(&book.gas_remaining, *prev)]);
        self.phi_sp.add_incoming(&[(&book.sp, *prev)]);
        self.phi_mem.add_incoming(&[(&book.mem, *prev)]);
    }

    pub fn add_incoming(
        &self,
        book: &JitEvmEngineBookkeeping<'ctx>,
        prev: &JitEvmEngineSimpleBlock<'ctx>,
    ) {
        self.phi_execution_context
            .add_incoming(&[(&book.execution_context, prev.block)]);
        self.phi_sp_min.add_incoming(&[(&book.sp_min, prev.block)]);
        self.phi_sp_max.add_incoming(&[(&book.sp_max, prev.block)]);
        self.phi_gas_remaining
            .add_incoming(&[(&book.gas_remaining, prev.block)]);
        self.phi_sp.add_incoming(&[(&book.sp, prev.block)]);
        self.phi_mem.add_incoming(&[(&book.mem, prev.block)]);
    }
}

#[derive(Debug, Clone)]
pub struct JitEvmContract<'ctx> {
    //context: &'ctx Context,
    // NOTE: will likely need the module, if linking contract calls via llvm
    //module: Module<'ctx>,
    //execution_engine: ExecutionEngine<'ctx>,
    function: JitFunction<'ctx, JitEvmCompiledContract>,
}

impl<'ctx> JitEvmContract<'ctx> {
    pub fn call(&self, context: &mut JitEvmExecutionContext) -> Result<u64, JitEvmEngineError> {
        unsafe {
            let mut ptrs = JitEvmPtrs::from_context(context);
            Ok(self.function.call(&mut ptrs as *mut _ as usize))
        }
    }
}

pub struct OperationsContext<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub types: JitTypes<'ctx>,
}

pub struct JitContractBuilder<'ctx> {
    pub ctx: OperationsContext<'ctx>,
    pub debug_ir: Option<String>,
    pub debug_asm: Option<String>,
    pub host_functions: HostFunctions<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> JitContractBuilder<'ctx> {
    pub fn with_context(name: &str, context: &'ctx Context) -> Result<Self, JitEvmEngineError> {
        Target::initialize_native(&InitializationConfig::default())?;

        let module = context.create_module(name);
        let builder = context.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;

        let types = JitTypes::new(&context, &execution_engine);
        let host_functions = HostFunctions::new(types.clone(), &module, &execution_engine);

        let ctx = OperationsContext {
            context,
            module,
            builder,
            types,
        };

        Ok(Self {
            ctx,
            execution_engine,
            debug_ir: None,
            debug_asm: None,
            host_functions,
        })
    }

    pub fn debug_ir(mut self, filename: &str) -> Self {
        self.debug_ir = Some(filename.into());
        self
    }

    pub fn debug_asm(mut self, filename: &str) -> Self {
        self.debug_asm = Some(filename.into());
        self
    }

    pub fn build(self, code: IndexedEvmCode) -> Result<JitEvmContract<'ctx>, JitEvmEngineError> {
        let JitContractBuilder {
            ctx,
            execution_engine,
            debug_ir,
            debug_asm,
            host_functions,
        } = self;

        let cursor = cursor::InstructionCursor::new(&ctx, code)?;
        let mut iter = cursor.iter();

        // RENDER INSTRUCTIONS

        while let Some(current) = iter.next() {
            use EvmOp::*;

            ctx.builder.position_at_end(current.block().block);

            match current.op() {
                Stop => jump::build_stop_op(&ctx, current)?,
                Push(_, val) => stack::build_push_op(&ctx, current, val)?,
                Pop => stack::build_pop_op(&ctx, current)?,
                Jumpdest => jump::build_jumpdest_op(&ctx, current)?,
                Mstore => memory::build_mstore_op(&ctx, current)?,
                Mstore8 => memory::build_mstore8_op(&ctx, current)?,
                Mload => memory::build_mload_op(&ctx, current)?,
                Sload => host_functions.build_sload(&ctx, current)?,
                Sstore => host_functions.build_sstore(&ctx, current)?,
                Sha3 => host_functions.build_sha3(&ctx, current)?,
                Jump => jump::build_jump_op(&ctx, current)?,
                Jumpi => jump::build_jumpi_op(&ctx, current)?,
                Swap1 => stack::build_stack_swap_op(&ctx, current, 1)?,
                Swap2 => stack::build_stack_swap_op(&ctx, current, 2)?,
                Swap3 => stack::build_stack_swap_op(&ctx, current, 3)?,
                Swap4 => stack::build_stack_swap_op(&ctx, current, 4)?,
                Swap5 => stack::build_stack_swap_op(&ctx, current, 5)?,
                Swap6 => stack::build_stack_swap_op(&ctx, current, 6)?,
                Swap7 => stack::build_stack_swap_op(&ctx, current, 7)?,
                Swap8 => stack::build_stack_swap_op(&ctx, current, 8)?,
                Swap9 => stack::build_stack_swap_op(&ctx, current, 9)?,
                Swap10 => stack::build_stack_swap_op(&ctx, current, 10)?,
                Swap11 => stack::build_stack_swap_op(&ctx, current, 11)?,
                Swap12 => stack::build_stack_swap_op(&ctx, current, 12)?,
                Swap13 => stack::build_stack_swap_op(&ctx, current, 13)?,
                Swap14 => stack::build_stack_swap_op(&ctx, current, 14)?,
                Swap15 => stack::build_stack_swap_op(&ctx, current, 15)?,
                Swap16 => stack::build_stack_swap_op(&ctx, current, 16)?,
                Dup1 => stack::build_dup_op(&ctx, current, 1)?,
                Dup2 => stack::build_dup_op(&ctx, current, 2)?,
                Dup3 => stack::build_dup_op(&ctx, current, 3)?,
                Dup4 => stack::build_dup_op(&ctx, current, 4)?,
                Dup5 => stack::build_dup_op(&ctx, current, 5)?,
                Dup6 => stack::build_dup_op(&ctx, current, 6)?,
                Dup7 => stack::build_dup_op(&ctx, current, 7)?,
                Dup8 => stack::build_dup_op(&ctx, current, 8)?,
                Dup9 => stack::build_dup_op(&ctx, current, 9)?,
                Dup10 => stack::build_dup_op(&ctx, current, 10)?,
                Dup11 => stack::build_dup_op(&ctx, current, 11)?,
                Dup12 => stack::build_dup_op(&ctx, current, 12)?,
                Dup13 => stack::build_dup_op(&ctx, current, 13)?,
                Dup14 => stack::build_dup_op(&ctx, current, 14)?,
                Dup15 => stack::build_dup_op(&ctx, current, 15)?,
                Dup16 => stack::build_dup_op(&ctx, current, 16)?,
                Iszero => arithmetic::iszero_op(&ctx, current)?,
                Add => arithmetic::build_arithmetic_op(&ctx, current)?,
                Sub => arithmetic::build_arithmetic_op(&ctx, current)?,
                Mul => arithmetic::build_arithmetic_op(&ctx, current)?,
                Div => arithmetic::build_arithmetic_op(&ctx, current)?,
                Sdiv => arithmetic::build_arithmetic_op(&ctx, current)?,
                Mod => arithmetic::build_arithmetic_op(&ctx, current)?,
                Smod => arithmetic::build_arithmetic_op(&ctx, current)?,
                Shl => arithmetic::build_arithmetic_op(&ctx, current)?,
                Shr => arithmetic::build_arithmetic_op(&ctx, current)?,
                Sar => arithmetic::build_arithmetic_op(&ctx, current)?,
                And => arithmetic::build_arithmetic_op(&ctx, current)?,
                Or => arithmetic::build_arithmetic_op(&ctx, current)?,
                Xor => arithmetic::build_arithmetic_op(&ctx, current)?,
                Exp => arithmetic::build_exp_op(&ctx, current)?,
                Eq => arithmetic::build_cmp_op(&ctx, current, IntPredicate::EQ)?,
                Lt => arithmetic::build_cmp_op(&ctx, current, IntPredicate::ULT)?,
                Gt => arithmetic::build_cmp_op(&ctx, current, IntPredicate::UGT)?,
                Slt => arithmetic::build_cmp_op(&ctx, current, IntPredicate::SLT)?,
                Sgt => arithmetic::build_cmp_op(&ctx, current, IntPredicate::SGT)?,
                Not => arithmetic::build_not_op(&ctx, current)?,
                Byte => arithmetic::build_byte_op(&ctx, current)?,
                Addmod => arithmetic::build_mod_op(&ctx, current)?,
                Mulmod => arithmetic::build_mod_op(&ctx, current)?,
                Signextend => arithmetic::build_signextend_op(&ctx, current)?,
                GasLimit => BlockContext::build_get_gas_limit(&ctx, current)?,
                BaseFee => BlockContext::build_get_basefee(&ctx, current)?,
                PrevRandao => BlockContext::build_get_randao(&ctx, current)?,
                Timestamp => BlockContext::build_get_timestamp(&ctx, current)?,
                Coinbase => BlockContext::build_get_coinbase(&ctx, current)?,
                Number => BlockContext::build_get_number(&ctx, current)?,
                AugmentedPushJump(_, val) => jump::build_augmented_jump_op(&ctx, current, val)?,
                AugmentedPushJumpi(_, val) => jump::build_augmented_jumpi_op(&ctx, current, val)?,
                _ => {
                    panic!("Op not implemented: {:?}", current.op());
                }
            };
        }

        // OUTPUT LLVM
        if let Some(path) = debug_ir {
            ctx.module.print_to_file(path)?;
        }

        // OUTPUT ASM
        if let Some(path) = debug_asm {
            // https://github.com/TheDan64/inkwell/issues/184
            // https://thedan64.github.io/inkwell/inkwell/targets/struct.TargetMachine.html#method.write_to_file
            use inkwell::targets::{CodeModel, FileType, RelocMode, TargetMachine};

            let triple = TargetMachine::get_default_triple();
            let cpu = TargetMachine::get_host_cpu_name().to_string();
            let features = TargetMachine::get_host_cpu_features().to_string();

            let target = Target::from_triple(&triple).unwrap();
            let machine = target
                .create_target_machine(
                    &triple,
                    &cpu,
                    &features,
                    OptimizationLevel::Aggressive,
                    RelocMode::Default,
                    CodeModel::Default,
                )
                .unwrap();

            // create a module and do JIT stuff

            machine.write_to_file(&ctx.module, FileType::Assembly, path.as_ref())?;
        }

        // COMPILE
        let function: JitFunction<JitEvmCompiledContract> =
            unsafe { execution_engine.get_function("executecontract")? };
        Ok(JitEvmContract { function })
    }
}
