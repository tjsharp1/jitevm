use crate::{
    code::{EvmCode, EvmOp, EvmOpParserMode},
    jit::{
        self,
        context::{BlockContext, JitContractExecutionResult, JitEvmPtrs, TransactionContext},
        cursor::{self, LendingIterator},
        error::JitEvmEngineError,
        gas::init_gas,
        ops,
        tracing::{TraceData, Tracers, TracingConfig},
        types::JitTypes,
        ExecutionResult,
    },
};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::values::{IntValue, PhiValue};
use inkwell::OptimizationLevel;
use revm_primitives::{Bytes, Spec};

pub type JitEvmCompiledContract = unsafe extern "C" fn(usize, usize, u64) -> ();
pub type JitEvmFetchTracing = unsafe extern "C" fn(usize, usize) -> ();

#[derive(Debug, Copy, Clone)]
pub struct JitEvmEngineBookkeeping<'ctx> {
    pub execution_context: IntValue<'ctx>,
    pub sp_min: IntValue<'ctx>,
    pub sp_max: IntValue<'ctx>,
    pub gas_remaining: IntValue<'ctx>,
    pub gas_refund: IntValue<'ctx>,
    pub sp: IntValue<'ctx>,
    pub mem: IntValue<'ctx>,
    pub mem_size: IntValue<'ctx>,
    pub mem_gas: IntValue<'ctx>,
}

impl<'ctx> JitEvmEngineBookkeeping<'ctx> {
    pub fn update_mem_gas(&mut self, mem_gas: IntValue<'ctx>, mem_size: IntValue<'ctx>) {
        self.mem_gas = mem_gas;
        self.mem_size = mem_size;
    }

    pub fn update_sp(&mut self, sp: IntValue<'ctx>) {
        self.sp = sp;
    }

    pub fn update_gas(&mut self, gas_remaining: IntValue<'ctx>) {
        self.gas_remaining = gas_remaining;
    }

    pub fn update_refund(&mut self, gas_refund: IntValue<'ctx>) {
        self.gas_refund = gas_refund;
    }
}

#[derive(Debug, Copy, Clone)]
pub struct JitEvmEngineSimpleBlock<'ctx> {
    pub block: BasicBlock<'ctx>,
    pub phi_execution_context: PhiValue<'ctx>,
    pub phi_sp_min: PhiValue<'ctx>,
    pub phi_sp_max: PhiValue<'ctx>,
    pub phi_gas_remaining: PhiValue<'ctx>,
    pub phi_gas_refund: PhiValue<'ctx>,
    pub phi_sp: PhiValue<'ctx>,
    pub phi_mem: PhiValue<'ctx>,
    pub phi_mem_size: PhiValue<'ctx>,
    pub phi_mem_gas: PhiValue<'ctx>,
}

impl<'ctx> JitEvmEngineSimpleBlock<'ctx> {
    pub fn new(
        ctx: &BuilderContext<'ctx>,
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
        let phi_gas_refund = ctx
            .builder
            .build_phi(ctx.types.type_i64, &format!("gas_refund{}", suffix))?;
        let phi_sp = ctx
            .builder
            .build_phi(ctx.types.type_i64, &format!("sp{}", suffix))?;
        let phi_mem = ctx
            .builder
            .build_phi(ctx.types.type_i64, &format!("mem{}", suffix))?;
        let phi_mem_size = ctx
            .builder
            .build_phi(ctx.types.type_i64, &format!("mem_size{}", suffix))?;
        let phi_mem_gas = ctx
            .builder
            .build_phi(ctx.types.type_i64, &format!("mem_gas{}", suffix))?;

        Ok(Self {
            block,
            phi_execution_context,
            phi_sp_min,
            phi_sp_max,
            phi_gas_remaining,
            phi_gas_refund,
            phi_sp,
            phi_mem,
            phi_mem_size,
            phi_mem_gas,
        })
    }

    pub fn book(&self) -> JitEvmEngineBookkeeping<'ctx> {
        JitEvmEngineBookkeeping {
            execution_context: self.phi_execution_context.as_basic_value().into_int_value(),
            sp_min: self.phi_sp_min.as_basic_value().into_int_value(),
            sp_max: self.phi_sp_max.as_basic_value().into_int_value(),
            gas_remaining: self.phi_gas_remaining.as_basic_value().into_int_value(),
            gas_refund: self.phi_gas_refund.as_basic_value().into_int_value(),
            sp: self.phi_sp.as_basic_value().into_int_value(),
            mem: self.phi_mem.as_basic_value().into_int_value(),
            mem_size: self.phi_mem_size.as_basic_value().into_int_value(),
            mem_gas: self.phi_mem_gas.as_basic_value().into_int_value(),
        }
    }

    pub fn phi_setup_block(&self, book: &JitEvmEngineBookkeeping<'ctx>, prev: &BasicBlock<'ctx>) {
        self.phi_execution_context
            .add_incoming(&[(&book.execution_context, *prev)]);
        self.phi_sp_min.add_incoming(&[(&book.sp_min, *prev)]);
        self.phi_sp_max.add_incoming(&[(&book.sp_max, *prev)]);
        self.phi_gas_remaining
            .add_incoming(&[(&book.gas_remaining, *prev)]);
        self.phi_gas_refund
            .add_incoming(&[(&book.gas_refund, *prev)]);
        self.phi_sp.add_incoming(&[(&book.sp, *prev)]);
        self.phi_mem.add_incoming(&[(&book.mem, *prev)]);
        self.phi_mem_size.add_incoming(&[(&book.mem_size, *prev)]);
        self.phi_mem_gas.add_incoming(&[(&book.mem_gas, *prev)]);
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
        self.phi_gas_refund
            .add_incoming(&[(&book.gas_refund, prev.block)]);
        self.phi_sp.add_incoming(&[(&book.sp, prev.block)]);
        self.phi_mem.add_incoming(&[(&book.mem, prev.block)]);
        self.phi_mem_size
            .add_incoming(&[(&book.mem_size, prev.block)]);
        self.phi_mem_gas
            .add_incoming(&[(&book.mem_gas, prev.block)]);
    }
}

#[derive(Debug)]
pub struct JitEvmContract<'ctx, SPEC: Spec> {
    //context: &'ctx Context,
    // NOTE: will likely need the module, if linking contract calls via llvm
    //module: Module<'ctx>,
    tracers: Tracers<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    spec: SPEC,
    function: JitFunction<'ctx, JitEvmCompiledContract>,
    fetch_trace_data: Option<JitFunction<'ctx, JitEvmFetchTracing>>,
}

impl<'ctx, SPEC: Spec> JitEvmContract<'ctx, SPEC> {
    pub fn transact(
        &self,
        context: &mut jit::JitEvmExecutionContext<SPEC>,
    ) -> Result<(ExecutionResult, Option<Vec<TraceData>>), JitEvmEngineError> {
        let init_gas = init_gas::<SPEC>(context.calldata());

        unsafe {
            let mut ptrs = JitEvmPtrs::from_context(context);
            let mut result = JitContractExecutionResult::new();

            self.function.call(
                &mut ptrs as *mut _ as usize,
                &mut result as *mut _ as usize,
                init_gas,
            );

            if self.tracers.is_empty() {
                Ok((ExecutionResult::from(result), None))
            } else {
                let mut data = vec![TraceData::Empty; self.tracers.len()];

                self.fetch_trace_data
                    .as_ref()
                    .expect("Tracer retreival fn should exist!")
                    .call(data.as_mut_ptr() as usize, data.len());

                Ok((ExecutionResult::from(result), Some(data)))
            }
        }
    }
}

pub struct BuilderContext<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub types: JitTypes<'ctx>,
    pub tracers: Tracers<'ctx>,
}

pub struct JitContractBuilder<'ctx> {
    pub ctx: BuilderContext<'ctx>,
    pub debug_ir: Option<String>,
    pub debug_asm: Option<String>,
    pub debug_obj: Option<String>,
    pub tracing_options: TracingConfig,
    pub host_functions: jit::HostFunctions<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> JitContractBuilder<'ctx> {
    pub fn with_context(name: &str, context: &'ctx Context) -> Result<Self, JitEvmEngineError> {
        Target::initialize_native(&InitializationConfig::default())?;

        let module = context.create_module(name);
        let builder = context.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;

        let types = JitTypes::new(context, &execution_engine);
        let host_functions = jit::HostFunctions::new(types.clone(), &module, &execution_engine);

        let tracers = Tracers::default();

        let ctx = BuilderContext {
            context,
            module,
            builder,
            types,
            tracers,
        };

        Ok(Self {
            ctx,
            execution_engine,
            debug_ir: None,
            debug_asm: None,
            debug_obj: None,
            tracing_options: Default::default(),
            host_functions,
        })
    }

    pub fn tracing_options(mut self, options: TracingConfig) -> Self {
        self.tracing_options = options;
        self
    }

    pub fn debug_ir(mut self, filename: &str) -> Self {
        self.debug_ir = Some(filename.into());
        self
    }

    pub fn debug_asm(mut self, filename: &str) -> Self {
        self.debug_asm = Some(filename.into());
        self
    }

    pub fn debug_obj(mut self, filename: &str) -> Self {
        self.debug_obj = Some(filename.into());
        self
    }

    pub fn build<SPEC: Spec>(
        self,
        spec: SPEC,
        code: &Bytes,
        mode: EvmOpParserMode,
    ) -> Result<JitEvmContract<'ctx, SPEC>, JitEvmEngineError> {
        let JitContractBuilder {
            mut ctx,
            execution_engine,
            debug_ir,
            debug_asm,
            debug_obj,
            tracing_options,
            host_functions,
        } = self;

        let code = EvmCode::new_from_bytes(code, mode)?
            .augment()
            .blocks::<SPEC>();

        // Initialize tracers, setup TLS variables, etc.
        ctx.tracers = tracing_options.into_tracer(&ctx, &code, &execution_engine)?;

        let block_cursor = cursor::BlockCursor::new(&ctx, code)?;
        let mut block_iter = block_cursor.iter();

        while let Some(current_block) = block_iter.next() {
            use EvmOp::*;

            ctx.builder.position_at_end(current_block.block().block);

            ops::insert_block_checks(&ctx, current_block)?;

            // Top-of-block stuff for tracers that might be enabled.
            let val = ctx
                .types
                .type_i64
                .const_int(current_block.index() as u64, false);
            ctx.tracers.trace_block(&ctx, val)?;

            let mut instruction_iter = current_block.instruction_iter();

            while let Some(current) = instruction_iter.next() {
                match current.op() {
                    Add => ops::build_arithmetic_op(&ctx, current)?,
                    Sub => ops::build_arithmetic_op(&ctx, current)?,
                    Mul => ops::build_arithmetic_op(&ctx, current)?,
                    Div => ops::build_arithmetic_op(&ctx, current)?,
                    Sdiv => ops::build_arithmetic_op(&ctx, current)?,
                    Mod => ops::build_arithmetic_op(&ctx, current)?,
                    Smod => ops::build_arithmetic_op(&ctx, current)?,
                    Shl => ops::build_arithmetic_op(&ctx, current)?,
                    Shr => ops::build_arithmetic_op(&ctx, current)?,
                    Sar => ops::build_arithmetic_op(&ctx, current)?,
                    And => ops::build_arithmetic_op(&ctx, current)?,
                    Or => ops::build_arithmetic_op(&ctx, current)?,
                    Xor => ops::build_arithmetic_op(&ctx, current)?,
                    Slt => ops::build_cmp_op(&ctx, current)?,
                    Sgt => ops::build_cmp_op(&ctx, current)?,
                    Not => ops::build_not_op(&ctx, current)?,
                    Byte => ops::build_byte_op(&ctx, current)?,
                    Addmod => ops::build_mod_op(&ctx, current)?,
                    Mulmod => ops::build_mod_op(&ctx, current)?,
                    Signextend => ops::build_signextend_op(&ctx, current)?,
                    Iszero => ops::iszero_op(&ctx, current)?,
                    Exp => ops::build_exp_op::<SPEC>(&ctx, current)?,
                    Eq => ops::build_cmp_op(&ctx, current)?,
                    Lt => ops::build_cmp_op(&ctx, current)?,
                    Gt => ops::build_cmp_op(&ctx, current)?,
                    Stop => ops::build_stop_op(&ctx, current)?,
                    Push(_, val) => ops::build_push_op(&ctx, current, val)?,
                    Pop => ops::build_pop_op(&ctx, current)?,
                    Swap1 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap2 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap3 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap4 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap5 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap6 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap7 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap8 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap9 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap10 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap11 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap12 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap13 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap14 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap15 => ops::build_stack_swap_op(&ctx, current)?,
                    Swap16 => ops::build_stack_swap_op(&ctx, current)?,
                    Dup1 => ops::build_dup_op(&ctx, current)?,
                    Dup2 => ops::build_dup_op(&ctx, current)?,
                    Dup3 => ops::build_dup_op(&ctx, current)?,
                    Dup4 => ops::build_dup_op(&ctx, current)?,
                    Dup5 => ops::build_dup_op(&ctx, current)?,
                    Dup6 => ops::build_dup_op(&ctx, current)?,
                    Dup7 => ops::build_dup_op(&ctx, current)?,
                    Dup8 => ops::build_dup_op(&ctx, current)?,
                    Dup9 => ops::build_dup_op(&ctx, current)?,
                    Dup10 => ops::build_dup_op(&ctx, current)?,
                    Dup11 => ops::build_dup_op(&ctx, current)?,
                    Dup12 => ops::build_dup_op(&ctx, current)?,
                    Dup13 => ops::build_dup_op(&ctx, current)?,
                    Dup14 => ops::build_dup_op(&ctx, current)?,
                    Dup15 => ops::build_dup_op(&ctx, current)?,
                    Dup16 => ops::build_dup_op(&ctx, current)?,
                    Mstore => ops::build_mstore_op::<SPEC>(&ctx, current)?,
                    Mstore8 => ops::build_mstore8_op::<SPEC>(&ctx, current)?,
                    Mload => ops::build_mload_op::<SPEC>(&ctx, current)?,
                    Sload => host_functions.build_sload(&ctx, current)?,
                    Sstore => host_functions.build_sstore(&ctx, current)?,
                    Sha3 => host_functions.build_sha3::<SPEC>(&ctx, current)?,
                    Invalid => ops::build_invalid_op(&ctx, current)?,
                    Revert => ops::build_revert_op::<SPEC>(&ctx, current)?,
                    Return => ops::build_return_op::<SPEC>(&ctx, current)?,
                    GasLimit => BlockContext::build_get_gas_limit(&ctx, current)?,
                    BaseFee => BlockContext::build_get_basefee(&ctx, current)?,
                    PrevRandao => BlockContext::build_get_randao(&ctx, current)?,
                    Timestamp => BlockContext::build_get_timestamp(&ctx, current)?,
                    Coinbase => BlockContext::build_get_coinbase(&ctx, current)?,
                    Number => BlockContext::build_get_number(&ctx, current)?,
                    Codesize => TransactionContext::build_get_codesize(&ctx, current)?,
                    Caller => TransactionContext::build_get_caller(&ctx, current)?,
                    Callvalue => TransactionContext::build_get_callvalue(&ctx, current)?,
                    Calldataload => TransactionContext::build_get_calldata(&ctx, current)?,
                    Calldatasize => TransactionContext::build_get_calldatalen(&ctx, current)?,
                    Origin => TransactionContext::build_get_origin(&ctx, current)?,
                    Jumpdest => ops::build_jumpdest_op(&ctx, current)?,
                    Jump => ops::build_jump_op(&ctx, current)?,
                    Jumpi => ops::build_jumpi_op(&ctx, current)?,
                    AugmentedPushJump(_, val) => ops::build_augmented_jump_op(&ctx, current, val)?,
                    AugmentedPushJumpi(_, val) => {
                        ops::build_augmented_jumpi_op(&ctx, current, val)?
                    }
                    _ => panic!(
                        "Unimplemented instruction {}: {:?}",
                        current.idx(),
                        current.op()
                    ),
                }

                if !current.op().is_block_terminal() && current.is_last_instruction() {
                    let next = current.next_block();
                    next.add_incoming(current.book_ref(), current.block());
                    ctx.builder.build_unconditional_branch(next.block)?;
                }
            }
        }

        if !ctx.tracers.is_empty() {
            let fetch_trace_fn_type = ctx.types.type_void.fn_type(
                &[ctx.types.type_ptrint.into(), ctx.types.type_ptrint.into()],
                false,
            );
            let function = ctx
                .module
                .add_function("fetch_trace_data", fetch_trace_fn_type, None);

            let block = ctx.context.append_basic_block(function, "fetch_trace");
            ctx.builder.position_at_end(block);

            let ptr = function
                .get_nth_param(0)
                .expect("Has 2 params")
                .into_int_value();
            let len = function
                .get_nth_param(1)
                .expect("Has 2 params")
                .into_int_value();

            ctx.tracers.save_trace_data(&ctx, ptr, len)?;
        };

        //// RENDER INSTRUCTIONS

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

        if let Some(path) = debug_obj {
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

            machine.write_to_file(&ctx.module, FileType::Object, path.as_ref())?;
        }

        let BuilderContext { tracers, .. } = ctx;

        let fetch_trace_data = if tracers.is_empty() {
            None
        } else {
            let f: JitFunction<JitEvmFetchTracing> =
                unsafe { execution_engine.get_function("fetch_trace_data")? };
            Some(f)
        };

        // COMPILE
        let function: JitFunction<JitEvmCompiledContract> =
            unsafe { execution_engine.get_function("executecontract")? };
        Ok(JitEvmContract {
            fetch_trace_data,
            execution_engine,
            tracers,
            function,
            spec,
        })
    }
}
