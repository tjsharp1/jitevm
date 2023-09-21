use crate::code::{EvmOp, IndexedEvmCode};
use crate::constants::*;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::values::{BasicValue, IntValue, PhiValue};
use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;

#[cfg(test)]
mod test;

#[macro_use]
mod arithmetic;
#[macro_use]
mod bytes;
mod context;
mod error;
mod ffi;
#[macro_use]
mod jump;
#[macro_use]
mod memory;
#[macro_use]
mod stack;
mod types;

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
    pub sp: IntValue<'ctx>,
    pub mem: IntValue<'ctx>,
}

impl<'ctx> JitEvmEngineBookkeeping<'ctx> {
    pub fn update_sp(&self, sp: IntValue<'ctx>) -> Self {
        Self { sp, ..*self }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct JitEvmEngineSimpleBlock<'ctx> {
    pub block: BasicBlock<'ctx>,
    pub phi_execution_context: PhiValue<'ctx>,
    pub phi_sp_min: PhiValue<'ctx>,
    pub phi_sp_max: PhiValue<'ctx>,
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
            phi_sp,
            phi_mem,
        })
    }

    pub fn book(&self) -> JitEvmEngineBookkeeping<'ctx> {
        JitEvmEngineBookkeeping {
            execution_context: self.phi_execution_context.as_basic_value().into_int_value(),
            sp_min: self.phi_sp_min.as_basic_value().into_int_value(),
            sp_max: self.phi_sp_max.as_basic_value().into_int_value(),
            sp: self.phi_sp.as_basic_value().into_int_value(),
            mem: self.phi_mem.as_basic_value().into_int_value(),
        }
    }

    pub fn phi_setup_block(&self, book: &JitEvmEngineBookkeeping<'ctx>, prev: &BasicBlock<'ctx>) {
        self.phi_execution_context
            .add_incoming(&[(&book.execution_context, *prev)]);
        self.phi_sp_min.add_incoming(&[(&book.sp_min, *prev)]);
        self.phi_sp_max.add_incoming(&[(&book.sp_max, *prev)]);
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

        // SETUP JIT'ED CONTRACT FUNCTION

        let executecontract_fn_type = ctx
            .types
            .type_retval
            .fn_type(&[ctx.types.type_ptrint.into()], false);
        let function = ctx
            .module
            .add_function("executecontract", executecontract_fn_type, None);

        // SETUP HANDLER

        let setup_block = ctx.context.append_basic_block(function, "setup");
        ctx.builder.position_at_end(setup_block);

        let setup_book = {
            let execution_context = function.get_nth_param(0).unwrap().into_int_value();
            let execution_context_ptr = ctx.builder.build_int_to_ptr(
                execution_context,
                ctx.types.type_ptrint.ptr_type(AddressSpace::default()),
                "",
            )?;
            let sp_int = ctx
                .builder
                .build_load(execution_context_ptr, "")?
                .into_int_value();
            let max_offset = (EVM_STACK_SIZE - 1) as u64 * EVM_STACK_ELEMENT_SIZE;
            let sp_max = ctx.builder.build_int_add(
                sp_int,
                ctx.types.type_ptrint.const_int(max_offset, false),
                "",
            )?;
            let mem_offset = ctx.builder.build_int_add(
                execution_context,
                ctx.types.type_ptrint.size_of(),
                "",
            )?;
            let mem_ptr = ctx.builder.build_int_to_ptr(
                mem_offset,
                ctx.types.type_ptrint.ptr_type(AddressSpace::default()),
                "",
            )?;
            let mem = ctx.builder.build_load(mem_ptr, "")?.into_int_value();
            JitEvmEngineBookkeeping {
                execution_context: execution_context,
                sp_min: sp_int,
                sp_max: sp_max,
                sp: sp_int,
                mem,
                // retval: retval
            }
        };

        // INSTRUCTIONS

        let ops_len = code.code.ops.len();
        assert!(ops_len > 0);

        let mut instructions: Vec<JitEvmEngineSimpleBlock<'_>> = Vec::new();
        for i in 0..ops_len {
            let block_before = if i == 0 {
                setup_block
            } else {
                instructions[i - 1].block
            };
            let label = format!("Instruction #{}: {:?}", i, code.code.ops[i]);
            instructions.push(JitEvmEngineSimpleBlock::new(
                &ctx,
                block_before,
                &label,
                &format!("_{}", i),
            )?);
        }

        ctx.builder.position_at_end(setup_block);
        ctx.builder
            .build_unconditional_branch(instructions[0].block)?;
        instructions[0].phi_setup_block(&setup_book, &setup_block);

        // END HANDLER

        let end =
            JitEvmEngineSimpleBlock::new(&ctx, instructions[ops_len - 1].block, &"end", &"-end")?;
        ctx.builder
            .build_return(Some(&ctx.types.type_retval.const_int(0, false)))?;

        // ERROR-JUMPDEST HANDLER

        let error_jumpdest =
            JitEvmEngineSimpleBlock::new(&ctx, end.block, &"error-jumpdest", &"-error-jumpdest")?;
        ctx.builder
            .build_return(Some(&ctx.types.type_retval.const_int(1, false)))?;

        // RENDER INSTRUCTIONS

        for (i, op) in code.code.ops.iter().enumerate() {
            use EvmOp::*;

            let this = instructions[i];

            ctx.builder.position_at_end(this.block);

            let book = this.book();

            let next = if i + 1 == ops_len {
                end
            } else {
                instructions[i + 1]
            };

            let book = match op {
                Stop => {
                    let val = ctx.types.type_retval.const_int(0, false);
                    ctx.builder.build_return(Some(&val))?;
                    continue; // skip auto-generated jump to next instruction
                }
                Push(_, val) => {
                    let val = ctx.types.type_stackel.const_int_arbitrary_precision(&val.0);
                    let book = build_stack_push!(ctx, book, val);
                    book
                }
                Pop => {
                    let (book, _) = build_stack_pop!(ctx, book);
                    book
                }
                Jumpdest => book,
                Mstore => build_mstore!(ctx, book),
                Mstore8 => build_mstore8!(ctx, book),
                Mload => build_mload!(ctx, book),
                Sload => host_functions.build_sload(&ctx, book)?,
                Sstore => host_functions.build_sstore(&ctx, book)?,
                Sha3 => host_functions.build_sha3(&ctx, book)?,
                Jump => build_jump!(
                    ctx,
                    book,
                    code,
                    this,
                    end,
                    instructions,
                    error_jumpdest,
                    i,
                    op
                ),
                Jumpi => build_jumpi!(
                    ctx,
                    book,
                    code,
                    this,
                    next,
                    end,
                    instructions,
                    error_jumpdest,
                    i,
                    op
                ),
                Swap1 => build_stack_swap!(ctx, book, 1 + 1),
                Swap2 => build_stack_swap!(ctx, book, 2 + 1),
                Swap3 => build_stack_swap!(ctx, book, 3 + 1),
                Swap4 => build_stack_swap!(ctx, book, 4 + 1),
                Swap5 => build_stack_swap!(ctx, book, 5 + 1),
                Swap6 => build_stack_swap!(ctx, book, 6 + 1),
                Swap7 => build_stack_swap!(ctx, book, 7 + 1),
                Swap8 => build_stack_swap!(ctx, book, 8 + 1),
                Swap9 => build_stack_swap!(ctx, book, 9 + 1),
                Swap10 => build_stack_swap!(ctx, book, 10 + 1),
                Swap11 => build_stack_swap!(ctx, book, 11 + 1),
                Swap12 => build_stack_swap!(ctx, book, 12 + 1),
                Swap13 => build_stack_swap!(ctx, book, 13 + 1),
                Swap14 => build_stack_swap!(ctx, book, 14 + 1),
                Swap15 => build_stack_swap!(ctx, book, 15 + 1),
                Swap16 => build_stack_swap!(ctx, book, 16 + 1),
                Dup1 => build_dup!(ctx, book, 1),
                Dup2 => build_dup!(ctx, book, 2),
                Dup3 => build_dup!(ctx, book, 3),
                Dup4 => build_dup!(ctx, book, 4),
                Dup5 => build_dup!(ctx, book, 5),
                Dup6 => build_dup!(ctx, book, 6),
                Dup7 => build_dup!(ctx, book, 7),
                Dup8 => build_dup!(ctx, book, 8),
                Dup9 => build_dup!(ctx, book, 9),
                Dup10 => build_dup!(ctx, book, 10),
                Dup11 => build_dup!(ctx, book, 11),
                Dup12 => build_dup!(ctx, book, 12),
                Dup13 => build_dup!(ctx, book, 13),
                Dup14 => build_dup!(ctx, book, 14),
                Dup15 => build_dup!(ctx, book, 15),
                Dup16 => build_dup!(ctx, book, 16),
                Iszero => llvmnativei256_iszero!(ctx, book, this, next, op, i),
                Add => llvmnativei256_op2!(ctx, book, build_int_add),
                Sub => llvmnativei256_op2!(ctx, book, build_int_sub),
                Mul => llvmnativei256_op2!(ctx, book, build_int_mul),
                Div => llvmnativei256_op2!(ctx, book, build_int_unsigned_div),
                Sdiv => llvmnativei256_op2!(ctx, book, build_int_signed_div),
                Mod => llvmnativei256_op2!(ctx, book, build_int_unsigned_rem),
                Smod => llvmnativei256_op2!(ctx, book, build_int_signed_rem),
                Shl => llvmnativei256_lshift!(ctx, book, build_left_shift),
                Shr => llvmnativei256_rshift!(ctx, book, build_right_shift, false),
                Sar => llvmnativei256_rshift!(ctx, book, build_right_shift, true),
                Exp => op2_i256_exp!(ctx, book, this, next, i),
                Eq => llvmnativei256_cmp!(
                    ctx,
                    book,
                    this,
                    next,
                    instructions,
                    i,
                    op,
                    IntPredicate::EQ
                ),
                Lt => llvmnativei256_cmp!(
                    ctx,
                    book,
                    this,
                    next,
                    instructions,
                    i,
                    op,
                    IntPredicate::ULT
                ),
                Gt => llvmnativei256_cmp!(
                    ctx,
                    book,
                    this,
                    next,
                    instructions,
                    i,
                    op,
                    IntPredicate::UGT
                ),
                Slt => llvmnativei256_cmp!(
                    ctx,
                    book,
                    this,
                    next,
                    instructions,
                    i,
                    op,
                    IntPredicate::SLT
                ),
                Sgt => llvmnativei256_cmp!(
                    ctx,
                    book,
                    this,
                    next,
                    instructions,
                    i,
                    op,
                    IntPredicate::SGT
                ),
                And => llvmnativei256_op2!(ctx, book, build_and),
                Or => llvmnativei256_op2!(ctx, book, build_or),
                Xor => llvmnativei256_op2!(ctx, book, build_xor),
                Not => llvmnativei256_op1!(ctx, book, build_not),
                Addmod => op2_i256_mod!(ctx, book, build_int_add),
                Mulmod => op2_i256_mod!(ctx, book, build_int_mul),
                Signextend => op_signextend!(ctx, book, this, next, i),
                AugmentedPushJump(_, val) => {
                    build_augmented_jump!(ctx, book, code, this, end, instructions, val)
                }
                AugmentedPushJumpi(_, val) => {
                    build_augmented_jumpi!(ctx, book, code, this, next, end, instructions, val)
                }

                _ => {
                    panic!("Op not implemented: {:?}", op);
                }
            };

            ctx.builder.build_unconditional_branch(next.block)?;
            next.add_incoming(&book, &this);
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
