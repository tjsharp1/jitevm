use crate::code::{EvmOp, IndexedEvmCode};
use crate::constants::*;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::values::{BasicValue, IntValue, PhiValue, VectorValue}; //PointerValue
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
mod stack;
mod types;

use ffi::*;

pub use context::JitEvmExecutionContext;
use context::JitEvmPtrs;
pub use error::JitEvmEngineError;

pub type JitEvmCompiledContract = unsafe extern "C" fn(usize) -> u64;

#[derive(Debug, Copy, Clone)]
pub struct JitEvmEngineBookkeeping<'ctx> {
    pub execution_context: IntValue<'ctx>,
    pub sp_min: IntValue<'ctx>,
    pub sp_max: IntValue<'ctx>,
    pub sp: IntValue<'ctx>,
    pub mem: IntValue<'ctx>,
    // pub retval: IntValue<'ctx>,
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
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        block_before: BasicBlock<'ctx>,
        name: &str,
        suffix: &str,
    ) -> Result<Self, JitEvmEngineError> {
        // TODO: move this to types
        let i64_type = context.i64_type();

        let block = context.insert_basic_block_after(block_before, name);
        builder.position_at_end(block);
        let phi_execution_context =
            builder.build_phi(i64_type, &format!("execution_context{}", suffix))?;
        let phi_sp_min = builder.build_phi(i64_type, &format!("sp_min{}", suffix))?;
        let phi_sp_max = builder.build_phi(i64_type, &format!("sp_max{}", suffix))?;
        let phi_sp = builder.build_phi(i64_type, &format!("sp{}", suffix))?;
        let phi_mem = builder.build_phi(i64_type, &format!("mem{}", suffix))?;

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

pub struct JitContractBuilder<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    pub types: types::JitTypes<'ctx>,
    pub debug_ir: Option<String>,
    pub debug_asm: Option<String>,
    pub host_functions: HostFunctions<'ctx>,
}

impl<'ctx> JitContractBuilder<'ctx> {
    pub fn with_context(name: &str, context: &'ctx Context) -> Result<Self, JitEvmEngineError> {
        Target::initialize_native(&InitializationConfig::default())?;

        let module = context.create_module(name);
        let builder = context.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;

        let types = types::JitTypes::new(&context, &execution_engine);
        let host_functions = HostFunctions::new(types.clone(), &module, &execution_engine);

        Ok(Self {
            context: &context,
            module,
            builder,
            execution_engine,
            types,
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
            context,
            module,
            builder,
            execution_engine,
            types,
            debug_ir,
            debug_asm,
            host_functions,
        } = self;

        // SETUP JIT'ED CONTRACT FUNCTION

        let executecontract_fn_type = types
            .type_retval
            .fn_type(&[types.type_ptrint.into()], false);
        let function = module.add_function("executecontract", executecontract_fn_type, None);

        // SETUP HANDLER

        let setup_block = context.append_basic_block(function, "setup");
        builder.position_at_end(setup_block);

        let setup_book = {
            let execution_context = function.get_nth_param(0).unwrap().into_int_value();
            let execution_context_ptr = builder.build_int_to_ptr(
                execution_context,
                types.type_ptrint.ptr_type(AddressSpace::default()),
                "",
            )?;
            let sp_int = builder
                .build_load(execution_context_ptr, "")?
                .into_int_value();
            let max_offset = (EVM_STACK_SIZE - 1) as u64 * EVM_STACK_ELEMENT_SIZE;
            let sp_max = builder.build_int_add(
                sp_int,
                types.type_ptrint.const_int(max_offset, false),
                "",
            )?;
            let mem_offset =
                builder.build_int_add(execution_context, types.type_ptrint.size_of(), "")?;
            let mem_ptr = builder.build_int_to_ptr(
                mem_offset,
                types.type_ptrint.ptr_type(AddressSpace::default()),
                "",
            )?;
            let mem = builder.build_load(mem_ptr, "")?.into_int_value();
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
                &context,
                &builder,
                block_before,
                &label,
                &format!("_{}", i),
            )?);
        }

        builder.position_at_end(setup_block);
        builder.build_unconditional_branch(instructions[0].block)?;
        instructions[0].phi_setup_block(&setup_book, &setup_block);

        // END HANDLER

        let end = JitEvmEngineSimpleBlock::new(
            &context,
            &builder,
            instructions[ops_len - 1].block,
            &"end",
            &"-end",
        )?;
        builder.build_return(Some(&types.type_retval.const_int(0, false)))?;

        // ERROR-JUMPDEST HANDLER

        let error_jumpdest = JitEvmEngineSimpleBlock::new(
            &context,
            &builder,
            end.block,
            &"error-jumpdest",
            &"-error-jumpdest",
        )?;
        builder.build_return(Some(&types.type_retval.const_int(1, false)))?;

        // RENDER INSTRUCTIONS

        for (i, op) in code.code.ops.iter().enumerate() {
            use EvmOp::*;

            let this = instructions[i];

            builder.position_at_end(this.block);

            let book = this.book();

            let next = if i + 1 == ops_len {
                end
            } else {
                instructions[i + 1]
            };

            let book = match op {
                Stop => {
                    let val = types.type_retval.const_int(0, false);
                    builder.build_return(Some(&val))?;
                    continue; // skip auto-generated jump to next instruction
                }
                Push(_, val) => {
                    let val = types.type_stackel.const_int_arbitrary_precision(&val.0);
                    let book = build_stack_push!(types, builder, book, val);
                    book
                }
                Pop => {
                    let (book, _) = build_stack_pop!(types, builder, book);
                    book
                }
                Jumpdest => book,
                Mstore => {
                    let (book, offset) = build_stack_pop!(types, builder, book);
                    let (book, vec0, vec1) = build_stack_pop_vector!(types, builder, book);

                    let shuffled =
                        builder.build_shuffle_vector(vec0, vec1, types.swap_bytes, "")?;

                    let casted = builder.build_int_cast(offset, types.type_ptrint, "")?;
                    let mem = builder.build_int_add(book.mem, casted, "")?;
                    let dest_ptr = builder.build_int_to_ptr(
                        mem,
                        types.type_stackel.ptr_type(AddressSpace::default()),
                        "",
                    )?;

                    builder.build_store(dest_ptr, shuffled)?.set_alignment(1)?;

                    book
                }
                Mstore8 => {
                    let (book, offset) = build_stack_pop!(types, builder, book);
                    let (book, value) = build_stack_pop!(types, builder, book);
                    let value_casted = builder.build_int_cast(value, types.type_i8, "")?;
                    let offset_casted = builder.build_int_cast(offset, types.type_ptrint, "")?;

                    let mem = builder.build_int_add(book.mem, offset_casted, "")?;
                    let dest_ptr = builder.build_int_to_ptr(
                        mem,
                        types.type_i8.ptr_type(AddressSpace::default()),
                        "",
                    )?;

                    builder.build_store(dest_ptr, value_casted)?;

                    book
                }
                Mload => {
                    // TODO: memory bounds checks
                    let (book, offset) = build_stack_pop!(types, builder, book);
                    let casted = builder.build_int_cast(offset, types.type_ptrint, "")?;

                    let mem0 = builder.build_int_add(book.mem, casted, "")?;
                    let mem1_offset = types
                        .type_ptrint
                        .const_int(EVM_JIT_STACK_ALIGN as u64, false);
                    let mem1 = builder.build_int_add(mem0, mem1_offset, "")?;

                    let mem0_ptr = builder.build_int_to_ptr(
                        mem0,
                        types.type_ivec.ptr_type(AddressSpace::default()),
                        "",
                    )?;
                    let mem1_ptr = builder.build_int_to_ptr(
                        mem1,
                        types.type_ivec.ptr_type(AddressSpace::default()),
                        "",
                    )?;

                    let vec0 = builder.build_load(mem0_ptr, "")?.into_vector_value();
                    let vec1 = builder.build_load(mem1_ptr, "")?.into_vector_value();

                    vec0.as_instruction_value()
                        .ok_or(JitEvmEngineError::NoInstructionValue)?
                        .set_alignment(1)?;
                    vec1.as_instruction_value()
                        .ok_or(JitEvmEngineError::NoInstructionValue)?
                        .set_alignment(1)?;

                    let shuffled =
                        builder.build_shuffle_vector(vec0, vec1, types.swap_bytes, "")?;

                    let book = build_stack_push_vector!(types, builder, book, shuffled);
                    book
                }
                Sload => host_functions.build_sload(&builder, book)?,
                Sstore => host_functions.build_sstore(&builder, book)?,
                Sha3 => host_functions.build_sha3(&builder, book)?,
                //Jump => {
                //    let (book, target) = self.build_stack_pop(book)?;

                //    if code.jumpdests.is_empty() {
                //        // there are no valid jump targets, this Jump has to fail!
                //        // TODO: should this be the error block?
                //        self.builder.build_unconditional_branch(end.block)?;
                //        end.add_incoming(&book, &this);
                //    } else {
                //        let mut jump_table: Vec<JitEvmEngineSimpleBlock<'_>> = Vec::new();
                //        for (j, jmp_i) in code.jumpdests.iter().enumerate() {
                //            let jmp_target = code.opidx2target[jmp_i];
                //            jump_table.push(JitEvmEngineSimpleBlock::new(
                //                &self,
                //                if j == 0 {
                //                    this.block
                //                } else {
                //                    jump_table[j - 1].block
                //                },
                //                &format!(
                //                    "instruction #{}: {:?} / to Jumpdest #{} at op #{} to byte #{}",
                //                    i, op, j, jmp_i, jmp_target
                //                ),
                //                &format!("_{}_{}", i, j),
                //            )?);
                //        }

                //        self.builder.position_at_end(this.block);
                //        self.builder
                //            .build_unconditional_branch(jump_table[0].block)?;
                //        jump_table[0].add_incoming(&book, &this);

                //        for (j, jmp_i) in code.jumpdests.iter().enumerate() {
                //            let jmp_target = code.opidx2target[jmp_i];
                //            let jmp_target = jmp_target.as_u64(); // REMARK: assumes that code cannot exceed 2^64 instructions, probably ok ;)
                //            self.builder.position_at_end(jump_table[j].block);
                //            let cmp = self.builder.build_int_compare(
                //                IntPredicate::EQ,
                //                self.types.type_stackel.const_int(jmp_target, false),
                //                target,
                //                "",
                //            )?;
                //            if j + 1 == code.jumpdests.len() {
                //                self.builder.build_conditional_branch(
                //                    cmp,
                //                    instructions[*jmp_i].block,
                //                    error_jumpdest.block,
                //                )?;
                //                instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
                //                error_jumpdest.add_incoming(&book, &jump_table[j]);
                //            } else {
                //                self.builder.build_conditional_branch(
                //                    cmp,
                //                    instructions[*jmp_i].block,
                //                    jump_table[j + 1].block,
                //                )?;
                //                instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
                //                jump_table[j + 1].add_incoming(&book, &jump_table[j]);
                //            }
                //        }
                //    }

                //    continue; // skip auto-generated jump to next instruction
                //}
                //Jumpi => {
                //    let (book, target) = self.build_stack_pop(book)?;
                //    let (book, val) = self.build_stack_pop(book)?;

                //    if code.jumpdests.is_empty() {
                //        // there are no valid jump targets, this Jumpi has to fail!
                //        self.builder.build_unconditional_branch(end.block)?;
                //        end.add_incoming(&book, &this);
                //    } else {
                //        let mut jump_table: Vec<JitEvmEngineSimpleBlock<'_>> = Vec::new();
                //        for (j, jmp_i) in code.jumpdests.iter().enumerate() {
                //            let jmp_target = code.opidx2target[jmp_i];
                //            jump_table.push(JitEvmEngineSimpleBlock::new(
                //                &self,
                //                if j == 0 {
                //                    this.block
                //                } else {
                //                    jump_table[j - 1].block
                //                },
                //                &format!(
                //                    "instruction #{}: {:?} / to Jumpdest #{} at op #{} to byte #{}",
                //                    i, op, j, jmp_i, jmp_target
                //                ),
                //                &format!("_{}_{}", i, j),
                //            )?);
                //        }

                //        self.builder.position_at_end(this.block);
                //        let cmp = self.builder.build_int_compare(
                //            IntPredicate::EQ,
                //            self.types.type_stackel.const_int(0, false),
                //            val,
                //            "",
                //        )?;
                //        self.builder.build_conditional_branch(
                //            cmp,
                //            next.block,
                //            jump_table[0].block,
                //        )?;
                //        next.add_incoming(&book, &this);
                //        jump_table[0].add_incoming(&book, &this);

                //        for (j, jmp_i) in code.jumpdests.iter().enumerate() {
                //            let jmp_target = code.opidx2target[jmp_i];
                //            let jmp_target = jmp_target.as_u64(); // REMARK: assumes that code cannot exceed 2^64 instructions, probably ok ;)
                //            self.builder.position_at_end(jump_table[j].block);
                //            let cmp = self.builder.build_int_compare(
                //                IntPredicate::EQ,
                //                self.types.type_stackel.const_int(jmp_target, false),
                //                target,
                //                "",
                //            )?;
                //            if j + 1 == code.jumpdests.len() {
                //                self.builder.build_conditional_branch(
                //                    cmp,
                //                    instructions[*jmp_i].block,
                //                    error_jumpdest.block,
                //                )?;
                //                instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
                //                error_jumpdest.add_incoming(&book, &jump_table[j]);
                //            } else {
                //                self.builder.build_conditional_branch(
                //                    cmp,
                //                    instructions[*jmp_i].block,
                //                    jump_table[j + 1].block,
                //                )?;
                //                instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
                //                jump_table[j + 1].add_incoming(&book, &jump_table[j]);
                //            }
                //        }
                //    }

                //    continue; // skip auto-generated jump to next instruction
                //}
                Swap1 => build_stack_swap!(types, builder, book, 1 + 1),
                Swap2 => build_stack_swap!(types, builder, book, 2 + 1),
                Swap3 => build_stack_swap!(types, builder, book, 3 + 1),
                Swap4 => build_stack_swap!(types, builder, book, 4 + 1),
                Swap5 => build_stack_swap!(types, builder, book, 5 + 1),
                Swap6 => build_stack_swap!(types, builder, book, 6 + 1),
                Swap7 => build_stack_swap!(types, builder, book, 7 + 1),
                Swap8 => build_stack_swap!(types, builder, book, 8 + 1),
                Swap9 => build_stack_swap!(types, builder, book, 9 + 1),
                Swap10 => build_stack_swap!(types, builder, book, 10 + 1),
                Swap11 => build_stack_swap!(types, builder, book, 11 + 1),
                Swap12 => build_stack_swap!(types, builder, book, 12 + 1),
                Swap13 => build_stack_swap!(types, builder, book, 13 + 1),
                Swap14 => build_stack_swap!(types, builder, book, 14 + 1),
                Swap15 => build_stack_swap!(types, builder, book, 15 + 1),
                Swap16 => build_stack_swap!(types, builder, book, 16 + 1),
                Dup1 => build_dup!(types, builder, book, 1),
                Dup2 => build_dup!(types, builder, book, 2),
                Dup3 => build_dup!(types, builder, book, 3),
                Dup4 => build_dup!(types, builder, book, 4),
                Dup5 => build_dup!(types, builder, book, 5),
                Dup6 => build_dup!(types, builder, book, 6),
                Dup7 => build_dup!(types, builder, book, 7),
                Dup8 => build_dup!(types, builder, book, 8),
                Dup9 => build_dup!(types, builder, book, 9),
                Dup10 => build_dup!(types, builder, book, 10),
                Dup11 => build_dup!(types, builder, book, 11),
                Dup12 => build_dup!(types, builder, book, 12),
                Dup13 => build_dup!(types, builder, book, 13),
                Dup14 => build_dup!(types, builder, book, 14),
                Dup15 => build_dup!(types, builder, book, 15),
                Dup16 => build_dup!(types, builder, book, 16),
                Iszero => {
                    op1_llvmnativei256_iszero!(context, types, builder, book, this, next, op, i)
                }
                Add => llvmnativei256_op2!(types, builder, book, build_int_add),
                Sub => llvmnativei256_op2!(types, builder, book, build_int_sub),
                Mul => llvmnativei256_op2!(types, builder, book, build_int_mul),
                Div => llvmnativei256_op2!(types, builder, book, build_int_unsigned_div),
                Sdiv => llvmnativei256_op2!(types, builder, book, build_int_signed_div),
                Mod => llvmnativei256_op2!(types, builder, book, build_int_unsigned_rem),
                Smod => llvmnativei256_op2!(types, builder, book, build_int_signed_rem),
                Shl => llvmnativei256_lshift!(types, builder, book, build_left_shift),
                Shr => llvmnativei256_rshift!(types, builder, book, build_right_shift, false),
                Sar => llvmnativei256_rshift!(types, builder, book, build_right_shift, true),
                Exp => op2_i256_exp!(context, types, builder, book, this, next, i),
                Eq => llvmnativei256_cmp!(
                    context,
                    types,
                    builder,
                    book,
                    this,
                    next,
                    instructions,
                    i,
                    op,
                    IntPredicate::EQ
                ),
                Lt => llvmnativei256_cmp!(
                    context,
                    types,
                    builder,
                    book,
                    this,
                    next,
                    instructions,
                    i,
                    op,
                    IntPredicate::ULT
                ),
                Gt => llvmnativei256_cmp!(
                    context,
                    types,
                    builder,
                    book,
                    this,
                    next,
                    instructions,
                    i,
                    op,
                    IntPredicate::UGT
                ),
                Slt => llvmnativei256_cmp!(
                    context,
                    types,
                    builder,
                    book,
                    this,
                    next,
                    instructions,
                    i,
                    op,
                    IntPredicate::SLT
                ),
                Sgt => llvmnativei256_cmp!(
                    context,
                    types,
                    builder,
                    book,
                    this,
                    next,
                    instructions,
                    i,
                    op,
                    IntPredicate::SGT
                ),
                And => llvmnativei256_op2!(types, builder, book, build_and),
                Or => llvmnativei256_op2!(types, builder, book, build_or),
                Xor => llvmnativei256_op2!(types, builder, book, build_xor),
                Not => llvmnativei256_op1!(types, builder, book, build_not),
                Addmod => {
                    let (book, a) = build_stack_pop!(types, builder, book);
                    let (book, b) = build_stack_pop!(types, builder, book);
                    let (book, n) = build_stack_pop!(types, builder, book);

                    let width = types.type_stackel.get_bit_width() * 2;
                    let type_iup = context.custom_width_int_type(width);

                    let a_up = builder.build_int_cast_sign_flag(a, type_iup, false, "")?;
                    let b_up = builder.build_int_cast_sign_flag(b, type_iup, false, "")?;
                    let n_up = builder.build_int_cast_sign_flag(n, type_iup, false, "")?;

                    let result = builder.build_int_add(a_up, b_up, "")?;
                    let result = builder.build_int_unsigned_rem(result, n_up, "")?;

                    let result = builder.build_int_cast(result, types.type_stackel, "")?;

                    build_stack_push!(types, builder, book, result)
                }
                Mulmod => {
                    let (book, a) = build_stack_pop!(types, builder, book);
                    let (book, b) = build_stack_pop!(types, builder, book);
                    let (book, n) = build_stack_pop!(types, builder, book);

                    let width = types.type_stackel.get_bit_width() * 2;
                    let type_iup = context.custom_width_int_type(width);

                    let a_up = builder.build_int_cast_sign_flag(a, type_iup, false, "")?;
                    let b_up = builder.build_int_cast_sign_flag(b, type_iup, false, "")?;
                    let n_up = builder.build_int_cast_sign_flag(n, type_iup, false, "")?;

                    let result = builder.build_int_mul(a_up, b_up, "")?;
                    let result = builder.build_int_unsigned_rem(result, n_up, "")?;

                    let result = builder.build_int_cast(result, types.type_stackel, "")?;

                    build_stack_push!(types, builder, book, result)
                }
                Signextend => {
                    let (book, x) = build_stack_pop!(types, builder, book);

                    let const_32 = types.type_stackel.const_int(32, false);
                    let cmp = builder.build_int_compare(IntPredicate::UGE, x, const_32, "")?;

                    let label = format!("Instruction #{}: Signextend / else", i);
                    let index = format!("_{}", i);
                    let else_block = JitEvmEngineSimpleBlock::new(
                        context, &builder, this.block, &label, &index,
                    )?;
                    builder.position_at_end(this.block);
                    builder.build_conditional_branch(cmp, next.block, else_block.block)?;

                    next.add_incoming(&book, &this);
                    else_block.add_incoming(&book, &this);

                    builder.position_at_end(else_block.block);

                    let (book, y) = build_stack_pop!(types, builder, book);

                    let const_1 = types.type_stackel.const_int(1, false);
                    let const_7 = types.type_stackel.const_int(7, false);
                    let const_8 = types.type_stackel.const_int(8, false);

                    let x_8 = builder.build_int_mul(x, const_8, "")?;
                    let bit_index = builder.build_int_add(x_8, const_7, "")?;
                    let bit = builder.build_left_shift(const_1, bit_index, "")?;
                    let mask = builder.build_int_sub(bit, const_1, "")?;

                    let sign = builder.build_and(y, bit, "")?;
                    let is_signed = builder.build_int_compare(IntPredicate::EQ, sign, bit, "")?;

                    let not_mask = builder.build_not(mask, "")?;
                    let extended = builder.build_or(y, not_mask, "")?;
                    let unextended = builder.build_and(y, mask, "")?;

                    let result = builder
                        .build_select(is_signed, extended, unextended, "")?
                        .into_int_value();

                    build_stack_push!(types, builder, book, result);
                    builder.build_unconditional_branch(next.block)?;

                    continue; // skip auto-generated jump to next instruction
                }
                AugmentedPushJump(_, val) => {
                    if code.jumpdests.is_empty() {
                        // there are no valid jump targets, this Jump has to fail!
                        builder.build_unconditional_branch(end.block)?;
                        end.add_incoming(&book, &this);
                    } else {
                        // retrieve the corresponding jump target (panic if not a valid jump target) ...
                        let jmp_i = code.target2opidx[val];
                        // ... and jump to there!
                        builder.build_unconditional_branch(instructions[jmp_i].block)?;
                        instructions[jmp_i].add_incoming(&book, &this);
                    }

                    continue; // skip auto-generated jump to next instruction
                }
                AugmentedPushJumpi(_, val) => {
                    let (book, condition) = build_stack_pop!(types, builder, book);

                    if code.jumpdests.is_empty() {
                        // there are no valid jump targets, this Jumpi has to fail!
                        builder.build_unconditional_branch(end.block)?;
                        end.add_incoming(&book, &this);
                    } else {
                        // retrieve the corresponding jump target (panic if not a valid jump target) ...
                        let jmp_i = code.target2opidx[val];
                        // ... and jump to there (conditionally)!
                        let cmp = builder.build_int_compare(
                            IntPredicate::EQ,
                            types.type_stackel.const_int(0, false),
                            condition,
                            "",
                        )?;
                        builder.build_conditional_branch(
                            cmp,
                            next.block,
                            instructions[jmp_i].block,
                        )?;
                        next.add_incoming(&book, &this);
                        instructions[jmp_i].add_incoming(&book, &this);
                    }

                    continue; // skip auto-generated jump to next instruction
                }

                _ => {
                    panic!("Op not implemented: {:?}", op);
                }
            };

            builder.build_unconditional_branch(next.block)?;
            next.add_incoming(&book, &this);
        }

        // OUTPUT LLVM
        if let Some(path) = debug_ir {
            module.print_to_file(path)?;
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

            machine.write_to_file(&module, FileType::Assembly, path.as_ref())?;
        }

        // COMPILE
        let function: JitFunction<JitEvmCompiledContract> =
            unsafe { execution_engine.get_function("executecontract")? };
        Ok(JitEvmContract { function })
    }
}
