use crate::code::{EvmOp, IndexedEvmCode};
use crate::constants::*;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::targets::{ByteOrdering, InitializationConfig, Target};
use inkwell::types::{IntType, VectorType}; //PointerType};
use inkwell::values::{BasicValue, IntValue, PhiValue, VectorValue}; //PointerValue
use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
use primitive_types::U256;
use std::convert::From;
use thiserror::Error;

#[cfg(test)]
mod test;

mod context;

pub use context::JitEvmExecutionContext;
use context::JitEvmPtrs;

pub type JitEvmCompiledContract = unsafe extern "C" fn(usize) -> u64;

// TODO: 256 bit double-add for now, will need to calc byte size of exp for gas accounting
macro_rules! op2_double_add_exp {
    ($self:ident, $book:ident, $accum:ident, $base:ident, $exp:ident, $mask:ident, 1) => {{
        let zero = $self.type_stackel.const_int(0, false);
        let one = $self.type_stackel.const_int(1, false);

        let and = $self.builder.build_and($exp, $mask, "");
        let is_zero_bit = $self
            .builder
            .build_int_compare(IntPredicate::EQ, and, zero, "");
        $mask = $self.builder.build_right_shift($mask, one, false, "");

        $accum = $self.builder.build_left_shift($accum, one, "");
        let add = $self
            .builder
            .build_select(is_zero_bit, zero, $base, "")
            .into_int_value();
        $accum = $self.builder.build_int_add($accum, add, "");
    }};
    ($self:ident, $book:ident, $accum:ident, $base:ident, $exp:ident, $mask:ident, 4) => {{
        op2_double_add_exp!($self, $book, $accum, $base, $exp, $mask, 1);
        op2_double_add_exp!($self, $book, $accum, $base, $exp, $mask, 1);
        op2_double_add_exp!($self, $book, $accum, $base, $exp, $mask, 1);
        op2_double_add_exp!($self, $book, $accum, $base, $exp, $mask, 1);
    }};
    ($self:ident, $book:ident, $accum:ident, $base:ident, $exp:ident, $mask:ident, 16) => {{
        op2_double_add_exp!($self, $book, $accum, $base, $exp, $mask, 4);
        op2_double_add_exp!($self, $book, $accum, $base, $exp, $mask, 4);
        op2_double_add_exp!($self, $book, $accum, $base, $exp, $mask, 4);
        op2_double_add_exp!($self, $book, $accum, $base, $exp, $mask, 4);
    }};
    ($self:ident, $book:ident, $accum:ident, $base:ident, $exp:ident, $mask:ident, 64) => {{
        op2_double_add_exp!($self, $book, $accum, $base, $exp, $mask, 16);
        op2_double_add_exp!($self, $book, $accum, $base, $exp, $mask, 16);
        op2_double_add_exp!($self, $book, $accum, $base, $exp, $mask, 16);
        op2_double_add_exp!($self, $book, $accum, $base, $exp, $mask, 16);
    }};
    ($self:ident, $book:ident, $this:ident, $next:ident, $i:ident) => {{
        let (book, a) = $self.build_stack_pop($book);
        let (book, exp) = $self.build_stack_pop(book);

        let const_255 = $self.type_stackel.const_int(255, false);
        let const_1 = $self.type_stackel.const_int(1, false);
        let zero = $self.type_stackel.const_int(0, false);
        let is_zero = $self
            .builder
            .build_int_compare(IntPredicate::EQ, exp, zero, "");

        let else_label = format!("Instruction #{}: Exp / else", $i);
        let then_label = format!("Instruction #{}: Exp / then", $i);
        let index = format!("_{}", $i);
        let else_block = JitEvmEngineSimpleBlock::new(&$self, $this.block, &else_label, &index);
        let then_block = JitEvmEngineSimpleBlock::new(&$self, $this.block, &then_label, &index);

        $self.builder.position_at_end($this.block);
        $self
            .builder
            .build_conditional_branch(is_zero, then_block.block, else_block.block);

        else_block.add_incoming(&book, &$this);
        then_block.add_incoming(&book, &$this);

        //// THEN BLOCK
        $self.builder.position_at_end(then_block.block);
        // TODO: constructor for this.....
        let then_book = JitEvmEngineBookkeeping {
            execution_context: then_block
                .phi_execution_context
                .as_basic_value()
                .into_int_value(),
            sp_min: then_block.phi_sp_min.as_basic_value().into_int_value(),
            sp_max: then_block.phi_sp_max.as_basic_value().into_int_value(),
            sp: then_block.phi_sp.as_basic_value().into_int_value(),
            mem: then_block.phi_mem.as_basic_value().into_int_value(),
        };
        $self.build_stack_push(then_book, const_1);
        $next.add_incoming(&then_book, &then_block);

        $self.builder.build_unconditional_branch($next.block);

        //// ELSE BLOCK
        $self.builder.position_at_end(else_block.block);
        let else_book = JitEvmEngineBookkeeping {
            execution_context: else_block
                .phi_execution_context
                .as_basic_value()
                .into_int_value(),
            sp_min: else_block.phi_sp_min.as_basic_value().into_int_value(),
            sp_max: else_block.phi_sp_max.as_basic_value().into_int_value(),
            sp: else_block.phi_sp.as_basic_value().into_int_value(),
            mem: else_block.phi_mem.as_basic_value().into_int_value(),
        };
        let mut mask = $self.builder.build_left_shift(const_1, const_255, "");

        let mut accum = a;
        op2_double_add_exp!($self, else_book, accum, a, exp, mask, 64);
        op2_double_add_exp!($self, else_book, accum, a, exp, mask, 64);
        op2_double_add_exp!($self, else_book, accum, a, exp, mask, 64);
        op2_double_add_exp!($self, else_book, accum, a, exp, mask, 64);

        $self.build_stack_push(else_book, accum);
        $self.builder.build_unconditional_branch($next.block);
        $next.add_incoming(&else_book, &else_block);
    }};
}

macro_rules! op1_llvmnativei256_operation {
    ($self:ident, $book:ident, $fname:ident) => {{
        let (book, a) = $self.build_stack_pop($book);
        let d = $self.builder.$fname(a, "");
        let book = $self.build_stack_push(book, d);
        book
    }};
}

macro_rules! op2_llvmnativei256_operation {
    ($self:ident, $book:ident, $fname:ident) => {{
        let (book, a) = $self.build_stack_pop($book);
        let (book, b) = $self.build_stack_pop(book);
        let d = $self.builder.$fname(a, b, "");
        let book = $self.build_stack_push(book, d);
        book
    }};
}

macro_rules! op2_llvmnativei256_compare_operation {
    ($self:ident, $book:ident, $this:expr, $next:expr, $instructions:expr, $i:expr, $op:expr, $predicate:expr) => {{
        let (book, a) = $self.build_stack_pop($book);
        let (book, b) = $self.build_stack_pop(book);
        let cmp = $self.builder.build_int_compare($predicate, a, b, "");

        let push_0 = JitEvmEngineSimpleBlock::new(
            &$self,
            $this.block,
            &format!("Instruction #{}: {:?} / push 0", $i, $op),
            &format!("_{}_0", $i),
        );
        let push_1 = JitEvmEngineSimpleBlock::new(
            &$self,
            push_0.block,
            &format!("Instruction #{}: {:?} / push 1", $i, $op),
            &format!("_{}_1", $i),
        );

        $self.builder.position_at_end($this.block);
        $self
            .builder
            .build_conditional_branch(cmp, push_1.block, push_0.block);
        push_0.add_incoming(&book, &$this);
        push_1.add_incoming(&book, &$this);

        $self.builder.position_at_end(push_0.block);
        let book_0 = $self.build_stack_push(book, $self.type_stackel.const_int(0, false));
        $self.builder.build_unconditional_branch($next.block);
        $next.add_incoming(&book_0, &push_0);

        $self.builder.position_at_end(push_1.block);
        let book_1 = $self.build_stack_push(book, $self.type_stackel.const_int(1, false));
        $self.builder.build_unconditional_branch($next.block);
        $next.add_incoming(&book_1, &push_1);

        continue; // skip auto-generated jump to next instruction
    }};
}

#[derive(Error, Debug)]
pub enum JitEvmEngineError {
    #[error("FunctionLookupError: {0:?}")]
    FunctionLookupError(#[from] inkwell::execution_engine::FunctionLookupError),
    #[error("LlvmStringError: {0:?}")]
    UnknownLlvmStringError(#[from] inkwell::support::LLVMString),
    #[error("StringError: {0:?}")]
    UnknownStringError(String),
}

impl From<String> for JitEvmEngineError {
    fn from(e: String) -> Self {
        Self::UnknownStringError(e)
    }
}

impl From<&str> for JitEvmEngineError {
    fn from(e: &str) -> Self {
        Self::UnknownStringError(e.to_string())
    }
}

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
        engine: &JitContractBuilder<'ctx>,
        block_before: BasicBlock<'ctx>,
        name: &str,
        suffix: &str,
    ) -> Self {
        let i64_type = engine.context.i64_type();

        let block = engine.context.insert_basic_block_after(block_before, name);
        engine.builder.position_at_end(block);
        let phi_execution_context = engine
            .builder
            .build_phi(i64_type, &format!("execution_context{}", suffix));
        let phi_sp_min = engine
            .builder
            .build_phi(i64_type, &format!("sp_min{}", suffix));
        let phi_sp_max = engine
            .builder
            .build_phi(i64_type, &format!("sp_max{}", suffix));
        let phi_sp = engine.builder.build_phi(i64_type, &format!("sp{}", suffix));
        let phi_mem = engine
            .builder
            .build_phi(i64_type, &format!("mem{}", suffix));

        Self {
            block,
            phi_execution_context,
            phi_sp_min,
            phi_sp_max,
            phi_sp,
            phi_mem,
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
    pub type_ptrint: IntType<'ctx>,
    pub type_stackel: IntType<'ctx>,
    pub type_retval: IntType<'ctx>,
    pub type_i8: IntType<'ctx>,
    pub type_ivec: VectorType<'ctx>,
    pub type_rvec: VectorType<'ctx>,
    pub swap_bytes: VectorValue<'ctx>,
    pub debug_ir: Option<String>,
    pub debug_asm: Option<String>,
    pub is_little_endian: bool,
}

impl<'ctx> JitContractBuilder<'ctx> {
    pub fn with_context(name: &str, context: &'ctx Context) -> Result<Self, JitEvmEngineError> {
        Target::initialize_native(&InitializationConfig::default())?;

        let module = context.create_module(name);
        let builder = context.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;

        let target_data = execution_engine.get_target_data();
        let type_ptrint = context.ptr_sized_int_type(&target_data, None); // type for pointers (stack pointer, host interaction)
                                                                          // ensure consistency btw Rust/LLVM definition of compiled contract function
        assert_eq!(type_ptrint.get_bit_width(), 64);
        assert_eq!(usize::BITS, 64);
        // TODO: the above assumes that pointers address memory byte-wise!

        let type_stackel = context.custom_width_int_type(256); // type for stack elements
        assert_eq!(type_stackel.get_bit_width(), 256);
        assert_eq!(
            type_stackel.get_bit_width() as u64,
            EVM_STACK_ELEMENT_SIZE * 8
        );

        let type_retval = context.i64_type(); // type for return value
                                              // ensure consistency btw Rust/LLVM definition of compiled contract function
        assert_eq!(type_retval.get_bit_width(), 64);
        assert_eq!(u64::BITS, 64);

        // vectorized byte swapping
        let type_i8 = context.i8_type();
        let type_ivec = type_i8.vec_type(16);
        let type_rvec = type_i8.vec_type(32);
        let i32_type = context.i32_type();
        let values: Vec<_> = (0..32)
            .rev()
            .map(|v| i32_type.const_int(v, false))
            .collect();
        let swap_bytes = VectorType::const_vector(&values);

        let is_little_endian = target_data.get_byte_ordering() == ByteOrdering::LittleEndian;

        Ok(Self {
            context: &context,
            module,
            builder,
            execution_engine,
            type_ptrint,
            type_stackel,
            type_retval,
            type_i8,
            type_ivec,
            type_rvec,
            swap_bytes,
            debug_ir: None,
            debug_asm: None,
            is_little_endian,
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

    // HELPER FUNCTIONS

    fn build_stack_push<'a>(
        &'a self,
        book: JitEvmEngineBookkeeping<'a>,
        val: IntValue<'a>,
    ) -> JitEvmEngineBookkeeping<'a> {
        let sp_offset = self.type_ptrint.const_int(EVM_STACK_ELEMENT_SIZE, false);

        let sp_ptr = self.builder.build_int_to_ptr(
            book.sp,
            self.type_stackel.ptr_type(AddressSpace::Generic),
            "",
        );
        self.builder.build_store(sp_ptr, val);
        let sp = self.builder.build_int_add(book.sp, sp_offset, "");

        book.update_sp(sp)
    }

    fn build_stack_push_vector<'a>(
        &'a self,
        book: JitEvmEngineBookkeeping<'a>,
        val: VectorValue<'a>,
    ) -> JitEvmEngineBookkeeping<'a> {
        let sp_offset = self.type_ptrint.const_int(EVM_STACK_ELEMENT_SIZE, false);

        let sp_ptr = self.builder.build_int_to_ptr(
            book.sp,
            self.type_rvec.ptr_type(AddressSpace::Generic),
            "",
        );
        self.builder.build_store(sp_ptr, val);
        let sp = self.builder.build_int_add(book.sp, sp_offset, "");

        book.update_sp(sp)
    }

    fn build_stack_pop<'a>(
        &'a self,
        book: JitEvmEngineBookkeeping<'a>,
    ) -> (JitEvmEngineBookkeeping<'a>, IntValue<'a>) {
        let sp_offset = self.type_ptrint.const_int(EVM_STACK_ELEMENT_SIZE, false);

        let sp = self.builder.build_int_sub(book.sp, sp_offset, "");
        let sp_ptr = self.builder.build_int_to_ptr(
            sp,
            self.type_stackel.ptr_type(AddressSpace::Generic),
            "",
        );
        let val = self.builder.build_load(sp_ptr, "").into_int_value();

        (book.update_sp(sp), val)
    }

    fn build_stack_pop_vector<'a>(
        &'a self,
        book: JitEvmEngineBookkeeping<'a>,
    ) -> (
        JitEvmEngineBookkeeping<'a>,
        VectorValue<'a>,
        VectorValue<'a>,
    ) {
        let sp0_offset = self.type_ptrint.const_int(EVM_STACK_ELEMENT_SIZE, false);
        let sp1_offset = self
            .type_ptrint
            .const_int(EVM_JIT_STACK_ALIGN as u64, false);

        let sp0 = self.builder.build_int_sub(book.sp, sp0_offset, "");
        let sp1 = self.builder.build_int_sub(book.sp, sp1_offset, "");

        let sp0_ptr =
            self.builder
                .build_int_to_ptr(sp0, self.type_ivec.ptr_type(AddressSpace::Generic), "");
        let sp1_ptr =
            self.builder
                .build_int_to_ptr(sp1, self.type_ivec.ptr_type(AddressSpace::Generic), "");

        let vec0 = self.builder.build_load(sp0_ptr, "").into_vector_value();
        let vec1 = self.builder.build_load(sp1_ptr, "").into_vector_value();

        (book.update_sp(sp0), vec0, vec1)
    }

    fn build_stack_write<'a>(
        &'a self,
        book: JitEvmEngineBookkeeping<'a>,
        idx: u64,
        val: IntValue<'a>,
    ) -> JitEvmEngineBookkeeping<'a> {
        let idx = self
            .type_ptrint
            .const_int(idx * EVM_STACK_ELEMENT_SIZE, false);

        let sp_int = self.builder.build_int_sub(book.sp, idx, "");
        let sp_ptr = self.builder.build_int_to_ptr(
            sp_int,
            self.type_stackel.ptr_type(AddressSpace::Generic),
            "",
        );
        self.builder.build_store(sp_ptr, val);

        book
    }

    fn build_stack_read<'a>(
        &'a self,
        book: JitEvmEngineBookkeeping<'a>,
        idx: u64,
    ) -> (JitEvmEngineBookkeeping<'a>, IntValue<'a>) {
        let idx = self
            .type_ptrint
            .const_int(idx * EVM_STACK_ELEMENT_SIZE, false);

        let sp_int = self.builder.build_int_sub(book.sp, idx, "");
        let sp_ptr = self.builder.build_int_to_ptr(
            sp_int,
            self.type_stackel.ptr_type(AddressSpace::Generic),
            "",
        );
        let val = self.builder.build_load(sp_ptr, "").into_int_value();

        (book, val)
    }

    fn build_dup<'a>(
        &'a self,
        book: JitEvmEngineBookkeeping<'a>,
        idx: u64,
    ) -> Result<JitEvmEngineBookkeeping<'a>, JitEvmEngineError> {
        let len_stackel = self.type_ptrint.const_int(EVM_STACK_ELEMENT_SIZE, false);
        let sp_src_offset = self
            .type_ptrint
            .const_int(idx * EVM_STACK_ELEMENT_SIZE, false);
        let src_int = self.builder.build_int_sub(book.sp, sp_src_offset, "");
        let src_ptr = self.builder.build_int_to_ptr(
            src_int,
            self.type_stackel.ptr_type(AddressSpace::Generic),
            "",
        );
        let dst_ptr = self.builder.build_int_to_ptr(
            book.sp,
            self.type_stackel.ptr_type(AddressSpace::Generic),
            "",
        );
        self.builder.build_memcpy(
            dst_ptr,
            EVM_JIT_STACK_ALIGN,
            src_ptr,
            EVM_JIT_STACK_ALIGN,
            len_stackel,
        )?;
        let sp = self.builder.build_int_add(book.sp, len_stackel, "");
        let book = book.update_sp(sp);

        Ok(book)
    }

    fn build_swap<'a>(
        &'a self,
        book: JitEvmEngineBookkeeping<'a>,
        idx: u64,
    ) -> JitEvmEngineBookkeeping<'a> {
        let (book, a) = self.build_stack_read(book, 1);
        let (book, b) = self.build_stack_read(book, idx);
        let book = self.build_stack_write(book, 1, b);
        let book = self.build_stack_write(book, idx, a);
        book
    }

    fn build_stack_index<'a>(
        &'a self,
        book: JitEvmEngineBookkeeping<'a>,
        idx: u64,
    ) -> IntValue<'a> {
        // let len_stackel = self.type_ptrint.const_int(EVM_STACK_ELEMENT_SIZE, false);
        let sp_offset = self
            .type_ptrint
            .const_int(idx * EVM_STACK_ELEMENT_SIZE, false);
        let sp_int = self.builder.build_int_sub(book.sp, sp_offset, "");
        sp_int
    }

    // CALLBACKS FOR OPERATIONS THAT CANNOT HAPPEN PURELY WITHIN THE EVM

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

    pub fn build(self, code: IndexedEvmCode) -> Result<JitEvmContract<'ctx>, JitEvmEngineError> {
        // CALLBACKS

        let callback_sload_func = {
            // SLOAD
            let cb_type = self
                .type_retval
                .fn_type(&[self.type_ptrint.into(), self.type_ptrint.into()], false);
            let cb_func = self.module.add_function("callback_sload", cb_type, None);
            self.execution_engine
                .add_global_mapping(&cb_func, JitContractBuilder::callback_sload as usize);
            cb_func
        };

        let callback_sstore_func = {
            // SSTORE
            let cb_type = self
                .type_retval
                .fn_type(&[self.type_ptrint.into(), self.type_ptrint.into()], false);
            let cb_func = self.module.add_function("callback_sstore", cb_type, None);
            self.execution_engine
                .add_global_mapping(&cb_func, JitContractBuilder::callback_sstore as usize);
            cb_func
        };

        // SETUP JIT'ED CONTRACT FUNCTION

        let executecontract_fn_type = self.type_retval.fn_type(&[self.type_ptrint.into()], false);
        let function = self
            .module
            .add_function("executecontract", executecontract_fn_type, None);

        // SETUP HANDLER

        let setup_block = self.context.append_basic_block(function, "setup");
        self.builder.position_at_end(setup_block);

        let setup_book = {
            let execution_context = function.get_nth_param(0).unwrap().into_int_value();
            let execution_context_ptr = self.builder.build_int_to_ptr(
                execution_context,
                self.type_ptrint.ptr_type(AddressSpace::Generic),
                "",
            );
            let sp_int = self
                .builder
                .build_load(execution_context_ptr, "")
                .into_int_value();
            let max_offset = (EVM_STACK_SIZE - 1) as u64 * EVM_STACK_ELEMENT_SIZE;
            let sp_max = self.builder.build_int_add(
                sp_int,
                self.type_ptrint.const_int(max_offset, false),
                "",
            );
            let mem_offset =
                self.builder
                    .build_int_add(execution_context, self.type_ptrint.size_of(), "");
            let mem_ptr = self.builder.build_int_to_ptr(
                mem_offset,
                self.type_ptrint.ptr_type(AddressSpace::Generic),
                "",
            );
            let mem = self.builder.build_load(mem_ptr, "").into_int_value();
            // let retval = self.type_retval.const_int(0, false);
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
                &self,
                block_before,
                &label,
                &format!("_{}", i),
            ));
        }

        self.builder.position_at_end(setup_block);
        self.builder
            .build_unconditional_branch(instructions[0].block);
        instructions[0].phi_setup_block(&setup_book, &setup_block);

        // END HANDLER

        let end =
            JitEvmEngineSimpleBlock::new(&self, instructions[ops_len - 1].block, &"end", &"-end");
        self.builder
            .build_return(Some(&self.type_retval.const_int(0, false)));

        // ERROR-JUMPDEST HANDLER

        let error_jumpdest =
            JitEvmEngineSimpleBlock::new(&self, end.block, &"error-jumpdest", &"-error-jumpdest");
        self.builder
            .build_return(Some(&self.type_retval.const_int(1, false)));

        // RENDER INSTRUCTIONS

        for (i, op) in code.code.ops.iter().enumerate() {
            use EvmOp::*;

            let this = instructions[i];

            self.builder.position_at_end(this.block);
            let book = JitEvmEngineBookkeeping {
                execution_context: this.phi_execution_context.as_basic_value().into_int_value(),
                sp_min: this.phi_sp_min.as_basic_value().into_int_value(),
                sp_max: this.phi_sp_max.as_basic_value().into_int_value(),
                sp: this.phi_sp.as_basic_value().into_int_value(),
                mem: this.phi_mem.as_basic_value().into_int_value(),
                // retval: this.phi_retval.as_basic_value().into_int_value(),
            };

            let next = if i + 1 == ops_len {
                end
            } else {
                instructions[i + 1]
            };

            let book = match op {
                Stop => {
                    let val = self.type_retval.const_int(0, false);
                    self.builder.build_return(Some(&val));
                    continue; // skip auto-generated jump to next instruction
                }
                Push(_, val) => {
                    let val = self.type_stackel.const_int_arbitrary_precision(&val.0);
                    let book = self.build_stack_push(book, val);
                    book
                }
                Pop => {
                    let (book, _) = self.build_stack_pop(book);
                    book
                }
                Jumpdest => book,
                Mstore => {
                    let (book, offset) = self.build_stack_pop(book);
                    let (book, vec0, vec1) = self.build_stack_pop_vector(book);

                    let shuffled =
                        self.builder
                            .build_shuffle_vector(vec0, vec1, self.swap_bytes, "");

                    let casted = self.builder.build_int_cast(offset, self.type_ptrint, "");
                    let mem = self.builder.build_int_add(book.mem, casted, "");
                    let dest_ptr = self.builder.build_int_to_ptr(
                        mem,
                        self.type_stackel.ptr_type(AddressSpace::Generic),
                        "",
                    );

                    self.builder
                        .build_store(dest_ptr, shuffled)
                        .set_alignment(1)
                        .unwrap();

                    book
                }
                Mstore8 => {
                    let (book, offset) = self.build_stack_pop(book);
                    let (book, value) = self.build_stack_pop(book);
                    let value_casted = self.builder.build_int_cast(value, self.type_i8, "");
                    let offset_casted = self.builder.build_int_cast(offset, self.type_ptrint, "");

                    let mem = self.builder.build_int_add(book.mem, offset_casted, "");
                    let dest_ptr = self.builder.build_int_to_ptr(
                        mem,
                        self.type_i8.ptr_type(AddressSpace::Generic),
                        "",
                    );

                    self.builder.build_store(dest_ptr, value_casted);

                    book
                }
                Mload => {
                    // TODO: memory bounds checks
                    let (book, offset) = self.build_stack_pop(book);
                    let casted = self.builder.build_int_cast(offset, self.type_ptrint, "");

                    let mem0 = self.builder.build_int_add(book.mem, casted, "");
                    let mem1_offset = self
                        .type_ptrint
                        .const_int(EVM_JIT_STACK_ALIGN as u64, false);
                    let mem1 = self.builder.build_int_add(mem0, mem1_offset, "");

                    let mem0_ptr = self.builder.build_int_to_ptr(
                        mem0,
                        self.type_ivec.ptr_type(AddressSpace::Generic),
                        "",
                    );
                    let mem1_ptr = self.builder.build_int_to_ptr(
                        mem1,
                        self.type_ivec.ptr_type(AddressSpace::Generic),
                        "",
                    );

                    let vec0 = self.builder.build_load(mem0_ptr, "").into_vector_value();
                    let vec1 = self.builder.build_load(mem1_ptr, "").into_vector_value();

                    vec0.as_instruction_value()
                        .unwrap()
                        .set_alignment(1)
                        .unwrap();
                    vec1.as_instruction_value()
                        .unwrap()
                        .set_alignment(1)
                        .unwrap();

                    let shuffled =
                        self.builder
                            .build_shuffle_vector(vec0, vec1, self.swap_bytes, "");

                    let book = self.build_stack_push_vector(book, shuffled);
                    book
                }
                Sload => {
                    let _retval = self
                        .builder
                        .build_call(
                            callback_sload_func,
                            &[book.execution_context.into(), book.sp.into()],
                            "",
                        )
                        .try_as_basic_value()
                        .left()
                        .unwrap()
                        .into_int_value();
                    book
                }
                Sstore => {
                    let _retval = self
                        .builder
                        .build_call(
                            callback_sstore_func,
                            &[book.execution_context.into(), book.sp.into()],
                            "",
                        )
                        .try_as_basic_value()
                        .left()
                        .unwrap()
                        .into_int_value();
                    let (book, _) = self.build_stack_pop(book);
                    let (book, _) = self.build_stack_pop(book);
                    book
                }
                Jump => {
                    // TODO: optimise this, can definitely map solidity opcode target -> basic block label
                    let (book, target) = self.build_stack_pop(book);

                    if code.jumpdests.is_empty() {
                        // there are no valid jump targets, this Jump has to fail!
                        // TODO: should this be the error block?
                        self.builder.build_unconditional_branch(end.block);
                        end.add_incoming(&book, &this);
                    } else {
                        let mut jump_table: Vec<JitEvmEngineSimpleBlock<'_>> = Vec::new();
                        for (j, jmp_i) in code.jumpdests.iter().enumerate() {
                            let jmp_target = code.opidx2target[jmp_i];
                            jump_table.push(JitEvmEngineSimpleBlock::new(
                                &self,
                                if j == 0 {
                                    this.block
                                } else {
                                    jump_table[j - 1].block
                                },
                                &format!(
                                    "instruction #{}: {:?} / to Jumpdest #{} at op #{} to byte #{}",
                                    i, op, j, jmp_i, jmp_target
                                ),
                                &format!("_{}_{}", i, j),
                            ));
                        }

                        self.builder.position_at_end(this.block);
                        self.builder.build_unconditional_branch(jump_table[0].block);
                        jump_table[0].add_incoming(&book, &this);

                        for (j, jmp_i) in code.jumpdests.iter().enumerate() {
                            let jmp_target = code.opidx2target[jmp_i];
                            let jmp_target = jmp_target.as_u64(); // REMARK: assumes that code cannot exceed 2^64 instructions, probably ok ;)
                            self.builder.position_at_end(jump_table[j].block);
                            let cmp = self.builder.build_int_compare(
                                IntPredicate::EQ,
                                self.type_stackel.const_int(jmp_target, false),
                                target,
                                "",
                            );
                            if j + 1 == code.jumpdests.len() {
                                self.builder.build_conditional_branch(
                                    cmp,
                                    instructions[*jmp_i].block,
                                    error_jumpdest.block,
                                );
                                instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
                                error_jumpdest.add_incoming(&book, &jump_table[j]);
                            } else {
                                self.builder.build_conditional_branch(
                                    cmp,
                                    instructions[*jmp_i].block,
                                    jump_table[j + 1].block,
                                );
                                instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
                                jump_table[j + 1].add_incoming(&book, &jump_table[j]);
                            }
                        }
                    }

                    continue; // skip auto-generated jump to next instruction
                }
                Jumpi => {
                    let (book, target) = self.build_stack_pop(book);
                    let (book, val) = self.build_stack_pop(book);

                    if code.jumpdests.is_empty() {
                        // there are no valid jump targets, this Jumpi has to fail!
                        self.builder.build_unconditional_branch(end.block);
                        end.add_incoming(&book, &this);
                    } else {
                        let mut jump_table: Vec<JitEvmEngineSimpleBlock<'_>> = Vec::new();
                        for (j, jmp_i) in code.jumpdests.iter().enumerate() {
                            let jmp_target = code.opidx2target[jmp_i];
                            jump_table.push(JitEvmEngineSimpleBlock::new(
                                &self,
                                if j == 0 {
                                    this.block
                                } else {
                                    jump_table[j - 1].block
                                },
                                &format!(
                                    "instruction #{}: {:?} / to Jumpdest #{} at op #{} to byte #{}",
                                    i, op, j, jmp_i, jmp_target
                                ),
                                &format!("_{}_{}", i, j),
                            ));
                        }

                        self.builder.position_at_end(this.block);
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::EQ,
                            self.type_stackel.const_int(0, false),
                            val,
                            "",
                        );
                        self.builder
                            .build_conditional_branch(cmp, next.block, jump_table[0].block);
                        next.add_incoming(&book, &this);
                        jump_table[0].add_incoming(&book, &this);

                        for (j, jmp_i) in code.jumpdests.iter().enumerate() {
                            let jmp_target = code.opidx2target[jmp_i];
                            let jmp_target = jmp_target.as_u64(); // REMARK: assumes that code cannot exceed 2^64 instructions, probably ok ;)
                            self.builder.position_at_end(jump_table[j].block);
                            let cmp = self.builder.build_int_compare(
                                IntPredicate::EQ,
                                self.type_stackel.const_int(jmp_target, false),
                                target,
                                "",
                            );
                            if j + 1 == code.jumpdests.len() {
                                self.builder.build_conditional_branch(
                                    cmp,
                                    instructions[*jmp_i].block,
                                    error_jumpdest.block,
                                );
                                instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
                                error_jumpdest.add_incoming(&book, &jump_table[j]);
                            } else {
                                self.builder.build_conditional_branch(
                                    cmp,
                                    instructions[*jmp_i].block,
                                    jump_table[j + 1].block,
                                );
                                instructions[*jmp_i].add_incoming(&book, &jump_table[j]);
                                jump_table[j + 1].add_incoming(&book, &jump_table[j]);
                            }
                        }
                    }

                    continue; // skip auto-generated jump to next instruction
                }
                Swap1 => self.build_swap(book, 1 + 1),
                Swap2 => self.build_swap(book, 2 + 1),
                Swap3 => self.build_swap(book, 3 + 1),
                Swap4 => self.build_swap(book, 4 + 1),
                Swap5 => self.build_swap(book, 5 + 1),
                Swap6 => self.build_swap(book, 6 + 1),
                Swap7 => self.build_swap(book, 7 + 1),
                Swap8 => self.build_swap(book, 8 + 1),
                Swap9 => self.build_swap(book, 9 + 1),
                Swap10 => self.build_swap(book, 10 + 1),
                Swap11 => self.build_swap(book, 11 + 1),
                Swap12 => self.build_swap(book, 12 + 1),
                Swap13 => self.build_swap(book, 13 + 1),
                Swap14 => self.build_swap(book, 14 + 1),
                Swap15 => self.build_swap(book, 15 + 1),
                Swap16 => self.build_swap(book, 16 + 1),
                Dup1 => self.build_dup(book, 1)?,
                Dup2 => self.build_dup(book, 2)?,
                Dup3 => self.build_dup(book, 3)?,
                Dup4 => self.build_dup(book, 4)?,
                Dup5 => self.build_dup(book, 5)?,
                Dup6 => self.build_dup(book, 6)?,
                Dup7 => self.build_dup(book, 7)?,
                Dup8 => self.build_dup(book, 8)?,
                Dup9 => self.build_dup(book, 9)?,
                Dup10 => self.build_dup(book, 10)?,
                Dup11 => self.build_dup(book, 11)?,
                Dup12 => self.build_dup(book, 12)?,
                Dup13 => self.build_dup(book, 13)?,
                Dup14 => self.build_dup(book, 14)?,
                Dup15 => self.build_dup(book, 15)?,
                Dup16 => self.build_dup(book, 16)?,
                Iszero => {
                    // TODO: this can also be optimized...
                    let (book, val) = self.build_stack_pop(book);
                    let cmp = self.builder.build_int_compare(
                        IntPredicate::EQ,
                        self.type_stackel.const_int(0, false),
                        val,
                        "",
                    );

                    let push_0 = JitEvmEngineSimpleBlock::new(
                        &self,
                        instructions[i].block,
                        &format!("Instruction #{}: {:?} / push 0", i, op),
                        &format!("_{}_0", i),
                    );
                    let push_1 = JitEvmEngineSimpleBlock::new(
                        &self,
                        push_0.block,
                        &format!("Instruction #{}: {:?} / push 1", i, op),
                        &format!("_{}_1", i),
                    );

                    self.builder.position_at_end(this.block);
                    self.builder
                        .build_conditional_branch(cmp, push_1.block, push_0.block);
                    push_0.add_incoming(&book, &this);
                    push_1.add_incoming(&book, &this);

                    self.builder.position_at_end(push_0.block);
                    let book_0 = self.build_stack_push(book, self.type_stackel.const_int(0, false));
                    self.builder.build_unconditional_branch(next.block);
                    next.add_incoming(&book_0, &push_0);

                    self.builder.position_at_end(push_1.block);
                    let book_1 = self.build_stack_push(book, self.type_stackel.const_int(1, false));
                    self.builder.build_unconditional_branch(next.block);
                    next.add_incoming(&book_1, &push_1);

                    continue; // skip auto-generated jump to next instruction
                }
                Add => {
                    op2_llvmnativei256_operation!(self, book, build_int_add)
                }
                Sub => {
                    op2_llvmnativei256_operation!(self, book, build_int_sub)
                }
                Mul => {
                    op2_llvmnativei256_operation!(self, book, build_int_mul)
                }
                Div => {
                    op2_llvmnativei256_operation!(self, book, build_int_unsigned_div)
                }
                Sdiv => {
                    op2_llvmnativei256_operation!(self, book, build_int_signed_div)
                }
                Mod => {
                    op2_llvmnativei256_operation!(self, book, build_int_unsigned_rem)
                }
                Smod => {
                    op2_llvmnativei256_operation!(self, book, build_int_signed_rem)
                }
                Shl => {
                    let (book, r) = self.build_stack_pop(book);
                    let (book, l) = self.build_stack_pop(book);
                    let d = self.builder.build_left_shift(l, r, "");
                    let book = self.build_stack_push(book, d);
                    book
                }
                Shr => {
                    let (book, r) = self.build_stack_pop(book);
                    let (book, l) = self.build_stack_pop(book);
                    let d = self.builder.build_right_shift(l, r, false, "");
                    let book = self.build_stack_push(book, d);
                    book
                }
                Sar => {
                    let (book, r) = self.build_stack_pop(book);
                    let (book, l) = self.build_stack_pop(book);
                    let d = self.builder.build_right_shift(l, r, true, "");
                    let book = self.build_stack_push(book, d);
                    book
                }
                Exp => {
                    op2_double_add_exp!(self, book, this, next, i);
                    continue;
                }
                Eq => {
                    op2_llvmnativei256_compare_operation!(
                        self,
                        book,
                        this,
                        next,
                        instructions,
                        i,
                        op,
                        IntPredicate::EQ
                    )
                }
                Lt => {
                    op2_llvmnativei256_compare_operation!(
                        self,
                        book,
                        this,
                        next,
                        instructions,
                        i,
                        op,
                        IntPredicate::ULT
                    )
                }
                Gt => {
                    op2_llvmnativei256_compare_operation!(
                        self,
                        book,
                        this,
                        next,
                        instructions,
                        i,
                        op,
                        IntPredicate::UGT
                    )
                }
                Slt => {
                    op2_llvmnativei256_compare_operation!(
                        self,
                        book,
                        this,
                        next,
                        instructions,
                        i,
                        op,
                        IntPredicate::SLT
                    )
                }
                Sgt => {
                    op2_llvmnativei256_compare_operation!(
                        self,
                        book,
                        this,
                        next,
                        instructions,
                        i,
                        op,
                        IntPredicate::SGT
                    )
                }
                And => {
                    op2_llvmnativei256_operation!(self, book, build_and)
                }
                Or => {
                    op2_llvmnativei256_operation!(self, book, build_or)
                }
                Xor => {
                    op2_llvmnativei256_operation!(self, book, build_xor)
                }
                Not => {
                    op1_llvmnativei256_operation!(self, book, build_not)
                }
                Signextend => {
                    let (book, x) = self.build_stack_pop(book);

                    let const_32 = self.type_stackel.const_int(32, false);
                    let cmp = self
                        .builder
                        .build_int_compare(IntPredicate::UGE, x, const_32, "");

                    let label = format!("Instruction #{}: Signextend / else", i);
                    let index = format!("_{}", i);
                    let else_block =
                        JitEvmEngineSimpleBlock::new(&self, this.block, &label, &index);
                    self.builder.position_at_end(this.block);
                    self.builder
                        .build_conditional_branch(cmp, next.block, else_block.block);

                    next.add_incoming(&book, &this);
                    else_block.add_incoming(&book, &this);

                    self.builder.position_at_end(else_block.block);

                    let (book, y) = self.build_stack_pop(book);

                    let const_1 = self.type_stackel.const_int(1, false);
                    let const_7 = self.type_stackel.const_int(7, false);
                    let const_8 = self.type_stackel.const_int(8, false);

                    let x_8 = self.builder.build_int_mul(x, const_8, "");
                    let bit_index = self.builder.build_int_add(x_8, const_7, "");
                    let bit = self.builder.build_left_shift(const_1, bit_index, "");
                    let mask = self.builder.build_int_sub(bit, const_1, "");

                    let sign = self.builder.build_and(y, bit, "");
                    let is_signed = self
                        .builder
                        .build_int_compare(IntPredicate::EQ, sign, bit, "");

                    let not_mask = self.builder.build_not(mask, "");
                    let extended = self.builder.build_or(y, not_mask, "");
                    let unextended = self.builder.build_and(y, mask, "");

                    let result = self
                        .builder
                        .build_select(is_signed, extended, unextended, "")
                        .into_int_value();

                    self.build_stack_push(book, result);
                    self.builder.build_unconditional_branch(next.block);

                    continue; // skip auto-generated jump to next instruction
                }
                AugmentedPushJump(_, val) => {
                    if code.jumpdests.is_empty() {
                        // there are no valid jump targets, this Jump has to fail!
                        self.builder.build_unconditional_branch(end.block);
                        end.add_incoming(&book, &this);
                    } else {
                        // retrieve the corresponding jump target (panic if not a valid jump target) ...
                        let jmp_i = code.target2opidx[val];
                        // ... and jump to there!
                        self.builder
                            .build_unconditional_branch(instructions[jmp_i].block);
                        instructions[jmp_i].add_incoming(&book, &this);
                    }

                    continue; // skip auto-generated jump to next instruction
                }
                AugmentedPushJumpi(_, val) => {
                    let (book, condition) = self.build_stack_pop(book);

                    if code.jumpdests.is_empty() {
                        // there are no valid jump targets, this Jumpi has to fail!
                        self.builder.build_unconditional_branch(end.block);
                        end.add_incoming(&book, &this);
                    } else {
                        // retrieve the corresponding jump target (panic if not a valid jump target) ...
                        let jmp_i = code.target2opidx[val];
                        // ... and jump to there (conditionally)!
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::EQ,
                            self.type_stackel.const_int(0, false),
                            condition,
                            "",
                        );
                        self.builder.build_conditional_branch(
                            cmp,
                            next.block,
                            instructions[jmp_i].block,
                        );
                        next.add_incoming(&book, &this);
                        instructions[jmp_i].add_incoming(&book, &this);
                    }

                    continue; // skip auto-generated jump to next instruction
                }

                _ => {
                    panic!("Op not implemented: {:?}", op);
                }
            };

            self.builder.build_unconditional_branch(next.block);
            next.add_incoming(&book, &this);
        }

        // OUTPUT LLVM
        if let Some(path) = self.debug_ir {
            self.module.print_to_file(path)?;
        }

        // OUTPUT ASM
        if let Some(path) = self.debug_asm {
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

            machine.write_to_file(&self.module, FileType::Assembly, path.as_ref())?;
        }

        // COMPILE
        let function: JitFunction<JitEvmCompiledContract> =
            unsafe { self.execution_engine.get_function("executecontract")? };
        Ok(JitEvmContract { function })
    }
}
