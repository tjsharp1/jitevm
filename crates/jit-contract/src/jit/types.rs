use crate::jit::{
    context::{BlockContext, JitContractExecutionResult, JitEvmPtrs, TransactionContext},
    EVM_STACK_ELEMENT_SIZE,
};
use inkwell::{
    context::Context,
    execution_engine::ExecutionEngine,
    targets::ByteOrdering,
    types::{IntType, StructType, VectorType, VoidType},
    values::VectorValue,
};

#[derive(Clone, Debug)]
pub struct JitTypes<'ctx> {
    /// Type for host pointers
    pub type_ptrint: IntType<'ctx>,
    /// Type for evm stack elements
    pub type_stackel: IntType<'ctx>,
    pub type_bool: IntType<'ctx>,
    pub type_void: VoidType<'ctx>,
    pub type_retval: IntType<'ctx>,
    pub type_i8: IntType<'ctx>,
    pub type_i32: IntType<'ctx>,
    pub type_i64: IntType<'ctx>,
    /// Types for vectorized byte swapping, to re-order endianness
    pub type_ivec: VectorType<'ctx>,
    pub type_rvec: VectorType<'ctx>,
    pub execution_context: StructType<'ctx>,
    pub execution_result: StructType<'ctx>,
    pub block_context: StructType<'ctx>,
    pub transaction_context: StructType<'ctx>,
    pub swap_bytes: VectorValue<'ctx>,
    pub is_little_endian: bool,
}

impl<'ctx> JitTypes<'ctx> {
    pub fn new(context: &'ctx Context, engine: &ExecutionEngine<'ctx>) -> JitTypes<'ctx> {
        let target_data = engine.get_target_data();
        let is_little_endian = target_data.get_byte_ordering() == ByteOrdering::LittleEndian;

        let type_ptrint = context.ptr_sized_int_type(target_data, None);
        assert_eq!(type_ptrint.get_bit_width(), 64);
        assert_eq!(usize::BITS, 64);

        let type_stackel = context.custom_width_int_type(256);
        assert_eq!(type_stackel.get_bit_width(), 256);
        assert_eq!(
            type_stackel.get_bit_width() as u64,
            EVM_STACK_ELEMENT_SIZE * 8
        );

        let type_retval = context.i64_type();
        assert_eq!(type_retval.get_bit_width(), 64);
        assert_eq!(u64::BITS, 64);

        let type_i32 = context.i32_type();
        let type_i64 = context.i64_type();

        // types for vectorized byte swapping
        let type_i8 = context.i8_type();
        let type_ivec = type_i8.vec_type(16);
        let type_rvec = type_i8.vec_type(32);

        let i32_type = context.i32_type();
        let values: Vec<_> = (0..32)
            .rev()
            .map(|v| i32_type.const_int(v, false))
            .collect();
        let swap_bytes = VectorType::const_vector(&values);

        let type_bool = context.bool_type();
        let type_void = context.void_type();

        let execution_context = JitEvmPtrs::llvm_struct_type(context, target_data);
        let execution_result = JitContractExecutionResult::llvm_struct_type(context, target_data);
        let block_context = BlockContext::llvm_struct_type(context, target_data);
        let transaction_context = TransactionContext::llvm_struct_type(context, target_data);

        JitTypes {
            type_ptrint,
            type_stackel,
            type_retval,
            type_bool,
            type_void,
            type_i8,
            type_i32,
            type_i64,
            type_ivec,
            type_rvec,
            execution_context,
            execution_result,
            block_context,
            transaction_context,
            swap_bytes,
            is_little_endian,
        }
    }
}
