use crate::jit::EVM_STACK_ELEMENT_SIZE;
use inkwell::{
    context::Context,
    execution_engine::ExecutionEngine,
    targets::ByteOrdering,
    types::{IntType, VectorType, VoidType},
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
    /// Types for vectorized byte swapping, to re-order endianness
    pub type_ivec: VectorType<'ctx>,
    pub type_rvec: VectorType<'ctx>,
    pub swap_bytes: VectorValue<'ctx>,
    pub is_little_endian: bool,
}

impl<'ctx> JitTypes<'ctx> {
    pub fn new(context: &'ctx Context, engine: &ExecutionEngine<'ctx>) -> JitTypes<'ctx> {
        let target_data = engine.get_target_data();
        let is_little_endian = target_data.get_byte_ordering() == ByteOrdering::LittleEndian;

        let type_ptrint = context.ptr_sized_int_type(&target_data, None);
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

        JitTypes {
            type_ptrint,
            type_stackel,
            type_retval,
            type_bool,
            type_void,
            type_i8,
            type_ivec,
            type_rvec,
            swap_bytes,
            is_little_endian,
        }
    }
}
