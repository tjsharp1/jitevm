use crate::jit::{
    types::JitTypes, JitContractBuilder, JitEvmEngineBookkeeping, JitEvmEngineError,
    EVM_STACK_ELEMENT_SIZE,
};
use inkwell::{values::IntValue, AddressSpace};

macro_rules! build_stack_push {
    ($types:expr, $builder:expr, $book:ident, $val:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $types.type_ptrint.const_int(EVM_STACK_ELEMENT_SIZE, false);

        let sp_ptr = $builder.build_int_to_ptr(
            $book.sp,
            $types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        $builder.build_store(sp_ptr, $val)?;
        let sp = $builder.build_int_add($book.sp, sp_offset, "")?;

        $book.update_sp(sp)
    }};
}

macro_rules! build_stack_pop {
    ($types:expr, $builder:expr, $book:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $types.type_ptrint.const_int(EVM_STACK_ELEMENT_SIZE, false);

        let sp = $builder.build_int_sub($book.sp, sp_offset, "")?;
        let sp_ptr = $builder.build_int_to_ptr(
            sp,
            $types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        let val = $builder.build_load(sp_ptr, "")?.into_int_value();

        ($book.update_sp(sp), val)
    }};
}

macro_rules! build_stack_push_vector {
    ($types:ident, $builder:expr, $book:ident, $val:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $types.type_ptrint.const_int(EVM_STACK_ELEMENT_SIZE, false);

        let sp_ptr = $builder.build_int_to_ptr(
            $book.sp,
            $types.type_rvec.ptr_type(AddressSpace::default()),
            "",
        )?;
        $builder.build_store(sp_ptr, $val)?;
        let sp = $builder.build_int_add($book.sp, sp_offset, "")?;

        $book.update_sp(sp)
    }};
}

macro_rules! build_stack_pop_vector {
    ($types:ident, $builder:expr, $book:ident) => {{
        use crate::jit::{EVM_JIT_STACK_ALIGN, EVM_STACK_ELEMENT_SIZE};
        let sp0_offset = $types.type_ptrint.const_int(EVM_STACK_ELEMENT_SIZE, false);
        let sp1_offset = $types
            .type_ptrint
            .const_int(EVM_JIT_STACK_ALIGN as u64, false);

        let sp0 = $builder.build_int_sub($book.sp, sp0_offset, "")?;
        let sp1 = $builder.build_int_sub($book.sp, sp1_offset, "")?;

        let sp0_ptr = $builder.build_int_to_ptr(
            sp0,
            $types.type_ivec.ptr_type(AddressSpace::default()),
            "",
        )?;
        let sp1_ptr = $builder.build_int_to_ptr(
            sp1,
            $types.type_ivec.ptr_type(AddressSpace::default()),
            "",
        )?;

        let vec0 = $builder.build_load(sp0_ptr, "")?.into_vector_value();
        let vec1 = $builder.build_load(sp1_ptr, "")?.into_vector_value();

        ($book.update_sp(sp0), vec0, vec1)
    }};
}

macro_rules! build_stack_inc {
    ($types:expr, $builder:expr, $book:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $types.type_ptrint.const_int(EVM_STACK_ELEMENT_SIZE, false);
        let sp = $builder.build_int_add($book.sp, sp_offset, "")?;

        $book.update_sp(sp)
    }};
}

macro_rules! build_stack_write {
    ($types:expr, $builder:expr, $book:ident, $idx:expr, $val:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let idx = $types
            .type_ptrint
            .const_int($idx * EVM_STACK_ELEMENT_SIZE, false);

        let sp_int = $builder.build_int_sub($book.sp, idx, "")?;
        let sp_ptr = $builder.build_int_to_ptr(
            sp_int,
            $types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        $builder.build_store(sp_ptr, $val)?;

        $book
    }};
}

macro_rules! build_stack_read {
    ($types:expr, $builder:expr, $book:ident, $idx:expr) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let idx = $types
            .type_ptrint
            .const_int($idx * EVM_STACK_ELEMENT_SIZE, false);

        let sp_int = $builder.build_int_sub($book.sp, idx, "")?;
        let sp_ptr = $builder.build_int_to_ptr(
            sp_int,
            $types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        let val = $builder.build_load(sp_ptr, "")?.into_int_value();

        ($book, val)
    }};
}

macro_rules! build_stack_swap {
    ($types:expr, $builder:expr, $book:ident, $idx:expr) => {{
        let (book, a) = build_stack_read!($types, $builder, $book, 1);
        let (book, b) = build_stack_read!($types, $builder, book, $idx);
        let book = build_stack_write!($types, $builder, book, 1, b);
        let book = build_stack_write!($types, $builder, book, $idx, a);
        book
    }};
}

macro_rules! build_dup {
    ($types:expr, $builder:expr, $book:ident, $idx:expr) => {{
        use crate::jit::{EVM_JIT_STACK_ALIGN, EVM_STACK_ELEMENT_SIZE};
        let len_stackel = $types.type_ptrint.const_int(EVM_STACK_ELEMENT_SIZE, false);
        let sp_src_offset = $types
            .type_ptrint
            .const_int($idx * EVM_STACK_ELEMENT_SIZE, false);
        let src_int = $builder.build_int_sub($book.sp, sp_src_offset, "")?;
        let src_ptr = $builder.build_int_to_ptr(
            src_int,
            $types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        let dst_ptr = $builder.build_int_to_ptr(
            $book.sp,
            $types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        $builder.build_memcpy(
            dst_ptr,
            EVM_JIT_STACK_ALIGN,
            src_ptr,
            EVM_JIT_STACK_ALIGN,
            len_stackel,
        )?;
        let sp = $builder.build_int_add($book.sp, len_stackel, "")?;

        $book.update_sp(sp)
    }};
}

pub(crate) use build_stack_inc;
pub(crate) use build_stack_pop;
pub(crate) use build_stack_push;
