macro_rules! build_stack_push {
    ($ctx:expr, $book:ident, $val:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);

        let sp_ptr = $ctx.builder.build_int_to_ptr(
            $book.sp,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        $ctx.builder.build_store(sp_ptr, $val)?;
        let sp = $ctx.builder.build_int_add($book.sp, sp_offset, "")?;

        $book.update_sp(sp)
    }};
}

macro_rules! build_stack_pop {
    ($ctx:expr, $book:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);

        let sp = $ctx.builder.build_int_sub($book.sp, sp_offset, "")?;
        let sp_ptr = $ctx.builder.build_int_to_ptr(
            sp,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        let val = $ctx.builder.build_load(sp_ptr, "")?.into_int_value();

        ($book.update_sp(sp), val)
    }};
}

macro_rules! build_stack_push_vector {
    ($ctx:expr, $book:ident, $val:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);

        let sp_ptr = $ctx.builder.build_int_to_ptr(
            $book.sp,
            $ctx.types.type_rvec.ptr_type(AddressSpace::default()),
            "",
        )?;
        $ctx.builder.build_store(sp_ptr, $val)?;
        let sp = $ctx.builder.build_int_add($book.sp, sp_offset, "")?;

        $book.update_sp(sp)
    }};
}

macro_rules! build_stack_pop_vector {
    ($ctx:expr, $book:ident) => {{
        use crate::jit::{EVM_JIT_STACK_ALIGN, EVM_STACK_ELEMENT_SIZE};
        let sp0_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);
        let sp1_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_JIT_STACK_ALIGN as u64, false);

        let sp0 = $ctx.builder.build_int_sub($book.sp, sp0_offset, "")?;
        let sp1 = $ctx.builder.build_int_sub($book.sp, sp1_offset, "")?;

        let sp0_ptr = $ctx.builder.build_int_to_ptr(
            sp0,
            $ctx.types.type_ivec.ptr_type(AddressSpace::default()),
            "",
        )?;
        let sp1_ptr = $ctx.builder.build_int_to_ptr(
            sp1,
            $ctx.types.type_ivec.ptr_type(AddressSpace::default()),
            "",
        )?;

        let vec0 = $ctx.builder.build_load(sp0_ptr, "")?.into_vector_value();
        let vec1 = $ctx.builder.build_load(sp1_ptr, "")?.into_vector_value();

        ($book.update_sp(sp0), vec0, vec1)
    }};
}

macro_rules! build_stack_inc {
    ($ctx:expr, $book:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let sp_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);
        let sp = $ctx.builder.build_int_add($book.sp, sp_offset, "")?;

        $book.update_sp(sp)
    }};
}

macro_rules! build_stack_write {
    ($ctx:expr, $book:ident, $idx:expr, $val:ident) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let idx = $ctx
            .types
            .type_ptrint
            .const_int($idx * EVM_STACK_ELEMENT_SIZE, false);

        let sp_int = $ctx.builder.build_int_sub($book.sp, idx, "")?;
        let sp_ptr = $ctx.builder.build_int_to_ptr(
            sp_int,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        $ctx.builder.build_store(sp_ptr, $val)?;

        $book
    }};
}

macro_rules! build_stack_read {
    ($ctx:expr, $book:ident, $idx:expr) => {{
        use crate::jit::EVM_STACK_ELEMENT_SIZE;
        let idx = $ctx
            .types
            .type_ptrint
            .const_int($idx * EVM_STACK_ELEMENT_SIZE, false);

        let sp_int = $ctx.builder.build_int_sub($book.sp, idx, "")?;
        let sp_ptr = $ctx.builder.build_int_to_ptr(
            sp_int,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        let val = $ctx.builder.build_load(sp_ptr, "")?.into_int_value();

        ($book, val)
    }};
}

macro_rules! build_stack_swap {
    ($ctx:expr, $book:ident, $idx:expr) => {{
        let (book, a) = build_stack_read!($ctx, $book, 1);
        let (book, b) = build_stack_read!($ctx, book, $idx);
        let book = build_stack_write!($ctx, book, 1, b);
        let book = build_stack_write!($ctx, book, $idx, a);
        book
    }};
}

macro_rules! build_dup {
    ($ctx:expr, $book:ident, $idx:expr) => {{
        use crate::jit::{EVM_JIT_STACK_ALIGN, EVM_STACK_ELEMENT_SIZE};
        let len_stackel = $ctx
            .types
            .type_ptrint
            .const_int(EVM_STACK_ELEMENT_SIZE, false);
        let sp_src_offset = $ctx
            .types
            .type_ptrint
            .const_int($idx * EVM_STACK_ELEMENT_SIZE, false);
        let src_int = $ctx.builder.build_int_sub($book.sp, sp_src_offset, "")?;
        let src_ptr = $ctx.builder.build_int_to_ptr(
            src_int,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        let dst_ptr = $ctx.builder.build_int_to_ptr(
            $book.sp,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;
        $ctx.builder.build_memcpy(
            dst_ptr,
            EVM_JIT_STACK_ALIGN,
            src_ptr,
            EVM_JIT_STACK_ALIGN,
            len_stackel,
        )?;
        let sp = $ctx.builder.build_int_add($book.sp, len_stackel, "")?;

        $book.update_sp(sp)
    }};
}

pub(crate) use build_stack_inc;
pub(crate) use build_stack_pop;
pub(crate) use build_stack_push;
