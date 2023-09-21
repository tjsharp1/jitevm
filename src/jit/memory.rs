// TODO: memory growth checks
macro_rules! build_mstore {
    ($ctx:ident, $book:ident) => {{
        let (book, offset) = build_stack_pop!($ctx, $book);
        let (book, vec0, vec1) = build_stack_pop_vector!($ctx, book);

        let shuffled = $ctx
            .builder
            .build_shuffle_vector(vec0, vec1, $ctx.types.swap_bytes, "")?;

        let casted = $ctx
            .builder
            .build_int_cast(offset, $ctx.types.type_ptrint, "")?;
        let mem = $ctx.builder.build_int_add(book.mem, casted, "")?;
        let dest_ptr = $ctx.builder.build_int_to_ptr(
            mem,
            $ctx.types.type_stackel.ptr_type(AddressSpace::default()),
            "",
        )?;

        $ctx.builder
            .build_store(dest_ptr, shuffled)?
            .set_alignment(1)?;

        book
    }};
}

macro_rules! build_mstore8 {
    ($ctx:ident, $book:ident) => {{
        let (book, offset) = build_stack_pop!($ctx, $book);
        let (book, value) = build_stack_pop!($ctx, book);
        let value_casted = $ctx.builder.build_int_cast(value, $ctx.types.type_i8, "")?;
        let offset_casted = $ctx
            .builder
            .build_int_cast(offset, $ctx.types.type_ptrint, "")?;

        let mem = $ctx.builder.build_int_add(book.mem, offset_casted, "")?;
        let dest_ptr = $ctx.builder.build_int_to_ptr(
            mem,
            $ctx.types.type_i8.ptr_type(AddressSpace::default()),
            "",
        )?;

        $ctx.builder.build_store(dest_ptr, value_casted)?;

        book
    }};
}

macro_rules! build_mload {
    ($ctx:ident, $book:ident) => {{
        let (book, offset) = build_stack_pop!($ctx, $book);
        let casted = $ctx
            .builder
            .build_int_cast(offset, $ctx.types.type_ptrint, "")?;

        let mem0 = $ctx.builder.build_int_add(book.mem, casted, "")?;
        let mem1_offset = $ctx
            .types
            .type_ptrint
            .const_int(EVM_JIT_STACK_ALIGN as u64, false);
        let mem1 = $ctx.builder.build_int_add(mem0, mem1_offset, "")?;

        let mem0_ptr = $ctx.builder.build_int_to_ptr(
            mem0,
            $ctx.types.type_ivec.ptr_type(AddressSpace::default()),
            "",
        )?;
        let mem1_ptr = $ctx.builder.build_int_to_ptr(
            mem1,
            $ctx.types.type_ivec.ptr_type(AddressSpace::default()),
            "",
        )?;

        let vec0 = $ctx.builder.build_load(mem0_ptr, "")?.into_vector_value();
        let vec1 = $ctx.builder.build_load(mem1_ptr, "")?.into_vector_value();

        vec0.as_instruction_value()
            .ok_or(JitEvmEngineError::NoInstructionValue)?
            .set_alignment(1)?;
        vec1.as_instruction_value()
            .ok_or(JitEvmEngineError::NoInstructionValue)?
            .set_alignment(1)?;

        let shuffled = $ctx
            .builder
            .build_shuffle_vector(vec0, vec1, $ctx.types.swap_bytes, "")?;

        build_stack_push_vector!($ctx, book, shuffled)
    }};
}
