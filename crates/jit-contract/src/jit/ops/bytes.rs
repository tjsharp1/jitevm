macro_rules! build_get_msbyte {
    ($ctx:ident, $val:ident) => {{
        let byte_count = $ctx.types.type_stackel.const_int(0, false);
        let const8 = $ctx.types.type_stackel.const_int(8, false);
        let const1 = $ctx.types.type_stackel.const_int(1, false);
        let const0 = $ctx.types.type_stackel.const_int(0, false);

        let bit_width = ($ctx.types.type_stackel.get_bit_width() / 2) as u64;
        let shift_width = $ctx.types.type_stackel.const_int(bit_width, false);
        let bytes = $ctx
            .builder
            .build_int_unsigned_div(shift_width, const8, "")?;

        let word_shifted = $ctx
            .builder
            .build_right_shift($val, shift_width, false, "")?;
        let is_zero = $ctx
            .builder
            .build_int_compare(IntPredicate::EQ, word_shifted, const0, "")?;

        let to_add = $ctx
            .builder
            .build_select(is_zero, const0, bytes, "")?
            .into_int_value();
        let byte_count = $ctx.builder.build_int_add(byte_count, to_add, "")?;
        let val = $ctx
            .builder
            .build_select(is_zero, $val, word_shifted, "")?
            .into_int_value();
        // 16-byte
        let bit_width = bit_width / 2;
        let shift_width = $ctx.types.type_stackel.const_int(bit_width, false);
        let bytes = $ctx
            .builder
            .build_int_unsigned_div(shift_width, const8, "")?;

        let word_shifted = $ctx
            .builder
            .build_right_shift(val, shift_width, false, "")?;
        let is_zero = $ctx
            .builder
            .build_int_compare(IntPredicate::EQ, word_shifted, const0, "")?;

        let to_add = $ctx
            .builder
            .build_select(is_zero, const0, bytes, "")?
            .into_int_value();
        let byte_count = $ctx.builder.build_int_add(byte_count, to_add, "")?;
        let val = $ctx
            .builder
            .build_select(is_zero, val, word_shifted, "")?
            .into_int_value();
        // 8-byte
        let bit_width = bit_width / 2;
        let shift_width = $ctx.types.type_stackel.const_int(bit_width, false);
        let bytes = $ctx
            .builder
            .build_int_unsigned_div(shift_width, const8, "")?;

        let word_shifted = $ctx
            .builder
            .build_right_shift(val, shift_width, false, "")?;
        let is_zero = $ctx
            .builder
            .build_int_compare(IntPredicate::EQ, word_shifted, const0, "")?;

        let to_add = $ctx
            .builder
            .build_select(is_zero, const0, bytes, "")?
            .into_int_value();
        let byte_count = $ctx.builder.build_int_add(byte_count, to_add, "")?;
        let val = $ctx
            .builder
            .build_select(is_zero, val, word_shifted, "")?
            .into_int_value();
        // 4-byte
        let bit_width = bit_width / 2;
        let shift_width = $ctx.types.type_stackel.const_int(bit_width, false);
        let bytes = $ctx
            .builder
            .build_int_unsigned_div(shift_width, const8, "")?;

        let word_shifted = $ctx
            .builder
            .build_right_shift(val, shift_width, false, "")?;
        let is_zero = $ctx
            .builder
            .build_int_compare(IntPredicate::EQ, word_shifted, const0, "")?;

        let to_add = $ctx
            .builder
            .build_select(is_zero, const0, bytes, "")?
            .into_int_value();
        let byte_count = $ctx.builder.build_int_add(byte_count, to_add, "")?;
        let val = $ctx
            .builder
            .build_select(is_zero, val, word_shifted, "")?
            .into_int_value();
        // 2-byte
        let bit_width = bit_width / 2;
        let shift_width = $ctx.types.type_stackel.const_int(bit_width, false);
        let bytes = $ctx
            .builder
            .build_int_unsigned_div(shift_width, const8, "")?;

        let word_shifted = $ctx
            .builder
            .build_right_shift(val, shift_width, false, "")?;
        let is_zero = $ctx
            .builder
            .build_int_compare(IntPredicate::EQ, word_shifted, const0, "")?;

        let to_add = $ctx
            .builder
            .build_select(is_zero, const0, bytes, "")?
            .into_int_value();
        let byte_count = $ctx.builder.build_int_add(byte_count, to_add, "")?;
        let val = $ctx
            .builder
            .build_select(is_zero, val, word_shifted, "")?
            .into_int_value();
        // 1-byte
        let is_zero = $ctx
            .builder
            .build_int_compare(IntPredicate::EQ, val, const0, "")?;
        let to_add = $ctx
            .builder
            .build_select(is_zero, const0, const1, "")?
            .into_int_value();
        let byte_count = $ctx.builder.build_int_add(byte_count, to_add, "")?;
        $ctx.builder
            .build_int_cast(byte_count, $ctx.types.type_i64, "")?
    }};
}
