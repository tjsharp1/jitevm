use crate::code::EvmOp;
use alloy_primitives::U256;
use revm_primitives::{Spec, SpecId};

const ZERO_DATA_COST: u64 = 4;
// TODO: changes depending on EIP-2028
const NONZERO_DATA_COST: u64 = 16;
const INIT_TX_COST: u64 = 21000;

macro_rules! build_sstore_gas_check {
    ($ctx:ident, $current:ident, $gas_cost:ident, $refund:ident) => {
        use crate::jit::{
            context::{JitContractExecutionResult, JitContractResultCode},
            contract::JitEvmEngineSimpleBlock,
        };
        use inkwell::{intrinsics::Intrinsic, IntPredicate};

        let expect = Intrinsic::find("llvm.expect").expect("expect intrinsic not found!");
        let expect_fn = expect
            .get_declaration(
                &$ctx.module,
                &[$ctx.types.type_bool.into(), $ctx.types.type_bool.into()],
            )
            .expect("Expect intrinsic declaration not found!");
        let const_true = $ctx.types.type_bool.const_int(1, false);
        let book = $current.book_ref();

        let gas_cost = $ctx
            .builder
            .build_int_cast($gas_cost, $ctx.types.type_i64, "")?;
        // TODO: this OR gas_remaining < 2300 (istanbul fork)
        let cmp =
            $ctx.builder
                .build_int_compare(IntPredicate::UGE, book.gas_remaining, gas_cost, "")?;
        let cmp = $ctx
            .builder
            .build_call(expect_fn, &[cmp.into(), const_true.into()], "")?
            .try_as_basic_value()
            .left()
            .ok_or(JitEvmEngineError::NoInstructionValue)?
            .into_int_value();

        let sub_gas = $ctx
            .builder
            .build_select(cmp, gas_cost, book.gas_remaining, "")?
            .into_int_value();
        let remaining = $ctx
            .builder
            .build_int_sub(book.gas_remaining, sub_gas, "deduct_gas")?;

        let refund = $ctx
            .builder
            .build_int_cast($refund, $ctx.types.type_i64, "")?;
        let refund = $ctx
            .builder
            .build_int_add(book.gas_refund, refund, "add_refund_amount")?;

        $current.book_ref_mut().update_refund(refund);
        $current.book_ref_mut().update_gas(remaining);

        let instruction_label = format!("i{}_enough_gas", $current.idx());
        let idx = format!("_{}", $current.idx());
        let next_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        let instruction_label = format!("i{}_error", $current.idx());
        let idx = format!("_{}", $current.idx());
        let error_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        next_block.add_incoming($current.book_ref(), $current.block());
        error_block.add_incoming($current.book_ref(), $current.block());

        $ctx.builder.position_at_end(error_block.block);
        let code = u32::from(JitContractResultCode::OutOfGasBasicOutOfGas);
        let code = $ctx.types.type_i64.const_int(code as u64, false);
        JitContractExecutionResult::build_exit_halt($ctx, &error_block.book(), code)?;

        $ctx.builder.position_at_end($current.block().block);
        $ctx.builder
            .build_conditional_branch(cmp, next_block.block, error_block.block)?;
        $ctx.builder.position_at_end(next_block.block);

        $current.insert_block(next_block.book(), next_block);
    };
}

macro_rules! build_sload_gas_check {
    ($ctx:ident, $current:ident, $gas_cost:ident) => {
        use crate::jit::{
            context::{JitContractExecutionResult, JitContractResultCode},
            contract::JitEvmEngineSimpleBlock,
        };
        use inkwell::{intrinsics::Intrinsic, IntPredicate};

        let expect = Intrinsic::find("llvm.expect").expect("expect intrinsic not found!");
        let expect_fn = expect
            .get_declaration(
                &$ctx.module,
                &[$ctx.types.type_bool.into(), $ctx.types.type_bool.into()],
            )
            .expect("Expect intrinsic declaration not found!");
        let const_true = $ctx.types.type_bool.const_int(1, false);

        let book = $current.book_ref();
        let cmp =
            $ctx.builder
                .build_int_compare(IntPredicate::UGE, book.gas_remaining, $gas_cost, "")?;
        let cmp = $ctx
            .builder
            .build_call(expect_fn, &[cmp.into(), const_true.into()], "")?
            .try_as_basic_value()
            .left()
            .ok_or(JitEvmEngineError::NoInstructionValue)?
            .into_int_value();

        let sub_gas = $ctx
            .builder
            .build_select(cmp, $gas_cost, book.gas_remaining, "")?
            .into_int_value();
        let remaining = $ctx
            .builder
            .build_int_sub(book.gas_remaining, sub_gas, "deduct_gas")?;
        $current.book_ref_mut().update_gas(remaining);

        let instruction_label = format!("i{}_enough_gas", $current.idx());
        let idx = format!("_{}", $current.idx());
        let next_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        let instruction_label = format!("i{}_error", $current.idx());
        let idx = format!("_{}", $current.idx());
        let error_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        next_block.add_incoming($current.book_ref(), $current.block());
        error_block.add_incoming($current.book_ref(), $current.block());

        $ctx.builder.position_at_end(error_block.block);
        let code = u32::from(JitContractResultCode::OutOfGasBasicOutOfGas);
        let code = $ctx.types.type_i64.const_int(code as u64, false);
        JitContractExecutionResult::build_exit_halt($ctx, &error_block.book(), code)?;

        $ctx.builder.position_at_end($current.block().block);
        $ctx.builder
            .build_conditional_branch(cmp, next_block.block, error_block.block)?;
        $ctx.builder.position_at_end(next_block.block);

        $current.insert_block(next_block.book(), next_block);
    };
}

macro_rules! build_sha3_gas_check {
    ($ctx:ident, $current:ident, $offset:ident, $len:ident) => {
        use crate::jit::{
            context::{JitContractExecutionResult, JitContractResultCode},
            contract::JitEvmEngineSimpleBlock,
        };
        use inkwell::{intrinsics::Intrinsic, IntPredicate};

        let expansion_cost = memory_expansion_cost!($ctx, $current, $offset, $len);

        let book = $current.book_ref();

        let expect = Intrinsic::find("llvm.expect").expect("expect intrinsic not found!");
        let expect_fn = expect
            .get_declaration(
                &$ctx.module,
                &[$ctx.types.type_bool.into(), $ctx.types.type_bool.into()],
            )
            .expect("Expect intrinsic declaration not found!");

        let const_true = $ctx.types.type_bool.const_int(1, false);
        let const_0 = $ctx.types.type_i64.const_int(0, false);
        let const_5 = $ctx.types.type_i64.const_int(5, false);
        let const_31 = $ctx.types.type_i64.const_int(31, false);

        let len = $ctx.builder.build_int_cast($len, $ctx.types.type_i64, "")?;
        let i1 = $ctx.builder.build_int_add(len, const_31, "")?;
        let word_size = $ctx.builder.build_right_shift(i1, const_5, false, "")?;

        let (_, dynamic_gas) = sha3_gas::<SPEC>();

        let dynamic_gas = $ctx.types.type_i64.const_int(dynamic_gas, false);

        let d1 = $ctx.builder.build_int_mul(dynamic_gas, word_size, "")?;
        let dynamic = $ctx.builder.build_int_add(d1, expansion_cost, "")?;

        let iszero =
            $ctx.builder
                .build_int_compare(IntPredicate::EQ, len, const_0, "sha3_iszero")?;
        let gas_cost = $ctx
            .builder
            .build_select(iszero, const_0, dynamic, "dynamic_sha3")?
            .into_int_value();

        let cmp =
            $ctx.builder
                .build_int_compare(IntPredicate::UGE, book.gas_remaining, gas_cost, "")?;
        let cmp = $ctx
            .builder
            .build_call(expect_fn, &[cmp.into(), const_true.into()], "")?
            .try_as_basic_value()
            .left()
            .ok_or(JitEvmEngineError::NoInstructionValue)?
            .into_int_value();

        let sub_gas = $ctx
            .builder
            .build_select(cmp, gas_cost, book.gas_remaining, "")?
            .into_int_value();
        let remaining = $ctx
            .builder
            .build_int_sub(book.gas_remaining, sub_gas, "deduct_gas")?;
        $current.book_ref_mut().update_gas(remaining);

        let instruction_label = format!("i{}_enough_gas", $current.idx());
        let idx = format!("_{}", $current.idx());
        let next_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        let instruction_label = format!("i{}_error", $current.idx());
        let idx = format!("_{}", $current.idx());
        let error_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        next_block.add_incoming($current.book_ref(), $current.block());
        error_block.add_incoming($current.book_ref(), $current.block());

        $ctx.builder.position_at_end(error_block.block);
        let code = u32::from(JitContractResultCode::OutOfGasMemory);
        let code = $ctx.types.type_i64.const_int(code as u64, false);
        JitContractExecutionResult::build_exit_halt($ctx, &error_block.book(), code)?;

        $ctx.builder.position_at_end($current.block().block);
        $ctx.builder
            .build_conditional_branch(cmp, next_block.block, error_block.block)?;
        $ctx.builder.position_at_end(next_block.block);

        $current.insert_block(next_block.book(), next_block);
    };
}

macro_rules! memory_expansion_cost {
    ($ctx:ident, $current:ident, $offset:ident, $len:ident) => {{
        let mem_gas = memory_gas::<SPEC>();

        let const_0 = $ctx.types.type_i64.const_int(0, false);
        let const_1 = $ctx.types.type_i64.const_int(1, false);
        let const_5 = $ctx.types.type_i64.const_int(5, false);
        let const_31 = $ctx.types.type_i64.const_int(31, false);
        let const_512 = $ctx.types.type_i64.const_int(512, false);
        let const_mem_gas = $ctx.types.type_i64.const_int(mem_gas, false);

        let offset = $ctx
            .builder
            .build_int_cast($offset, $ctx.types.type_i64, "")?;
        let len = $ctx.builder.build_int_cast($len, $ctx.types.type_i64, "")?;
        let i0 = $ctx.builder.build_int_add(offset, len, "")?;
        let i1 = $ctx.builder.build_and(i0, const_31, "")?;
        let i2 = $ctx.builder.build_not(i1, "")?;
        let i3 = $ctx.builder.build_int_add(i2, const_1, "")?;
        let i4 = $ctx.builder.build_and(i3, const_31, "")?;
        let new_size = $ctx.builder.build_int_add(i0, i4, "")?;

        let book = $current.book_ref();
        let iszero =
            $ctx.builder
                .build_int_compare(IntPredicate::EQ, len, const_0, "len_is_zero")?;
        let new_size = $ctx
            .builder
            .build_select(iszero, book.mem_size, new_size, "new_size")?
            .into_int_value();

        let has_grown =
            $ctx.builder
                .build_int_compare(IntPredicate::UGT, new_size, book.mem_size, "")?;

        let size_word = $ctx
            .builder
            .build_right_shift(new_size, const_5, false, "")?;
        let size_sq = $ctx.builder.build_int_mul(size_word, size_word, "")?;
        let size_0 = $ctx
            .builder
            .build_int_unsigned_div(size_sq, const_512, "")?;
        let size_1 = $ctx.builder.build_int_mul(const_mem_gas, size_word, "")?;
        let mem_gas = $ctx.builder.build_int_add(size_0, size_1, "")?;

        let gas_diff = $ctx.builder.build_int_sub(mem_gas, book.mem_gas, "")?;

        let expansion_cost = $ctx
            .builder
            .build_select(has_grown, gas_diff, const_0, "")?
            .into_int_value();
        let new_size = $ctx
            .builder
            .build_select(has_grown, new_size, book.mem_size, "")?
            .into_int_value();
        let new_gas = $ctx
            .builder
            .build_select(has_grown, mem_gas, book.mem_gas, "")?
            .into_int_value();

        $current.book_ref_mut().update_mem_gas(new_gas, new_size);

        expansion_cost
    }};
}

macro_rules! build_memory_gas_check {
    ($ctx:ident, $current:ident, $offset:ident, $len:literal) => {
        let len = $ctx.types.type_i64.const_int($len, false);
        build_memory_gas_check!($ctx, $current, $offset, len);
    };
    ($ctx:ident, $current:ident, $offset:ident, $len:ident) => {
        use crate::jit::{
            context::{JitContractExecutionResult, JitContractResultCode},
            contract::JitEvmEngineSimpleBlock,
        };
        use inkwell::{intrinsics::Intrinsic, IntPredicate};

        let const_true = $ctx.types.type_bool.const_int(1, false);

        let expect = Intrinsic::find("llvm.expect").expect("expect intrinsic not found!");
        let expect_fn = expect
            .get_declaration(
                &$ctx.module,
                &[$ctx.types.type_bool.into(), $ctx.types.type_bool.into()],
            )
            .expect("Expect intrinsic declaration not found!");

        let gas_cost = memory_expansion_cost!($ctx, $current, $offset, $len);

        let book = $current.book_ref();
        let cmp =
            $ctx.builder
                .build_int_compare(IntPredicate::UGE, book.gas_remaining, gas_cost, "")?;
        let cmp = $ctx
            .builder
            .build_call(expect_fn, &[cmp.into(), const_true.into()], "")?
            .try_as_basic_value()
            .left()
            .ok_or(JitEvmEngineError::NoInstructionValue)?
            .into_int_value();

        let sub_gas = $ctx
            .builder
            .build_select(cmp, gas_cost, book.gas_remaining, "")?
            .into_int_value();
        let remaining = $ctx
            .builder
            .build_int_sub(book.gas_remaining, sub_gas, "deduct_gas")?;
        $current.book_ref_mut().update_gas(remaining);

        let instruction_label = format!("i{}_enough_gas", $current.idx());
        let idx = format!("_{}", $current.idx());
        let next_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        let instruction_label = format!("i{}_error", $current.idx());
        let idx = format!("_{}", $current.idx());
        let error_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        next_block.add_incoming($current.book_ref(), $current.block());
        error_block.add_incoming($current.book_ref(), $current.block());

        $ctx.builder.position_at_end(error_block.block);
        let code = u32::from(JitContractResultCode::OutOfGasMemory);
        let code = $ctx.types.type_i64.const_int(code as u64, false);
        JitContractExecutionResult::build_exit_halt($ctx, &error_block.book(), code)?;

        $ctx.builder.position_at_end($current.block().block);
        $ctx.builder
            .build_conditional_branch(cmp, next_block.block, error_block.block)?;
        $ctx.builder.position_at_end(next_block.block);

        $current.insert_block(next_block.book(), next_block);
    };
}

macro_rules! build_gas_check_exp {
    ($ctx:ident, $current:ident, $exp:ident) => {{
        use crate::jit::{
            context::{JitContractExecutionResult, JitContractResultCode},
            contract::JitEvmEngineSimpleBlock,
        };
        use inkwell::{intrinsics::Intrinsic, IntPredicate};

        let expect = Intrinsic::find("llvm.expect").expect("expect intrinsic not found!");
        let expect_fn = expect
            .get_declaration(
                &$ctx.module,
                &[$ctx.types.type_bool.into(), $ctx.types.type_bool.into()],
            )
            .expect("Expect intrinsic declaration not found!");
        let const_true = $ctx.types.type_bool.const_int(1, false);

        let book = $current.book_ref();

        let (_, byte_gas) = exp_cost::<SPEC>();
        let byte_gas_cost = $ctx.types.type_i64.const_int(byte_gas, false);

        let gas_cost = $ctx
            .builder
            .build_int_mul(byte_gas_cost, $exp, "exp_byte_cost")?;

        let cmp =
            $ctx.builder
                .build_int_compare(IntPredicate::UGE, book.gas_remaining, gas_cost, "")?;
        let cmp = $ctx
            .builder
            .build_call(expect_fn, &[cmp.into(), const_true.into()], "")?
            .try_as_basic_value()
            .left()
            .ok_or(JitEvmEngineError::NoInstructionValue)?
            .into_int_value();

        let sub_gas = $ctx
            .builder
            .build_select(cmp, gas_cost, book.gas_remaining, "")?
            .into_int_value();

        let remaining = $ctx
            .builder
            .build_int_sub(book.gas_remaining, sub_gas, "deduct_gas")?;
        $current.book_ref_mut().update_gas(remaining);

        let block_name = $current
            .block()
            .block
            .get_name()
            .to_str()
            .expect("Block has a name");

        let instruction_label = format!("{}_{}_enough_gas", block_name, $current.idx());
        let idx = format!("_{}", $current.idx());
        let next_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        let instruction_label = format!("{}_{}_error", block_name, $current.idx());
        let idx = format!("_{}", $current.idx());
        let error_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        next_block.add_incoming($current.book_ref(), $current.block());
        error_block.add_incoming($current.book_ref(), $current.block());

        $ctx.builder.position_at_end(error_block.block);
        let code = u32::from(JitContractResultCode::OutOfGasBasicOutOfGas);
        let code = $ctx.types.type_i64.const_int(code as u64, false);
        JitContractExecutionResult::build_exit_halt($ctx, &error_block.book(), code)?;

        $ctx.builder.position_at_end($current.block().block);
        $ctx.builder
            .build_conditional_branch(cmp, next_block.block, error_block.block)?;
        $ctx.builder.position_at_end(next_block.block);

        $current.insert_block(next_block.book(), next_block);
    }};
}

pub fn init_gas<SPEC: Spec>(calldata: &[u8]) -> u64 {
    if SPEC::enabled(SpecId::LATEST) {
        let cost = if calldata.is_empty() {
            0
        } else {
            calldata.iter().fold(0, |a, b| {
                if *b == 0 {
                    a + ZERO_DATA_COST
                } else {
                    a + NONZERO_DATA_COST
                }
            })
        };

        INIT_TX_COST + cost
    } else {
        unimplemented!("Only LATEST implemented currently!");
    }
}

pub fn sload_gas<SPEC: Spec>(warm: bool) -> u64 {
    if SPEC::enabled(SpecId::LATEST) {
        if warm {
            100
        } else {
            2100
        }
    } else {
        unimplemented!("Only LATEST implemented currently!");
    }
}

pub fn sstore_gas<SPEC: Spec>(original: U256, current: U256, new: U256, warm: bool) -> (u64, i64) {
    let cost = sstore_cost::<SPEC>(original, current, new, warm);
    let refund = sstore_refund::<SPEC>(original, current, new, warm);
    (cost, refund)
}

fn sstore_cost<SPEC: Spec>(original: U256, current: U256, new: U256, warm: bool) -> u64 {
    if SPEC::enabled(SpecId::LATEST) {
        let warm_cold = if warm { 0 } else { 2100 };

        if new == current {
            100 + warm_cold
        } else if current == original {
            let gas = if original == U256::ZERO { 20000 } else { 2900 };
            gas + warm_cold
        } else {
            100 + warm_cold
        }
    } else {
        unimplemented!("Only LATEST implemented currently!");
    }
}

fn sstore_refund<SPEC: Spec>(original: U256, current: U256, new: U256, warm: bool) -> i64 {
    if SPEC::enabled(SpecId::LATEST) {
        #[allow(clippy::collapsible_else_if)]
        if current == new {
            0
        } else {
            if current == original {
                if original != U256::ZERO && new == U256::ZERO {
                    4800
                } else {
                    0
                }
            } else {
                let mut refund = 0i64;

                if original != U256::ZERO {
                    if current == U256::ZERO {
                        refund -= 4800;
                    } else if new == U256::ZERO {
                        refund += 4800;
                    }
                }

                if new == original {
                    if original == U256::ZERO {
                        refund += 19900;
                    } else {
                        refund += if warm { 2800 } else { 4900 };
                    }
                }

                refund
            }
        }
    } else {
        unimplemented!("Only LATEST implemented currently!");
    }
}

pub fn memory_gas<SPEC: Spec>() -> u64 {
    if SPEC::enabled(SpecId::LATEST) {
        3
    } else {
        unimplemented!("Only LATEST implemented currently!");
    }
}

pub fn sha3_gas<SPEC: Spec>() -> (u64, u64) {
    if SPEC::enabled(SpecId::LATEST) {
        (30, 6)
    } else {
        unimplemented!("Only LATEST implemented currently!");
    }
}

pub fn exp_cost<SPEC: Spec>() -> (u64, u64) {
    if SPEC::enabled(SpecId::LATEST) {
        (10, 50)
    } else {
        unimplemented!("Only LATEST implemented currently!");
    }
}

pub fn const_cost<SPEC: Spec>(op: EvmOp) -> u64 {
    if !SPEC::enabled(SpecId::LATEST) {
        unimplemented!("Only LATEST implemented currently!");
    }

    match op {
        EvmOp::Exp => 10,
        EvmOp::Iszero => 3,
        EvmOp::Byte => 3,
        EvmOp::Add => 3,
        EvmOp::Mul => 5,
        EvmOp::Sub => 3,
        EvmOp::Div => 5,
        EvmOp::Sdiv => 5,
        EvmOp::Mod => 5,
        EvmOp::Smod => 5,
        EvmOp::Shl => 3,
        EvmOp::Shr => 3,
        EvmOp::Sar => 3,
        EvmOp::And => 3,
        EvmOp::Or => 3,
        EvmOp::Xor => 3,
        EvmOp::Eq => 3,
        EvmOp::Lt => 3,
        EvmOp::Gt => 3,
        EvmOp::Slt => 3,
        EvmOp::Sgt => 3,
        EvmOp::Not => 3,
        EvmOp::Signextend => 5,
        EvmOp::Addmod => 8,
        EvmOp::Mulmod => 8,
        EvmOp::Push(len, _) => {
            if len == 0 {
                2
            } else {
                3
            }
        }
        EvmOp::Pop => 2,
        EvmOp::Dup1 => 3,
        EvmOp::Dup2 => 3,
        EvmOp::Dup3 => 3,
        EvmOp::Dup4 => 3,
        EvmOp::Dup5 => 3,
        EvmOp::Dup6 => 3,
        EvmOp::Dup7 => 3,
        EvmOp::Dup8 => 3,
        EvmOp::Dup9 => 3,
        EvmOp::Dup10 => 3,
        EvmOp::Dup11 => 3,
        EvmOp::Dup12 => 3,
        EvmOp::Dup13 => 3,
        EvmOp::Dup14 => 3,
        EvmOp::Dup15 => 3,
        EvmOp::Dup16 => 3,
        EvmOp::Swap1 => 3,
        EvmOp::Swap2 => 3,
        EvmOp::Swap3 => 3,
        EvmOp::Swap4 => 3,
        EvmOp::Swap5 => 3,
        EvmOp::Swap6 => 3,
        EvmOp::Swap7 => 3,
        EvmOp::Swap8 => 3,
        EvmOp::Swap9 => 3,
        EvmOp::Swap10 => 3,
        EvmOp::Swap11 => 3,
        EvmOp::Swap12 => 3,
        EvmOp::Swap13 => 3,
        EvmOp::Swap14 => 3,
        EvmOp::Swap15 => 3,
        EvmOp::Swap16 => 3,
        EvmOp::Jumpdest => 1,
        EvmOp::Jumpi => 10,
        EvmOp::Jump => 8,
        EvmOp::Mload => 3,
        EvmOp::Mstore => 3,
        EvmOp::Mstore8 => 3,
        EvmOp::Number => 2,
        EvmOp::Coinbase => 2,
        EvmOp::Timestamp => 2,
        EvmOp::PrevRandao => 2,
        EvmOp::BaseFee => 2,
        EvmOp::GasLimit => 2,
        EvmOp::Callvalue => 2,
        EvmOp::Codesize => 2,
        EvmOp::Caller => 2,
        EvmOp::Origin => 2,
        EvmOp::Return => 0,
        EvmOp::Revert => 0,
        EvmOp::Calldataload => 3,
        EvmOp::Calldatasize => 2,
        EvmOp::AugmentedPushJumpi(_, _) => 13,
        EvmOp::AugmentedPushJump(_, _) => 11,
        EvmOp::Stop => 0,
        EvmOp::Sstore => 0,
        EvmOp::Sload => 0,
        EvmOp::Sha3 => 30,
        EvmOp::Invalid => 0,
        _ => unimplemented!("Gas cost for {:?} unimplemented!", op),
    }
}

pub(crate) use build_gas_check_exp;
pub(crate) use build_memory_gas_check;
pub(crate) use build_sha3_gas_check;
pub(crate) use build_sload_gas_check;
pub(crate) use build_sstore_gas_check;
pub(crate) use memory_expansion_cost;
