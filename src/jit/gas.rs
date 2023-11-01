use crate::code::EvmOp;
use crate::spec::SpecId;

const ZERO_DATA_COST: u64 = 4;
// TODO: changes depending on EIP-2028
const NONZERO_DATA_COST: u64 = 16;
const INIT_TX_COST: u64 = 21000;

macro_rules! build_sha3_gas_check {
    ($ctx:ident, $current:ident, $book:ident, $offset:ident, $len:ident) => {
        use crate::jit::{context::JitContractResultCode, contract::JitEvmEngineSimpleBlock};
        use inkwell::{intrinsics::Intrinsic, IntPredicate};

        let (book, expansion_cost) = memory_expansion_cost!($ctx, $current, $book, $offset, $len);

        let expect = Intrinsic::find("llvm.expect").expect("expect intrinsic not found!");
        let expect_fn = expect
            .get_declaration(
                &$ctx.module,
                &[$ctx.types.type_bool.into(), $ctx.types.type_bool.into()],
            )
            .expect("Expect intrinsic declaration not found!");

        let const_true = $ctx.types.type_bool.const_int(1, false);
        let const_5 = $ctx.types.type_i64.const_int(5, false);
        let const_31 = $ctx.types.type_i64.const_int(31, false);

        let len = $ctx.builder.build_int_cast($len, $ctx.types.type_i64, "")?;
        let i1 = $ctx.builder.build_int_add(len, const_31, "")?;
        let word_size = $ctx.builder.build_right_shift(i1, const_5, false, "")?;

        let (static_gas, dynamic_gas) = $ctx.gas.sha3_gas();

        let static_gas = $ctx.types.type_i64.const_int(static_gas, false);
        let dynamic_gas = $ctx.types.type_i64.const_int(dynamic_gas, false);

        let d1 = $ctx.builder.build_int_mul(dynamic_gas, word_size, "")?;
        let dynamic = $ctx.builder.build_int_add(d1, expansion_cost, "")?;

        let gas_cost = $ctx.builder.build_int_add(static_gas, dynamic, "")?;

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
        let book = book.update_gas(remaining);

        let instruction_label = format!("{:?}_{}_enough_gas", $current.op(), $current.idx());
        let idx = format!("_{}", $current.idx());
        let next_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        next_block.add_incoming(&book, $current.block());

        let oog = u32::from(JitContractResultCode::OutOfGasMemory);
        let result_code = $ctx.types.type_i32.const_int(oog as u64, false);
        $current.incoming_error(&book, $current.block(), result_code);

        $ctx.builder.position_at_end($current.block().block);
        $ctx.builder
            .build_conditional_branch(cmp, next_block.block, $current.error_block())?;
        $ctx.builder.position_at_end(next_block.block);

        $current.update_current_block(next_block);
    };
}

macro_rules! memory_expansion_cost {
    ($ctx:ident, $current:ident, $book:ident, $offset:ident, $len:ident) => {{
        let mem_gas = $ctx.gas.memory_gas();

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

        let has_grown =
            $ctx.builder
                .build_int_compare(IntPredicate::UGT, new_size, $book.mem_size, "")?;

        let size_word = $ctx
            .builder
            .build_right_shift(new_size, const_5, false, "")?;
        let size_sq = $ctx.builder.build_int_mul(size_word, size_word, "")?;
        let size_0 = $ctx
            .builder
            .build_int_unsigned_div(size_sq, const_512, "")?;
        let size_1 = $ctx.builder.build_int_mul(const_mem_gas, size_word, "")?;
        let mem_gas = $ctx.builder.build_int_add(size_0, size_1, "")?;

        let gas_diff = $ctx.builder.build_int_sub(mem_gas, $book.mem_gas, "")?;

        let expansion_cost = $ctx
            .builder
            .build_select(has_grown, gas_diff, const_0, "")?
            .into_int_value();
        let new_size = $ctx
            .builder
            .build_select(has_grown, new_size, $book.mem_size, "")?
            .into_int_value();
        let new_gas = $ctx
            .builder
            .build_select(has_grown, mem_gas, $book.mem_gas, "")?
            .into_int_value();

        let book = $book.update_mem_gas(new_gas, new_size);

        (book, expansion_cost)
    }};
}

macro_rules! build_memory_gas_check {
    ($ctx:ident, $current:ident, $book:ident, $offset:ident, $len:literal) => {
        let len = $ctx.types.type_i64.const_int($len, false);
        build_memory_gas_check!($ctx, $current, $book, $offset, len);
    };
    ($ctx:ident, $current:ident, $book:ident, $offset:ident, $len:ident) => {
        use crate::jit::{context::JitContractResultCode, contract::JitEvmEngineSimpleBlock};
        use inkwell::{intrinsics::Intrinsic, IntPredicate};

        let const_true = $ctx.types.type_bool.const_int(1, false);

        let expect = Intrinsic::find("llvm.expect").expect("expect intrinsic not found!");
        let expect_fn = expect
            .get_declaration(
                &$ctx.module,
                &[$ctx.types.type_bool.into(), $ctx.types.type_bool.into()],
            )
            .expect("Expect intrinsic declaration not found!");

        let (book, expansion_cost) = memory_expansion_cost!($ctx, $current, $book, $offset, $len);
        let const_cost = $ctx.gas.const_cost($current.op());
        let const_cost = $ctx.types.type_i64.const_int(const_cost, false);
        let gas_cost = $ctx.builder.build_int_add(const_cost, expansion_cost, "")?;

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
        let book = book.update_gas(remaining);

        let instruction_label = format!("{:?}_{}_enough_gas", $current.op(), $current.idx());
        let idx = format!("_{}", $current.idx());
        let next_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        next_block.add_incoming(&book, $current.block());

        let oog = u32::from(JitContractResultCode::OutOfGasMemory);
        let result_code = $ctx.types.type_i32.const_int(oog as u64, false);
        $current.incoming_error(&book, $current.block(), result_code);

        $ctx.builder.position_at_end($current.block().block);
        $ctx.builder
            .build_conditional_branch(cmp, next_block.block, $current.error_block())?;
        $ctx.builder.position_at_end(next_block.block);

        $current.update_current_block(next_block);
    };
}

macro_rules! build_gas_check {
    ($ctx:ident, $current:ident) => {
        use crate::jit::{context::JitContractResultCode, contract::JitEvmEngineSimpleBlock};
        use inkwell::{intrinsics::Intrinsic, IntPredicate};

        let expect = Intrinsic::find("llvm.expect").expect("expect intrinsic not found!");
        let expect_fn = expect
            .get_declaration(
                &$ctx.module,
                &[$ctx.types.type_bool.into(), $ctx.types.type_bool.into()],
            )
            .expect("Expect intrinsic declaration not found!");
        let const_true = $ctx.types.type_bool.const_int(1, false);

        let book = $current.book();

        let gas_cost = $ctx.gas.const_cost($current.op());
        let gas_cost = $ctx.types.type_i64.const_int(gas_cost, false);

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
        let book = book.update_gas(remaining);

        let instruction_label = format!("{:?}_{}_enough_gas", $current.op(), $current.idx());
        let idx = format!("_{}", $current.idx());
        let next_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        next_block.add_incoming(&book, $current.block());

        let oog = u32::from(JitContractResultCode::OutOfGasBasicOutOfGas);
        let result_code = $ctx.types.type_i32.const_int(oog as u64, false);
        $current.incoming_error(&book, $current.block(), result_code);

        $ctx.builder.position_at_end($current.block().block);
        $ctx.builder
            .build_conditional_branch(cmp, next_block.block, $current.error_block())?;
        $ctx.builder.position_at_end(next_block.block);

        $current.update_current_block(next_block);
    };
}

macro_rules! build_gas_check_exp {
    ($ctx:ident, $current:ident) => {
        let const_zero = $ctx.types.type_i64.const_int(0, false);
        build_gas_check_exp!($ctx, $current, const_zero);
    };
    ($ctx:ident, $current:ident, $exp:ident) => {{
        use crate::jit::{context::JitContractResultCode, contract::JitEvmEngineSimpleBlock};
        use inkwell::{intrinsics::Intrinsic, IntPredicate};

        let expect = Intrinsic::find("llvm.expect").expect("expect intrinsic not found!");
        let expect_fn = expect
            .get_declaration(
                &$ctx.module,
                &[$ctx.types.type_bool.into(), $ctx.types.type_bool.into()],
            )
            .expect("Expect intrinsic declaration not found!");
        let const_true = $ctx.types.type_bool.const_int(1, false);

        let book = $current.book();

        let (base_gas, byte_gas) = $ctx.gas.exp_cost();
        let base_gas_cost = $ctx.types.type_i64.const_int(base_gas, false);
        let byte_gas_cost = $ctx.types.type_i64.const_int(byte_gas, false);

        let exp_gas = $ctx
            .builder
            .build_int_mul(byte_gas_cost, $exp, "exp_byte_cost")?;
        let gas_cost = $ctx
            .builder
            .build_int_add(base_gas_cost, exp_gas, "exp_gas")?;

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
        let book = book.update_gas(remaining);

        let instruction_label = format!(
            "{:?}_{}_enough_gas",
            $current
                .block()
                .block
                .get_name()
                .to_str()
                .expect("Block has a name"),
            $current.idx()
        );
        let idx = format!("_{}", $current.idx());
        let next_block =
            JitEvmEngineSimpleBlock::new($ctx, $current.block().block, &instruction_label, &idx)?;

        next_block.add_incoming(&book, $current.block());

        let oog = u32::from(JitContractResultCode::OutOfGasBasicOutOfGas);
        let result_code = $ctx.types.type_i32.const_int(oog as u64, false);
        $current.incoming_error(&book, $current.block(), result_code);

        $ctx.builder.position_at_end($current.block().block);
        $ctx.builder
            .build_conditional_branch(cmp, next_block.block, $current.error_block())?;
        $ctx.builder.position_at_end(next_block.block);

        $current.update_current_block(next_block);
    }};
}

// TODO: only supporting LATEST spec now, but more later
#[derive(Debug, Clone)]
pub struct Gas {
    spec_id: SpecId,
}

impl Gas {
    pub fn new(spec_id: SpecId) -> Self {
        Self { spec_id }
    }

    pub fn set_spec_id(&mut self, spec_id: SpecId) {
        self.spec_id = spec_id
    }

    pub fn init_gas(&self, calldata: &[u8]) -> u64 {
        INIT_TX_COST
    }

    pub fn memory_gas(&self) -> u64 {
        3
    }

    pub fn sha3_gas(&self) -> (u64, u64) {
        (30, 6)
    }

    pub fn exp_cost(&self) -> (u64, u64) {
        (10, 50)
    }

    pub fn is_const_gas(&self, op: EvmOp) -> bool {
        match op {
            EvmOp::Iszero => true,
            EvmOp::Byte => true,
            EvmOp::Add => true,
            EvmOp::Mul => true,
            EvmOp::Sub => true,
            EvmOp::Div => true,
            EvmOp::Sdiv => true,
            EvmOp::Mod => true,
            EvmOp::Smod => true,
            EvmOp::Shl => true,
            EvmOp::Shr => true,
            EvmOp::Sar => true,
            EvmOp::And => true,
            EvmOp::Or => true,
            EvmOp::Xor => true,
            EvmOp::Eq => true,
            EvmOp::Lt => true,
            EvmOp::Gt => true,
            EvmOp::Slt => true,
            EvmOp::Sgt => true,
            EvmOp::Not => true,
            EvmOp::Signextend => true,
            EvmOp::Addmod => true,
            EvmOp::Mulmod => true,
            EvmOp::Push(_, _) => true,
            EvmOp::Pop => true,
            EvmOp::Dup1 => true,
            EvmOp::Dup2 => true,
            EvmOp::Dup3 => true,
            EvmOp::Dup4 => true,
            EvmOp::Dup5 => true,
            EvmOp::Dup6 => true,
            EvmOp::Dup7 => true,
            EvmOp::Dup8 => true,
            EvmOp::Dup9 => true,
            EvmOp::Dup10 => true,
            EvmOp::Dup11 => true,
            EvmOp::Dup12 => true,
            EvmOp::Dup13 => true,
            EvmOp::Dup14 => true,
            EvmOp::Dup15 => true,
            EvmOp::Dup16 => true,
            EvmOp::Swap1 => true,
            EvmOp::Swap2 => true,
            EvmOp::Swap3 => true,
            EvmOp::Swap4 => true,
            EvmOp::Swap5 => true,
            EvmOp::Swap6 => true,
            EvmOp::Swap7 => true,
            EvmOp::Swap8 => true,
            EvmOp::Swap9 => true,
            EvmOp::Swap10 => true,
            EvmOp::Swap11 => true,
            EvmOp::Swap12 => true,
            EvmOp::Swap13 => true,
            EvmOp::Swap14 => true,
            EvmOp::Swap15 => true,
            EvmOp::Swap16 => true,
            EvmOp::Jumpdest => true,
            EvmOp::Jumpi => true,
            EvmOp::Jump => true,
            EvmOp::AugmentedPushJumpi(_, _) => true,
            EvmOp::AugmentedPushJump(_, _) => true,
            _ => false,
        }
    }

    pub fn const_cost(&self, op: EvmOp) -> u64 {
        match op {
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
            EvmOp::Signextend => 3,
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
            EvmOp::AugmentedPushJumpi(_, _) => 13,
            EvmOp::AugmentedPushJump(_, _) => 11,
            _ => unimplemented!("Gas cost for {:?} unimplemented!", op),
        }
    }
}

pub(crate) use build_gas_check;
pub(crate) use build_gas_check_exp;
pub(crate) use build_memory_gas_check;
pub(crate) use build_sha3_gas_check;
pub(crate) use memory_expansion_cost;
