use crate::{
    code::EvmOp,
	jit::{ExecutionResult, Halt, JitEvmEngineError, JitEvmExecutionContext, gas, Success},
	spec::SpecId,
};
use rand::{prelude::SliceRandom, Rng};
use std::ops::{BitAnd, Not};
use super::{expect_halt, expect_success, test_jit};
use primitive_types::U256;


#[test]
fn operations_test_jump() {
    for _ in 0..1000 {
        let mut execution_context = JitEvmExecutionContext::new();
        let jumpdest_offset = 0x20;

        let mut ops = Vec::new();

        ops.push(EvmOp::Push(32, U256::from(jumpdest_offset)));
        ops.push(EvmOp::Mload);
        ops.push(EvmOp::Jump);
        ops.push(EvmOp::Push(32, U256::from(1)));
        ops.push(EvmOp::Push(32, U256::from(0x40)));
        ops.push(EvmOp::Mstore);
        ops.push(EvmOp::Push(32, U256::from(1)));
        ops.push(EvmOp::Push(32, U256::from(0x60)));
        ops.push(EvmOp::Mstore);

        let bytes = ops.iter().fold(0usize, |a, op| a + op.len());
        let jumpdest = U256::from(bytes);

        ops.push(EvmOp::Push(32, U256::from(1)));
        ops.push(EvmOp::Push(32, U256::from(1)));

        let mut slice = &mut execution_context.memory[jumpdest_offset..jumpdest_offset + 0x20];
        jumpdest.to_big_endian(&mut slice);

        let result = test_jit(ops.clone(), &mut execution_context);
        assert_eq!(result, Err(JitEvmEngineError::NoValidJumpDestinations(2)));

        let key0 = U256::from(0x40);
        let value0 = U256::from(9);
        let key1 = U256::from(0x60);
        let value1 = U256::from(90);

		let mut expected_mem = vec![0u8; 64];
        value0.to_big_endian(&mut expected_mem[0x00..0x20]);
        value1.to_big_endian(&mut expected_mem[0x20..0x40]);

        // remove the ops that should've been jumpdest above
        ops.pop();
        ops.pop();
        ops.push(EvmOp::Jumpdest);
        ops.push(EvmOp::Push(32, value0));
        ops.push(EvmOp::Push(32, key0));
        ops.push(EvmOp::Mstore);
        ops.push(EvmOp::Push(32, value1));
        ops.push(EvmOp::Push(32, key1));
        ops.push(EvmOp::Mstore);

        let result = test_jit(ops.clone(), &mut execution_context).expect("Should return OK()");

        let gas = gas::Gas::new(SpecId::LATEST);
		let init_cost = gas.init_gas(&[]);
		let jump_cost = gas.const_cost(EvmOp::Jump);
		let jumpdest_cost = gas.const_cost(EvmOp::Jumpdest);
		let push_cost = gas.const_cost(EvmOp::Push(32, U256::zero()));
		let mload_cost = gas.const_cost(EvmOp::Mload);
		let mstore_cost = gas.const_cost(EvmOp::Mload);

		let mem_cost = gas.memory_gas();

        let offset = 0x80u64;
        let up = offset.bitand(31).not().wrapping_add(1).bitand(31);
        let size = up.checked_add(offset).expect("Overflow on add");
        let size_words = size / 32;

        let mem_gas = (size_words * size_words) / 512 + mem_cost * size_words;
		let expected_gas = init_cost + jump_cost + jumpdest_cost + 5 * push_cost + mload_cost + 2 * mstore_cost + mem_gas;

        expect_success!(test_jump, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { memory, .. } = execution_context;

		assert_eq!(memory[0x40..0x80], expected_mem);
    }
}

#[test]
fn test_jump_underflow() {
    let gas = gas::Gas::new(SpecId::LATEST);
    let init_cost = gas.init_gas(&[]);
    let expected_gas = init_cost + gas.const_cost(EvmOp::Jump);
    let mut execution_context = JitEvmExecutionContext::new();

    let ops = vec![EvmOp::Jump, EvmOp::Jumpdest];
    let result = test_jit(ops, &mut execution_context).expect("Should return OK()");
    expect_halt!(jump_underflow, result, Halt::StackUnderflow, expected_gas);
}

#[test]
fn operations_test_jumpi() {
    for _ in 0..1000 {
        let mut execution_context = JitEvmExecutionContext::new();
        let jumpdest_offset = 0x20;

        let mut ops = Vec::new();

        let zero = U256::zero();
        let val = rand::thread_rng().gen::<[u8; 32]>();
        let nonzero = U256::from_big_endian(&val);
        let choices = [zero, nonzero];
        let condition = choices
            .choose(&mut rand::thread_rng())
            .expect("Should make random choice");
        let key0 = U256::from(0x40);
        let key1 = U256::from(0x60);
        let value0_branch0 = U256::from(1);
        let value1_branch0 = U256::from(8);

        ops.push(EvmOp::Push(32, *condition));
        ops.push(EvmOp::Push(32, U256::from(jumpdest_offset)));
        ops.push(EvmOp::Mload);
        ops.push(EvmOp::Jumpi);
        ops.push(EvmOp::Push(32, value0_branch0));
        ops.push(EvmOp::Push(32, key0));
        ops.push(EvmOp::Mstore);
        ops.push(EvmOp::Push(32, value1_branch0));
        ops.push(EvmOp::Push(32, key1));
        ops.push(EvmOp::Mstore);
        ops.push(EvmOp::Stop);

        let bytes = ops.iter().fold(0usize, |a, op| a + op.len());
        let jumpdest = U256::from(bytes);

        // push a couple non-jumpdest instructions, to check jumpdest error
        ops.push(EvmOp::Push(32, U256::from(1)));
        ops.push(EvmOp::Push(32, U256::from(1)));

        let mut slice = &mut execution_context.memory[jumpdest_offset..jumpdest_offset + 0x20];
        jumpdest.to_big_endian(&mut slice);

        let result = test_jit(ops.clone(), &mut execution_context);
        assert_eq!(result, Err(JitEvmEngineError::NoValidJumpDestinations(3)));

        let value0_branch1 = U256::from(9);
        let value1_branch1 = U256::from(90);

		let mut expected_mem = vec![0u8; 64];

        if *condition == U256::zero() {
            value0_branch0.to_big_endian(&mut expected_mem[..0x20]);
            value1_branch0.to_big_endian(&mut expected_mem[0x20..0x40]);
        } else {
            value0_branch1.to_big_endian(&mut expected_mem[..0x20]);
            value1_branch1.to_big_endian(&mut expected_mem[0x20..0x40]);
        }

        // remove the ops that should've been jumpdest above
        ops.pop();
        ops.pop();
        ops.push(EvmOp::Jumpdest);
        ops.push(EvmOp::Push(32, value0_branch1));
        ops.push(EvmOp::Push(32, key0));
        ops.push(EvmOp::Mstore);
        ops.push(EvmOp::Push(32, value1_branch1));
        ops.push(EvmOp::Push(32, key1));
        ops.push(EvmOp::Mstore);
        ops.push(EvmOp::Stop);

        let result = test_jit(ops.clone(), &mut execution_context).expect("Should return OK()");

        let gas = gas::Gas::new(SpecId::LATEST);
		let init_cost = gas.init_gas(&[]);
		let jumpi_cost = gas.const_cost(EvmOp::Jumpi);
		let jumpdest_cost = gas.const_cost(EvmOp::Jumpdest);
		let push_cost = gas.const_cost(EvmOp::Push(32, U256::zero()));
		let mload_cost = gas.const_cost(EvmOp::Mload);
		let mstore_cost = gas.const_cost(EvmOp::Mload);

		let mem_cost = gas.memory_gas();

        let offset = 0x80u64;
        let up = offset.bitand(31).not().wrapping_add(1).bitand(31);
        let size = up.checked_add(offset).expect("Overflow on add");
        let size_words = size / 32;

        let jumpdest = if *condition == U256::zero() { 0 } else { jumpdest_cost };

        let mem_gas = (size_words * size_words) / 512 + mem_cost * size_words;

		let expected_gas = init_cost + 6 * push_cost + mload_cost + jumpi_cost + 2 * mstore_cost + jumpdest + mem_gas;

        expect_success!(test_jumpi, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { memory, .. } = execution_context;

		assert_eq!(memory[0x40..0x80], expected_mem);
    }
}

#[test]
fn test_jumpi_underflow() {
    let mut execution_context = JitEvmExecutionContext::new();

    let ops = vec![EvmOp::Jumpi, EvmOp::Jumpdest];
    let result = test_jit(ops, &mut execution_context).expect("Should return OK()");

	let gas = gas::Gas::new(SpecId::LATEST);
	let init_cost = gas.init_gas(&[]);
	let jumpi_cost = gas.const_cost(EvmOp::Jumpi);
	let mload_cost = gas.const_cost(EvmOp::Mload);
	let push_cost = gas.const_cost(EvmOp::Push(32, U256::zero()));

	let expected_gas = init_cost + jumpi_cost;
    expect_halt!(jumpi_underflow1, result, Halt::StackUnderflow, expected_gas);

    let mut execution_context = JitEvmExecutionContext::new();

    let ops = vec![
        EvmOp::Push(32, U256::zero()),
        EvmOp::Mload,
        EvmOp::Jumpi,
        EvmOp::Jumpdest,
    ];
    let result = test_jit(ops, &mut execution_context).expect("Should return OK()");

	let mem_cost = gas.memory_gas();

	let offset = 0x20u64;
	let up = offset.bitand(31).not().wrapping_add(1).bitand(31);
	let size = up.checked_add(offset).expect("Overflow on add");
	let size_words = size / 32;

	let mem_gas = (size_words * size_words) / 512 + mem_cost * size_words;
	let expected_gas = init_cost + push_cost + mload_cost + jumpi_cost + mem_gas;

    expect_halt!(jumpi_underflow2, result, Halt::StackUnderflow, expected_gas);
}

#[test]
fn operations_test_augmented_jump() {
    let mut execution_context = JitEvmExecutionContext::new();

    let mut ops = Vec::new();
	let key0 = U256::zero();
	let key1 = U256::from(0x20);

    ops.push(EvmOp::Push(32, U256::zero()));
    ops.push(EvmOp::Jump);
    ops.push(EvmOp::Push(32, U256::from(1)));
    ops.push(EvmOp::Push(32, key0));
    ops.push(EvmOp::Mstore);
    ops.push(EvmOp::Push(32, U256::from(1)));
    ops.push(EvmOp::Push(32, key1));
    ops.push(EvmOp::Mstore);

    let bytes = ops.iter().fold(0usize, |a, op| a + op.len());
    let jumpdest = U256::from(bytes);

    ops[0] = EvmOp::Push(32, jumpdest);

    ops.push(EvmOp::Push(32, U256::from(1)));
    ops.push(EvmOp::Push(32, U256::from(1)));

    let result = test_jit(ops.clone(), &mut execution_context);
    assert_eq!(result, Err(JitEvmEngineError::NoValidJumpDestinations(0)));

    let value0 = U256::from(9);
    let value1 = U256::from(90);

    let mut expected_mem = vec![0u8; 64];
    value0.to_big_endian(&mut expected_mem[..0x20]);
    value1.to_big_endian(&mut expected_mem[0x20..0x40]);

    // remove the ops that should've been jumpdest above
    ops.pop();
    ops.pop();
    ops.push(EvmOp::Jumpdest);
    ops.push(EvmOp::Push(32, value0));
    ops.push(EvmOp::Push(32, key0));
    ops.push(EvmOp::Mstore);
    ops.push(EvmOp::Push(32, value1));
    ops.push(EvmOp::Push(32, key1));
    ops.push(EvmOp::Mstore);

    let result = test_jit(ops.clone(), &mut execution_context).expect("Should return OK()");

	let gas = gas::Gas::new(SpecId::LATEST);
	let init_cost = gas.init_gas(&[]);
	let jump_cost = gas.const_cost(EvmOp::Jump);
	let jumpdest_cost = gas.const_cost(EvmOp::Jumpdest);
	let push_cost = gas.const_cost(EvmOp::Push(32, U256::zero()));
	let mstore_cost = gas.const_cost(EvmOp::Mload);

	let mem_cost = gas.memory_gas();

	let offset = 0x40u64;
	let up = offset.bitand(31).not().wrapping_add(1).bitand(31);
	let size = up.checked_add(offset).expect("Overflow on add");
	let size_words = size / 32;

	let mem_gas = (size_words * size_words) / 512 + mem_cost * size_words;
	let expected_gas = init_cost + jump_cost + push_cost * 5 + 2 * mstore_cost + jumpdest_cost + mem_gas;

    expect_success!(augmented_jump, result, Success::Stop, expected_gas);

    let JitEvmExecutionContext { memory, .. } = execution_context;

	assert_eq!(memory[..0x40], expected_mem);
}

#[test]
fn operations_test_augmented_jumpi() {
    for _ in 0..1000 {
        let mut execution_context = JitEvmExecutionContext::new();

        let mut ops = Vec::new();

        let zero = U256::zero();
        let val = rand::thread_rng().gen::<[u8; 32]>();
        let nonzero = U256::from_big_endian(&val);
        let choices = [zero, nonzero];
        let condition = choices
            .choose(&mut rand::thread_rng())
            .expect("Should make random choice");

        let key0 = U256::from(0);
        let key1 = U256::from(0x20);

        let value0_branch0 = U256::from(1);
        let value1_branch0 = U256::from(8);

        ops.push(EvmOp::Push(32, *condition));
        ops.push(EvmOp::Push(32, U256::zero()));
        ops.push(EvmOp::Jumpi);
        ops.push(EvmOp::Push(32, value0_branch0));
        ops.push(EvmOp::Push(32, key0));
        ops.push(EvmOp::Mstore);
        ops.push(EvmOp::Push(32, value1_branch0));
        ops.push(EvmOp::Push(32, key1));
        ops.push(EvmOp::Mstore);
        ops.push(EvmOp::Stop);

        let bytes = ops.iter().fold(0usize, |a, op| a + op.len());
        let jumpdest = U256::from(bytes);

        ops[1] = EvmOp::Push(32, jumpdest);

        // push a couple non-jumpdest instructions, to check jumpdest error
        ops.push(EvmOp::Push(32, U256::from(1)));
        ops.push(EvmOp::Push(32, U256::from(1)));

        let result = test_jit(ops.clone(), &mut execution_context);
        assert_eq!(result, Err(JitEvmEngineError::NoValidJumpDestinations(1)));

        let value0_branch1 = U256::from(9);
        let value1_branch1 = U256::from(90);

        let mut expected_mem = vec![0u8; 64];

        if *condition == U256::zero() {
            value0_branch0.to_big_endian(&mut expected_mem[..0x20]);
            value1_branch0.to_big_endian(&mut expected_mem[0x20..0x40]);
        } else {
            value0_branch1.to_big_endian(&mut expected_mem[..0x20]);
            value1_branch1.to_big_endian(&mut expected_mem[0x20..0x40]);
        }

        // remove the ops that should've been jumpdest above
        ops.pop();
        ops.pop();
        ops.push(EvmOp::Jumpdest);
        ops.push(EvmOp::Push(32, value0_branch1));
        ops.push(EvmOp::Push(32, key0));
        ops.push(EvmOp::Mstore);
        ops.push(EvmOp::Push(32, value1_branch1));
        ops.push(EvmOp::Push(32, key1));
        ops.push(EvmOp::Mstore);
        ops.push(EvmOp::Stop);

        let result = test_jit(ops.clone(), &mut execution_context).expect("Should be Ok()");

        let gas = gas::Gas::new(SpecId::LATEST);
		let init_cost = gas.init_gas(&[]);
		let jumpi_cost = gas.const_cost(EvmOp::Jumpi);
		let jumpdest_cost = gas.const_cost(EvmOp::Jumpdest);
		let push_cost = gas.const_cost(EvmOp::Push(32, U256::zero()));
		let mstore_cost = gas.const_cost(EvmOp::Mstore);

		let mem_cost = gas.memory_gas();

        let offset = 0x40u64;
        let up = offset.bitand(31).not().wrapping_add(1).bitand(31);
        let size = up.checked_add(offset).expect("Overflow on add");
        let size_words = size / 32;

        let jumpdest = if *condition == U256::zero() { 0 } else { jumpdest_cost };

        let mem_gas = (size_words * size_words) / 512 + mem_cost * size_words;

        let expected_gas = init_cost + 6 * push_cost + jumpi_cost + 2 * mstore_cost + jumpdest + mem_gas;
        expect_success!(augmented_jumpi, result, Success::Stop, expected_gas);

        let JitEvmExecutionContext { memory, .. } = execution_context;

		assert_eq!(memory[..0x40], expected_mem);
    }
}
