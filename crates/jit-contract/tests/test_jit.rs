mod common;

use common::*;

use jit_contract::jit::{ExecutionResult, Halt, Success};
use revm::primitives::{
    Bytes, Eval, ExecutionResult as REVMExecutionResult, Halt as REVMHalt, OutOfGasError, Output,
    ResultAndState,
};

#[test]
fn test_evm_and_jit_fibonacci() {
    let image = DeployImage::new_from_bytecode("fibonacci");
    let calldata = Bytes::new();

    assert_evm_jit_equivalence!(image, calldata);
}

#[test]
fn test_evm_and_jit_fibonacci_repetitions() {
    let image = DeployImage::new_from_bytecode("fibonacci_repetitions");
    let calldata = Bytes::new();

    assert_evm_jit_equivalence!(image, calldata);
}

#[test]
fn test_evm_and_jit_exp() {
    let image = DeployImage::new_from_bytecode("exp");
    let calldata = Bytes::new();

    assert_evm_jit_equivalence!(image, calldata);
}

#[test]
fn test_evm_and_jit_mload() {
    let image = DeployImage::new_from_bytecode("mload");
    let calldata = Bytes::new();

    assert_evm_jit_equivalence!(image, calldata);
}

#[test]
fn test_evm_and_jit_mstore8() {
    let image = DeployImage::new_from_bytecode("mstore8");
    let calldata = Bytes::new();

    assert_evm_jit_equivalence!(image, calldata);
}

#[test]
fn test_evm_and_jit_sha3() {
    let image = DeployImage::new_from_bytecode("sha3");
    let calldata = Bytes::new();

    assert_evm_jit_equivalence!(image, calldata);
}

#[test]
fn test_evm_and_jit_sstore() {
    let image = DeployImage::new_from_bytecode("store");
    let calldata = Bytes::new();

    assert_evm_jit_equivalence!(image, calldata);
}

#[test]
fn test_evm_and_jit_sload() {
    let image = DeployImage::new_from_bytecode("store");
    let calldata = Bytes::new();

    assert_evm_jit_equivalence!(image, calldata);
}

#[test]
fn test_evm_and_jit_reth_random_stuff() {
    let image = DeployImage::new_from_bytecode("reth_random_stuff");
    let calldata = Bytes::new();

    assert_evm_jit_equivalence!(image, calldata);
}
