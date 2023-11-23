mod common;
use common::*;

use jit_contract::jit::{
    ExecutionResult, Halt, Success,
};
use revm::{
    primitives::{
        Bytes, Eval, ExecutionResult as REVMExecutionResult,
        Halt as REVMHalt, OutOfGasError, Output, ResultAndState,
    },
};

#[test]
fn test_evm_and_jit_snailtracer() {
    let image = include_str!("snailtracer.json");
    let image: DeployImage = serde_json::from_str(image).expect("Failed parsing json");
    let calldata = Bytes::copy_from_slice(&[0x30, 0x62, 0x7b, 0x7c]);

    assert_evm_jit_equivalence!(image, calldata);
}
