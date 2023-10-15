use crate::constants::*;
use crate::{
    code::{EvmOp, IndexedEvmCode},
    jit::context::JitContractExecutionResult,
};

#[cfg(test)]
mod test;

mod context;
mod cursor;
mod error;
mod ffi;
mod types;

mod ops;

use ffi::*;

pub use context::{ExecutionResult, Halt, JitEvmExecutionContext, Success};
pub use error::JitEvmEngineError;

pub mod contract;
