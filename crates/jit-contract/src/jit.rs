use crate::constants::*;
use crate::{
    code::{EvmBlock, EvmBlocks, EvmOp},
    jit::context::JitContractExecutionResult,
};

#[cfg(test)]
mod test;

mod context;
mod cursor;
mod error;
mod ffi;
pub mod gas;
pub mod tracing;
mod types;

mod ops;

use ffi::*;

pub use context::{
    BlockConfig, DBBox, ExecutionResult, Halt, JitEvmExecutionContext, Success, TransactionConfig,
};
pub use error::JitEvmEngineError;

pub mod contract;
