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
pub mod gas;
mod tracing;
mod types;

mod ops;

use ffi::*;

pub use context::{
    BlockConfig, DBBox, ExecutionResult, Halt, JitEvmExecutionContext, Success, TransactionConfig,
};
pub use error::JitEvmEngineError;
pub use tracing::TracingOptions;

pub mod contract;
