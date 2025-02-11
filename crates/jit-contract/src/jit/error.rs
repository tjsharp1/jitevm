use inkwell::{builder::BuilderError, execution_engine::FunctionLookupError, support::LLVMString};
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum JitEvmEngineError {
    #[error("FunctionLookupError: {0:?}")]
    FunctionLookupError(#[from] FunctionLookupError),
    #[error("LlvmStringError: {0:?}")]
    UnknownLlvmStringError(#[from] LLVMString),
    #[error("LlvmBuilderError: {0:?}")]
    LlvmBuilderError(#[from] BuilderError),
    #[error("NoInstructionValue")]
    NoInstructionValue,
    #[error("NoInstructionsToRender")]
    NoInstructionsToRender,
    #[error("StringError: {0:?}")]
    UnknownStringError(String),
    #[error("Jump at opcode index {0} has no valid jump destinations!")]
    NoValidJumpDestinations(usize),
    #[error("Reth database error {0}!")]
    RethDBError(#[from] reth_interfaces::RethError),
    #[error("EVM opcode error {0}!")]
    EVMCodeError(#[from] crate::code::EvmCodeError),
    #[error("UTF8 error {0}!")]
    Utf8Error(#[from] core::str::Utf8Error),
}

impl From<String> for JitEvmEngineError {
    fn from(e: String) -> Self {
        Self::UnknownStringError(e)
    }
}

impl From<&str> for JitEvmEngineError {
    fn from(e: &str) -> Self {
        Self::UnknownStringError(e.to_string())
    }
}
