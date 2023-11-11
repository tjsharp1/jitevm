use crate::jit::{
    context::{JitContractExecutionResult, JitContractResultCode},
    contract::BuilderContext,
    cursor::CurrentInstruction,
    JitEvmEngineError,
};

pub(crate) fn build_invalid_op<'a, 'ctx>(
    ctx: &BuilderContext<'ctx>,
    current: &mut CurrentInstruction<'a, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let block = current.block();

    JitContractExecutionResult::build_exit_halt(
        ctx,
        block,
        JitContractResultCode::InvalidFEOpcode,
    )?;

    Ok(())
}
