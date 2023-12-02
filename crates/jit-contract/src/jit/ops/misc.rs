use crate::jit::{
    context::{JitContractExecutionResult, JitContractResultCode},
    contract::BuilderContext,
    cursor::Current,
    JitEvmEngineError,
};

pub(crate) fn build_invalid_op<'ctx>(
    ctx: &BuilderContext<'ctx>,
    current: &mut Current<'_, 'ctx>,
) -> Result<(), JitEvmEngineError> {
    let book = current.book_ref();

    let code = u32::from(JitContractResultCode::InvalidFEOpcode);
    let code = ctx.types.type_i64.const_int(code as u64, false);
    JitContractExecutionResult::build_exit_halt(ctx, book, code)?;

    Ok(())
}
