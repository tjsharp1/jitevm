use crate::jit::{contract::BuilderContext, cursor::CurrentInstruction, error::JitEvmEngineError};
use inkwell::execution_engine::ExecutionEngine;

mod cycle_breakpoint;
use cycle_breakpoint::CycleBreakpoint;
use revm_primitives::U256;

#[derive(Clone, Debug)]
pub enum TraceData {
    EVMState {
        stack: Vec<U256>,
        memory: Vec<u8>,
        ip: usize,
    },
}

pub(crate) trait Tracer<'ctx> {
    fn step(
        &self,
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'_, 'ctx>,
    ) -> Result<(), JitEvmEngineError>;
    // TODO: likely many other fns.
}

#[derive(Default)]
pub struct TracingOptions {
    /// Run for `cycle_breakpoint` instructions, then issue a Stop instruction.
    pub cycle_breakpoint: Option<u64>,
}

#[derive(Default)]
pub(crate) struct Tracers<'ctx> {
    pub tracers: Vec<Box<dyn Tracer<'ctx> + 'ctx>>,
}

impl<'ctx> Tracers<'ctx> {
    pub fn initialize(
        ctx: &BuilderContext<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
        options: TracingOptions,
    ) -> Tracers<'ctx> {
        let mut tracers = Vec::new();

        let TracingOptions { cycle_breakpoint } = options;

        if let Some(cycles) = cycle_breakpoint {
            let t: Box<dyn Tracer> =
                Box::new(CycleBreakpoint::initialize(ctx, execution_engine, cycles));
            tracers.push(t);
        }

        Tracers { tracers }
    }

    pub fn step(
        &self,
        ctx: &BuilderContext<'ctx>,
        current: &mut CurrentInstruction<'_, 'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        for tracer in &self.tracers {
            tracer.step(ctx, current)?;
        }

        Ok(())
    }
}
