use crate::{
    code::EvmBlocks,
    jit::{contract::BuilderContext, JitEvmEngineError},
};
use inkwell::{execution_engine::ExecutionEngine, values::IntValue};
use itertools::join;

mod block_times;
use block_times::BlockTimesTracer;

pub enum TraceType {
    BlockLatency,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TraceData {
    Empty,
    Histogram(Vec<u64>, Vec<u64>),
}

#[derive(Clone, Copy, Debug, Default)]
pub struct TracingConfig {
    trace_times: bool,
}

impl TracingConfig {
    pub fn new() -> TracingConfig {
        TracingConfig { trace_times: false }
    }

    pub fn trace_times(&mut self, trace: bool) {
        self.trace_times = trace;
    }

    pub fn into_tracer<'ctx>(
        self,
        ctx: &BuilderContext<'ctx>,
        code: &EvmBlocks,
        engine: &ExecutionEngine<'ctx>,
    ) -> Result<Tracers<'ctx>, JitEvmEngineError> {
        Tracers::init(self, ctx, code, engine)
    }
}

pub trait Tracer<'ctx> {
    fn tracer_name(&self) -> &'static str;

    fn data_callback(&self) -> &'static str;

    fn trace_block(
        &self,
        ctx: &BuilderContext<'ctx>,
        val: IntValue<'ctx>,
    ) -> Result<(), JitEvmEngineError>;

    fn save_trace_data(
        &self,
        ctx: &BuilderContext,
        offset: IntValue<'ctx>,
    ) -> Result<(), JitEvmEngineError>;
}

#[derive(Default)]
pub struct Tracers<'ctx> {
    tracers: Vec<Box<dyn Tracer<'ctx> + 'ctx>>,
}

impl<'ctx> std::fmt::Debug for Tracers<'ctx> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let i = self.tracers.iter().map(|t| t.tracer_name().to_string());

        write!(fmt, "Tracers [ {} ]", join(i, ", "))
    }
}

impl<'ctx> Tracers<'ctx> {
    pub fn init(
        config: TracingConfig,
        ctx: &BuilderContext<'ctx>,
        code: &EvmBlocks,
        engine: &ExecutionEngine<'ctx>,
    ) -> Result<Tracers<'ctx>, JitEvmEngineError> {
        let TracingConfig { trace_times } = config;

        let mut tracers = Vec::new();

        if trace_times {
            let t = BlockTimesTracer::new(ctx, code, engine)?;
            let t: Box<dyn Tracer<'ctx>> = Box::new(t);

            tracers.push(t);
        }

        Ok(Tracers { tracers })
    }

    pub fn is_empty(&self) -> bool {
        self.tracers.is_empty()
    }

    pub fn len(&self) -> usize {
        self.tracers.len()
    }

    pub fn trace_block(
        &self,
        ctx: &BuilderContext<'ctx>,
        val: IntValue<'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        for tracer in &self.tracers {
            tracer.trace_block(ctx, val)?;
        }
        Ok(())
    }

    pub fn save_trace_data(
        &self,
        ctx: &BuilderContext<'ctx>,
        ptr: IntValue<'ctx>,
        len: IntValue<'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        if !self.tracers.is_empty() {
            let trace_size = std::mem::size_of::<TraceData>();
            let trace_size = ctx.types.type_ptrint.const_int(trace_size as u64, false);

            for (idx, tracer) in self.tracers.iter().enumerate() {
                let idx = ctx.types.type_ptrint.const_int(idx as u64, false);
                let offset = ctx
                    .builder
                    .build_int_mul(trace_size, idx, "calc_trace_offset")?;
                let slot = ctx
                    .builder
                    .build_int_add(ptr, offset, "index_trace_array")?;

                tracer.save_trace_data(ctx, slot)?;
            }

            ctx.builder.build_return(None)?;
        }

        Ok(())
    }
}
