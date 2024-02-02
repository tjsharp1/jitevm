//! Traces the amount of time spent executing a basic block of EVM bytecode.
//! Collects frequency counts for basic blocks as well.
use crate::jit::{
    contract::BuilderContext,
    tracing::{TraceData, Tracer},
    EvmBlocks, JitEvmEngineError,
};
use inkwell::{
    execution_engine::ExecutionEngine,
    intrinsics::Intrinsic,
    types::ArrayType,
    values::{FunctionValue, GlobalValue, IntValue, PointerValue},
    IntPredicate,
};

pub struct BlockTimesTracer<'ctx> {
    num_blocks: usize,
    array_type: ArrayType<'ctx>,
    times: GlobalValue<'ctx>,
    counts: GlobalValue<'ctx>,
    last_block: PointerValue<'ctx>,
    last_ts: PointerValue<'ctx>,
    read_cycle_counter: FunctionValue<'ctx>,
}

impl<'ctx> BlockTimesTracer<'ctx> {
    pub fn new(
        ctx: &BuilderContext<'ctx>,
        code: &EvmBlocks,
        engine: &ExecutionEngine<'ctx>,
    ) -> Result<BlockTimesTracer<'ctx>, JitEvmEngineError> {
        let num_blocks = code.blocks.len();
        let array_type = ctx.types.type_i64.array_type(num_blocks as u32);
        let times = ctx.module.add_global(array_type, None, "time_hist");
        times.set_thread_local(true);
        times.set_initializer(&array_type.const_zero());

        let counts = ctx.module.add_global(array_type, None, "counts");
        counts.set_thread_local(true);
        counts.set_initializer(&array_type.const_zero());

        let last_block = ctx
            .module
            .add_global(ctx.types.type_i64, None, "last_block");
        last_block.set_thread_local(true);
        last_block.set_initializer(&ctx.types.type_i64.const_all_ones());
        let last_block = last_block.as_pointer_value();

        let last_ts = ctx.module.add_global(ctx.types.type_i64, None, "last_ts");
        last_ts.set_thread_local(true);
        last_ts.set_initializer(&ctx.types.type_i64.const_zero());
        let last_ts = last_ts.as_pointer_value();

        let intrinsic = Intrinsic::find("llvm.readcyclecounter").unwrap();
        let read_cycle_counter = intrinsic.get_declaration(&ctx.module, &[]).unwrap();

        let t = BlockTimesTracer {
            num_blocks,
            array_type,
            times,
            counts,
            last_block,
            last_ts,
            read_cycle_counter,
        };

        let ty_i64 = ctx.types.type_i64;
        let cb_type = ctx
            .types
            .type_void
            .fn_type(&[ty_i64.into(), ty_i64.into(), ty_i64.into()], false);
        let cb_fn = ctx.module.add_function(t.data_callback(), cb_type, None);
        engine.add_global_mapping(&cb_fn, callback_block_time as usize);

        Ok(t)
    }
}

impl<'ctx> Tracer<'ctx> for BlockTimesTracer<'ctx> {
    fn tracer_name(&self) -> &'static str {
        "block_times"
    }

    fn data_callback(&self) -> &'static str {
        "callback_block_time"
    }

    fn trace_block(
        &self,
        ctx: &BuilderContext<'ctx>,
        val: IntValue<'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let typ = ctx.types.type_i64;

        let current_ts = ctx
            .builder
            .build_call(self.read_cycle_counter, &[], "current_ts")?
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();
        let last_ts = ctx
            .builder
            .build_load(typ, self.last_ts, "last_ts")?
            .into_int_value();
        ctx.builder.build_store(self.last_ts, current_ts)?;

        let time_delta = ctx
            .builder
            .build_int_sub(current_ts, last_ts, "calc_delta")?;

        let last_block = ctx
            .builder
            .build_load(typ, self.last_block, "load_last_block")?
            .into_int_value();
        ctx.builder.build_store(self.last_block, val)?;

        let cmp = ctx.builder.build_int_compare(
            IntPredicate::EQ,
            last_block,
            typ.const_all_ones(),
            "last_block_check",
        )?;

        let hist_index = ctx
            .builder
            .build_select(cmp, typ.const_zero(), last_block, "select_index")?
            .into_int_value();
        let time_delta = ctx
            .builder
            .build_select(cmp, typ.const_zero(), time_delta, "select_delta")?
            .into_int_value();
        let count_inc = ctx
            .builder
            .build_select(
                cmp,
                typ.const_zero(),
                typ.const_int(1, false),
                "select_count",
            )?
            .into_int_value();

        let lat_ptr = self.times.as_pointer_value();
        let ct_ptr = self.counts.as_pointer_value();

        let zero = ctx.types.type_i64.const_int(0, false);
        let (lat_ptr, ct_ptr) = unsafe {
            (
                ctx.builder.build_gep(
                    self.array_type,
                    lat_ptr,
                    &[zero, hist_index],
                    "load_lat_ptr",
                )?,
                ctx.builder.build_gep(
                    self.array_type,
                    ct_ptr,
                    &[zero, hist_index],
                    "load_ct_ptr",
                )?,
            )
        };
        let bin = ctx
            .builder
            .build_load(typ, lat_ptr, "load_bin")?
            .into_int_value();
        let bin = ctx
            .builder
            .build_int_add(bin, time_delta, "accumulate_delta")?;
        ctx.builder.build_store(lat_ptr, bin)?;

        let bin = ctx
            .builder
            .build_load(typ, ct_ptr, "load_bin")?
            .into_int_value();
        let bin = ctx.builder.build_int_add(bin, count_inc, "accumulate_ct")?;
        ctx.builder.build_store(ct_ptr, bin)?;

        Ok(())
    }

    fn save_trace_data(
        &self,
        ctx: &BuilderContext,
        offset: IntValue<'ctx>,
    ) -> Result<(), JitEvmEngineError> {
        let lat_ptr = self.times.as_pointer_value();
        let ct_ptr = self.counts.as_pointer_value();
        let array_size = ctx.types.type_i64.const_int(self.num_blocks as u64, false);

        let cb_fn = ctx.module.get_function(self.data_callback()).unwrap();

        ctx.builder.build_call(
            cb_fn,
            &[
                lat_ptr.into(),
                ct_ptr.into(),
                array_size.into(),
                offset.into(),
            ],
            "save_time_hist",
        )?;

        Ok(())
    }
}

// allocate & copy the data out of TLS
pub extern "C" fn callback_block_time(times: usize, counts: usize, size: usize, offset: usize) {
    unsafe {
        let ts: Vec<u64> = std::slice::from_raw_parts(times as *const _, size).to_vec();
        let cts: Vec<u64> = std::slice::from_raw_parts(counts as *const _, size).to_vec();

        let traces: &mut TraceData = &mut *(offset as *mut _);
        *traces = TraceData::Histogram(ts, cts);
    }
}
