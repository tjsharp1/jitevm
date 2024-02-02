use crate::util::{init_db_from_image, parse_bytes, DeployImage};

use inkwell::context::Context;
use jit_contract::{
    code::EvmOpParserMode,
    jit::{
        contract::JitContractBuilder, ExecutionResult as JITExecutionResult,
        JitEvmExecutionContext, TransactionConfig,
        tracing::{TraceData, TracingConfig},
    },
};
use revm::{EVM, InMemoryDB};
use revm_primitives::{Bytes, LatestSpec, ResultAndState, TransactTo};
use std::{
    ffi::CString,
    fs::{File, OpenOptions},
    io::{BufReader, Write},
    path::PathBuf,
    process::{Child, Command},
};


/// Run JIT with contract.
#[derive(clap::Args, Debug)]
#[command(author, version, about, long_about = None)]
pub struct ExecuteTxCmd {
    /// Set inital contract state (as DeployImage)
    #[arg(long)]
    init_state: PathBuf,
    /// Calldata
    #[arg(long, value_parser = parse_bytes)]
    calldata: Bytes,
    /// Use REVM instead
    #[arg(long, default_value = "false")]
    use_revm: bool,
    /// Perf stat events to capture, ignored if --use-revm flag is used.
    #[arg(long)]
    stat: Option<String>,
    /// Capture cumulative block time histogram, ignored if --use-revm flag is used.
    #[arg(long, conflicts_with = "stat")]
    times: bool,
}

fn run_revm(image: &DeployImage, calldata: Bytes) -> eyre::Result<ResultAndState> {
    let database = init_db_from_image(image)?;

    let mut evm: EVM<InMemoryDB> = EVM::new();
    evm.database(database);
    evm.env.tx.transact_to = TransactTo::Call(image.address);
    evm.env.tx.data = calldata.into();

    let start = std::time::Instant::now();
    let result = match evm.transact() {
        Ok(result) => result,
        Err(_) => panic!("EVM transaction failed!"),
    };
    println!("EVM took {} usec", start.elapsed().as_micros());

    Ok(result)
}

fn run_jit(image: &DeployImage, calldata: Bytes, context: &Context, capture: &mut EventCapture, trace_times: bool) -> eyre::Result<(JITExecutionResult, Option<Vec<TraceData>>)> {
    let database = init_db_from_image(image)?;

    let mut cfg = TransactionConfig::default();
    cfg.transact_to = image.address;

    let mut trace_config = TracingConfig::new();
    trace_config.trace_times(trace_times);

    let mut ctx = JitEvmExecutionContext::builder(LatestSpec)
        // snailtracer needs moar mem!
        .memory_size(16 * 1024 * 1024)
        .with_transaction_config(cfg)
        .with_calldata(calldata.clone())
        .build_with_db(&database);

    let contract = JitContractBuilder::with_context("jit-instructions", &context)
        .expect("Could not create builder")
        .tracing_options(trace_config)
        .debug_ir("stacksave.ll")
        .debug_asm("stacksave.s")
        .build(LatestSpec, &image.code, EvmOpParserMode::Strict)
        .expect("Could not build JIT contract");

    let start = std::time::Instant::now();
    capture.start();
    let result = match contract.transact(&mut ctx) {
        Ok(result) => result,
        Err(_) => panic!("JIT transaction failed!"),
    };
    capture.stop();
    println!("JIT took {} usec", start.elapsed().as_micros());

    Ok(result)
}

pub struct EventCapture {
    fifo: Option<File>,
    child: Option<Child>,
}

impl EventCapture {
    fn new_fifo() -> eyre::Result<(libc::c_int, File)> {
        let mut file = std::env::temp_dir();
        file.push("perf-ctrl.fifo");

        let path = file.as_path();
        let file = path.as_os_str().to_str().unwrap();
        let c_str = CString::new(file)?;

        if path.exists() {
            unsafe { libc::unlink(c_str.as_ptr()); }
        }

        unsafe { libc::mkfifo(c_str.as_ptr(), 0o666); }

        let write_fd = unsafe { libc::open(c_str.as_ptr(), libc::O_RDONLY | libc::O_NONBLOCK) };

        Ok((write_fd, OpenOptions::new().write(true).open(file)?))
    }

    pub fn from_args(args: &ExecuteTxCmd) -> eyre::Result<EventCapture> {
        if let Some(events) = &args.stat {
            let (write_fd, fifo) = Self::new_fifo()?;

            let ctrl = format!("fd:{}", write_fd);
            let pid = format!("{}", std::process::id());

            let stat_cmd = ["stat", "-D", "-1", "-e", &events, "--control", &ctrl, "-p", &pid];
            let child = Command::new("perf")
                .args(stat_cmd)
                .spawn()?;

            return Ok(EventCapture { fifo: Some(fifo), child: Some(child) });
        }

        Ok(EventCapture { fifo: None, child: None })
    }

    pub fn start(&mut self) {
        if let Some(ref mut fifo) = &mut self.fifo {
            fifo.write_all(b"enable\n").expect("Failed to enable perf events!");
        }
    }

    pub fn stop(&mut self) {
        if let Some(ref mut fifo) = &mut self.fifo {
            fifo.write_all(b"disable\n").expect("Failed to disable perf events!");
        }
    }
}

impl ExecuteTxCmd {
    pub fn run(self) -> eyre::Result<()> {
        let file = File::open(&self.init_state)?;
        let reader = BufReader::new(file);

        let image: DeployImage = serde_json::from_reader(reader)?;

        if self.use_revm {
            let ResultAndState { result, .. } = run_revm(&image, self.calldata.clone())?;

            println!("REVM {:?}", result);
        } else {
            let context = Context::create();
            let mut capture = EventCapture::from_args(&self)?;
            let jit_result = run_jit(&image, self.calldata.clone(), &context, &mut capture, self.times)?;

            println!("JIT {:?}", jit_result);
        }

        Ok(())
    }
}
