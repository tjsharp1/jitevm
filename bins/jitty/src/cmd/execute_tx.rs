use crate::util::{init_db_from_image, parse_bytes, DeployImage};

use inkwell::context::Context;
use jit_contract::{
    code::EvmOpParserMode,
    jit::{
        contract::JitContractBuilder, ExecutionResult as JITExecutionResult,
        JitEvmExecutionContext, TransactionConfig,
    },
};
use revm_primitives::{Bytes, LatestSpec};
use std::{fs::File, io::BufReader, path::PathBuf};

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
}

fn run_jit(image: &DeployImage, calldata: Bytes, code: &Bytes) -> eyre::Result<JITExecutionResult> {
    println!("PID: {}", std::process::id());

    let database = init_db_from_image(image)?;

    let mut cfg = TransactionConfig::default();
    cfg.transact_to = image.address;

    let mut ctx = JitEvmExecutionContext::builder(LatestSpec)
        // snailtracer needs moar mem!
        .memory_size(12 * 1024 * 1024)
        .with_transaction_config(cfg)
        .with_calldata(calldata.clone())
        .build_with_db(&database);

    let context = Context::create();
    let contract = JitContractBuilder::with_context("jit-instructions", &context)
        .expect("Could not create builder")
        .debug_ir("dooey.ll")
        .debug_asm("dooey.asm")
        .build(LatestSpec, code, EvmOpParserMode::Strict)
        .expect("Could not build JIT contract");

    let result = match contract.transact(&mut ctx) {
        Ok(result) => result,
        Err(_) => panic!("JIT transaction failed!"),
    };

    Ok(result)
}

impl ExecuteTxCmd {
    pub fn run(self) -> eyre::Result<()> {
        let file = File::open(self.init_state)?;
        let reader = BufReader::new(file);

        let image: DeployImage = serde_json::from_reader(reader)?;

        let jit_result = run_jit(&image, self.calldata.clone(), &image.code)?;

        println!("JIT {:?}", jit_result);

        Ok(())
    }
}
