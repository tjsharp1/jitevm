use crate::util::{init_db_from_image, parse_bytes, DeployImage};

use inkwell::context::Context;
use jit_contract::{
    code::EvmOpParserMode,
    jit::{
        contract::JitContractBuilder, ExecutionResult as JITExecutionResult,
        JitEvmExecutionContext, TransactionConfig,
    },
};
use revm::{EVM, InMemoryDB};
use revm_primitives::{Bytes, LatestSpec, ResultAndState, TransactTo};
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
    /// Use REVM instead
    #[arg(long, default_value = "false")]
    use_revm: bool,
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
    println!("EVM took {} msec", start.elapsed().as_millis());

    Ok(result)
}

fn run_jit(image: &DeployImage, calldata: Bytes, context: &Context) -> eyre::Result<JITExecutionResult> {
    let database = init_db_from_image(image)?;

    let mut cfg = TransactionConfig::default();
    cfg.transact_to = image.address;

    let mut ctx = JitEvmExecutionContext::builder(LatestSpec)
        // snailtracer needs moar mem!
        .memory_size(16 * 1024 * 1024)
        .with_transaction_config(cfg)
        .with_calldata(calldata.clone())
        .build_with_db(&database);

    let contract = JitContractBuilder::with_context("jit-instructions", &context)
        .expect("Could not create builder")
        //.debug_ir("tx.ll")
        //.debug_asm("tx.asm")
        .build(LatestSpec, &image.code, EvmOpParserMode::Strict)
        .expect("Could not build JIT contract");

    let start = std::time::Instant::now();
    let result = match contract.transact(&mut ctx) {
        Ok(result) => result,
        Err(_) => panic!("JIT transaction failed!"),
    };
    println!("JIT took {} msec", start.elapsed().as_millis());

    Ok(result)
}

impl ExecuteTxCmd {
    pub fn run(self) -> eyre::Result<()> {
        let file = File::open(self.init_state)?;
        let reader = BufReader::new(file);

        let image: DeployImage = serde_json::from_reader(reader)?;

        if self.use_revm {
            let ResultAndState { result, .. } = run_revm(&image, self.calldata.clone())?;

            println!("REVM {:?}", result);
        } else {
            let context = Context::create();
            let jit_result = run_jit(&image, self.calldata.clone(), &context)?;

            println!("JIT {:?}", jit_result);
        }

        Ok(())
    }
}
