use crate::{
    inspectors::CycleCountInspector,
	util::{DeployImage, parse_bytes},
};

use inkwell::context::Context;
use jit_contract::{
    code::{EvmCode, EvmOpParserMode},
    jit::{contract::JitContractBuilder, ExecutionResult as JITExecutionResult, JitEvmExecutionContext, TransactionConfig},
};
use revm::{
	db::InMemoryDB,
	EVM,
};
use revm_primitives::{AccountInfo, Bytecode, Bytes, ExecutionResult as REVMExecutionResult, LatestSpec, ResultAndState, TransactTo, U256};
use std::{fs::File, io::BufReader, path::PathBuf};


/// Compare REVM & JIT output at specified cycle breakpoint.
#[derive(clap::Args, Debug)]
#[command(author, version, about, long_about = None)]
pub struct CycleDiffCmd {
    /// Set inital contract state (as DeployImage)
    #[arg(long)]
    init_state: PathBuf,
    /// Calldata
    #[arg(long, value_parser = parse_bytes)]
    calldata: Bytes,
	/// Break at cycle count
	#[arg(long)]
	cycle_breakpoint: u64,
}

fn init_db_from_image(image: &DeployImage) -> eyre::Result<InMemoryDB> {
    let mut database = InMemoryDB::default();

    let bytecode = Bytecode::new_raw(image.code.clone()).to_checked();

    let one_eth = U256::from(1) * U256::from(10).pow(U256::from(18));
    let mut info = AccountInfo {
	    balance: U256::from(10) * one_eth,
		nonce: 0,
		code_hash: bytecode.hash_slow(),
		code: Some(bytecode),
	};
	database.insert_contract(&mut info);
	database.insert_account_info(image.address, info);
	database.replace_account_storage(image.address, image.storage.clone())?;

	Ok(database)
}

fn run_revm(image: &DeployImage, calldata: Bytes, cycle_breakpoint: u64) -> eyre::Result<REVMExecutionResult> {
	let database = init_db_from_image(image)?;

	let mut evm = EVM::new();
	evm.database(database);
	evm.env.tx.transact_to = TransactTo::Call(image.address);
	evm.env.tx.data = calldata.into();

    let mut inspector = CycleCountInspector::new(cycle_breakpoint);
	let ResultAndState { result, .. } = evm.inspect(&mut inspector)?;

	Ok(result)
}

fn run_jit(image: &DeployImage, calldata: Bytes, cycle_breakpoint: u64) -> eyre::Result<JITExecutionResult> {
	let database = init_db_from_image(image)?;

	let mut cfg = TransactionConfig::default();
	cfg.transact_to = image.address;

	let mut ctx = JitEvmExecutionContext::builder(LatestSpec)
	    // snailtracer needs moar mem!
	    .memory_size(12 * 1024 * 1024)
	    .with_transaction_config(cfg)
		.with_calldata(calldata)
		.build_with_db(&database);

	let evm_code = EvmCode::new_from_bytes(&image.code, EvmOpParserMode::Strict)?;
	let indexed = evm_code.augment().index();

	let context = Context::create();
	let contract = JitContractBuilder::with_context("jit-instructions", &context)
	    .expect("Could not create builder")
		.cycle_breakpoint(cycle_breakpoint)
	    .build(LatestSpec, indexed)
		.expect("Could not build JIT contract");

	let result = match contract.transact(&mut ctx) {
	    Ok(result) => result,
		Err(_) => panic!("JIT transaction failed!"),
	};

	Ok(result)
}

impl CycleDiffCmd {
    pub fn run(self) -> eyre::Result<()> {
	    let file = File::open(self.init_state)?;
		let reader = BufReader::new(file);

	    let image: DeployImage = serde_json::from_reader(reader)?;

        let revm_result = run_revm(&image, self.calldata.clone(), self.cycle_breakpoint)?;
		let jit_result = run_jit(&image, self.calldata.clone(), self.cycle_breakpoint)?;

		println!("REVM {:?}", revm_result);
		println!("JIT {:?}", jit_result);

		Ok(())
	}
}
