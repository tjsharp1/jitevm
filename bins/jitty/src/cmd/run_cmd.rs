use crate::util::{chainspec_parser, load_evm_code};

use alloy_primitives::Address;
use inkwell::context::Context;
use jit_contract::{
    db::StateProviderDatabase,
    jit::{contract::JitContractBuilder, JitEvmExecutionContext, TransactionConfig},
};
use reth::dirs::{DataDirPath, MaybePlatformPath};
use reth_db::open_db_read_only;
use reth_interfaces::db::LogLevel;
use reth_primitives::ChainSpec;
use reth_provider::ProviderFactory;
use revm::db::CacheDB;
use revm_primitives::LatestSpec;
use std::{path::PathBuf, sync::Arc};

/// Simulate a transaction, based off of state from the specified chain.
#[derive(clap::Args, Debug)]
#[command(author, version, about, long_about = None)]
pub struct RunCmd {
    /// Chain state to fork from.
    #[arg(long, default_value = "mainnet", value_parser = chainspec_parser)]
    chain: Arc<ChainSpec>,
    /// DB directory.
    #[arg(long, default_value_t)]
    datadir: MaybePlatformPath<DataDirPath>,
    // TODO: make this an Option<> and use on-chain bytecode by default.
    /// Use this bytecode instead of the on-chain bytecode
    #[arg(long)]
    bytecode: PathBuf,
    /// Contract address to run.
    #[arg(long)]
    address: String,
    /// Calldata
    #[arg(long)]
    calldata: String,
    #[arg(long, value_enum)]
    log_level: Option<LogLevel>,
}

impl RunCmd {
    pub fn run(self) -> eyre::Result<()> {
        let datadir = self.datadir.unwrap_or_chain_default(self.chain.chain);

        let log_level = None;
        let db = open_db_read_only(&datadir.db_path(), log_level)?;

        let factory = ProviderFactory::new(db, self.chain);
        let db = StateProviderDatabase::new(factory.latest()?);
        let cachedb = CacheDB::new(db);

        let address = Address::parse_checksummed(self.address, None)?;
        let mut cfg = TransactionConfig::default();
        cfg.transact_to = address;

        let mut ctx = JitEvmExecutionContext::builder(LatestSpec)
            .with_transaction_config(cfg)
            .build_with_db(&cachedb);

        let evm_code = load_evm_code(self.bytecode)?;

        let context = Context::create();
        let contract = JitContractBuilder::with_context("jit-instructions", &context)
            .expect("Could not build jit contract")
            .debug_ir("jit_test.ll")
            .debug_asm("jit_test.asm")
            .build(LatestSpec, evm_code.augment().index())
            .unwrap();

        match contract.transact(&mut ctx) {
            Ok(result) => {
                println!("RESULT {:?}", result);
                Ok(())
            }
            e => panic!("Error {e:?}"),
        }
    }
}
