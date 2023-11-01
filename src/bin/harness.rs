use alloy_primitives::Address;
use clap::Parser;
use eyre::eyre;
use inkwell::context::Context;
use jitevm::{
    code::{EvmCode, EvmOpParserMode},
    db::StateProviderDatabase,
    jit::{
        contract::JitContractBuilder, gas, ExecutionResult, JitEvmEngineError,
        JitEvmExecutionContext, Success,
    },
    spec::SpecId,
};
use reth::dirs::{DataDirPath, MaybePlatformPath};
use reth_db::open_db_read_only;
use reth_interfaces::db::LogLevel;
use reth_primitives::{ChainSpec, DEV, GOERLI, HOLESKY, MAINNET, SEPOLIA};
use reth_provider::ProviderFactory;
use revm::db::CacheDB;
use std::{path::PathBuf, sync::Arc};

fn chainspec_parser(specname: &str) -> eyre::Result<Arc<ChainSpec>> {
    Ok(match specname {
        "mainnet" => MAINNET.clone(),
        "goerli" => GOERLI.clone(),
        "sepolia" => SEPOLIA.clone(),
        "holesky" => HOLESKY.clone(),
        "dev" => DEV.clone(),
        spec => return Err(eyre!("Unknown specname: {spec}")),
    })
}

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[arg(long, default_value = "mainnet", value_parser = chainspec_parser)]
    chain: Arc<ChainSpec>,
    #[arg(long, default_value_t)]
    datadir: MaybePlatformPath<DataDirPath>,
    #[arg(long)]
    bytecode: PathBuf,
    #[arg(long)]
    address: String,
}

fn load_evm_code(path: PathBuf) -> eyre::Result<EvmCode> {
    let code = std::fs::read_to_string(path)?;
    let bytes = hex::decode(code)?;

    Ok(EvmCode::new_from_bytes(&bytes, EvmOpParserMode::Strict)?)
}

fn main() -> eyre::Result<()> {
    let args = Args::parse();

    let datadir = args.datadir.unwrap_or_chain_default(args.chain.chain);

    let log_level = None;
    let db = open_db_read_only(&datadir.db_path(), log_level)?;

    let factory = ProviderFactory::new(db, args.chain);
    let db = StateProviderDatabase::new(factory.latest()?);
    let cachedb = CacheDB::new(db);

    let mut ctx = JitEvmExecutionContext::new_with_db(&cachedb);

    let address = Address::parse_checksummed(args.address, None)?;
    ctx.transaction_context.set_contract_address(address);

    let evm_code = load_evm_code(args.bytecode)?;

    let context = Context::create();
    let contract = JitContractBuilder::with_context("jit-instructions", &context)
        .expect("Could not build jit contract")
        .debug_ir("jit_test.ll")
        .debug_asm("jit_test.asm")
        .build(evm_code.augment().index())
        .unwrap();

    match contract.transact(&mut ctx) {
        Ok(result) => {
            println!("RESULT {:?}", result);
        }
        e => panic!("Error {e:?}"),
    }

    let JitEvmExecutionContext { memory, .. } = ctx;

    let mut output = [0u8; 32];
    output.copy_from_slice(&memory[..0x20]);

    println!("MEMORY {:?}", output);

    Ok(())
}
