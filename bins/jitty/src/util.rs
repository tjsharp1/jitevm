use eyre::eyre;

use alloy_primitives::hex::FromHex;
use jit_contract::code::{EvmCode, EvmOpParserMode};
use reth_primitives::{Bytes, ChainSpec, DEV, GOERLI, HOLESKY, MAINNET, SEPOLIA};
use std::{path::PathBuf, sync::Arc};

pub fn chainspec_parser(specname: &str) -> eyre::Result<Arc<ChainSpec>> {
    Ok(match specname {
        "mainnet" => MAINNET.clone(),
        "goerli" => GOERLI.clone(),
        "sepolia" => SEPOLIA.clone(),
        "holesky" => HOLESKY.clone(),
        "dev" => DEV.clone(),
        spec => return Err(eyre!("Unknown specname: {spec}")),
    })
}

pub fn parse_bytes(bytes: &str) -> eyre::Result<Bytes> {
    Ok(Bytes::from_hex(bytes)?)
}

pub fn load_evm_code(path: PathBuf) -> eyre::Result<EvmCode> {
    let code = std::fs::read_to_string(path)?;
    let bytes = hex::decode(code)?;

    Ok(EvmCode::new_from_bytes(&bytes, EvmOpParserMode::Strict)?)
}
