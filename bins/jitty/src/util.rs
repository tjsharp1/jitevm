use eyre::eyre;

use alloy_primitives::hex::FromHex;
use reth_primitives::{ChainSpec, DEV, GOERLI, HOLESKY, MAINNET, SEPOLIA};
use revm::inspectors::CustomPrintTracer;
use revm::{InMemoryDB, EVM};
use revm_primitives::{
    Account, AccountInfo, Address, Bytecode, Bytes, CreateScheme, Eval, ExecutionResult, HashMap,
    Output, ResultAndState, TransactTo, B256, U256,
};
use serde::{Deserialize, Serialize};
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

pub fn load_evm_code(path: PathBuf) -> eyre::Result<Bytes> {
    let code = std::fs::read_to_string(path)?;
    let bytes = hex::decode(code)?;

    Ok(Bytes::copy_from_slice(&bytes))
}

pub fn init_db_from_image(image: &DeployImage) -> eyre::Result<InMemoryDB> {
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeployImage {
    pub address: Address,
    pub code_hash: B256,
    pub code: Bytes,
    pub storage: HashMap<U256, U256>,
}

impl DeployImage {
    pub fn from_file(initcode: &PathBuf, args: &Bytes, inspect: bool) -> eyre::Result<DeployImage> {
        let code_bytes = std::fs::read_to_string(initcode)?;
        let mut code = hex::decode(code_bytes)?;
        code.extend(args);

        let mut database = InMemoryDB::default();

        let info = AccountInfo {
            balance: U256::from(10) * U256::from(1).pow(U256::from(18)),
            nonce: 0,
            code_hash: Default::default(),
            code: None,
        };
        database.insert_account_info(Address::ZERO, info);

        let mut evm = EVM::new();
        evm.database(database);
        evm.env.tx.transact_to = TransactTo::Create(CreateScheme::Create);
        evm.env.tx.data = code.into();

        let ResultAndState { result, mut state } = if inspect {
            evm.inspect(CustomPrintTracer::default())?
        } else {
            evm.transact()?
        };

        match result {
            ExecutionResult::Success {
                reason: Eval::Return,
                output: Output::Create(_, Some(address)),
                ..
            } => {
                let Account { info, storage, .. } =
                    state.remove(&address).expect("Account should exist!");
                let AccountInfo {
                    code_hash, code, ..
                } = info;
                let code = code.expect("Bytecode should exist!").bytecode;

                let mut store = HashMap::new();

                for (key, value) in storage.iter() {
                    if value.present_value != U256::ZERO {
                        store.insert(*key, value.present_value);
                    }
                }

                Ok(DeployImage {
                    address,
                    code_hash,
                    code,
                    storage: store,
                })
            }
            e => return Err(eyre!("Could not deploy contract! {:?}", e)),
        }
    }
}
