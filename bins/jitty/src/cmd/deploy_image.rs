use crate::util::parse_bytes;

use eyre::eyre;
use revm::{
    db::InMemoryDB,
    inspectors::CustomPrintTracer,
    primitives::{
        Account, AccountInfo, Address, Bytes, CreateScheme, Eval, ExecutionResult, HashMap, Output,
        ResultAndState, TransactTo, B256, U256,
    },
    EVM,
};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Deploy contract & output post-deploy state.
#[derive(clap::Args, Debug)]
#[command(author, version, about, long_about = None)]
pub struct DeployCmd {
    /// Bytecode to deploy
    #[arg(long)]
    initcode: PathBuf,
    /// Constructor args
    #[arg(long, value_parser = parse_bytes)]
    args: Bytes,
    /// Run deploy with inspector
    #[arg(long, default_value = "false")]
    inspect: bool,
}

impl DeployCmd {
    pub fn run(self) -> eyre::Result<()> {
        let image = DeployImage::from_file(&self.initcode, &self.args, self.inspect)?;
        println!("{}", serde_json::to_string(&image)?);
        Ok(())
    }
}

#[derive(Clone, Serialize, Deserialize)]
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
