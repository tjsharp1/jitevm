use crate::util::{init_db_from_image, parse_bytes, DeployImage};
use crate::inspectors::jumpdest::JumpdestInspector;

use revm::{EVM, InMemoryDB, inspectors::CustomPrintTracer};
use revm_primitives::{Bytes, TransactTo};
use std::{fs::File, io::BufReader, path::PathBuf};

#[derive(Clone, Copy, Debug, clap::ValueEnum)]
pub enum Inspector {
    Print,
    Jumpdest,
}

/// Run JIT with contract.
#[derive(clap::Args, Debug)]
#[command(author, version, about, long_about = None)]
pub struct RunInspectCmd {
    /// Set inital contract state (as DeployImage)
    #[arg(long)]
    init_state: PathBuf,
    /// Calldata
    #[arg(long, value_parser = parse_bytes)]
    calldata: Bytes,
    /// Inspector
    #[arg(value_enum, default_value_t = Inspector::Print)]
    inspector: Inspector,
}

fn run_inspector(image: &DeployImage, calldata: Bytes, inspector: Inspector) -> eyre::Result<()> {
    let database = init_db_from_image(image)?;

    let mut evm: EVM<InMemoryDB> = EVM::new();
    evm.database(database);
    evm.env.tx.transact_to = TransactTo::Call(image.address);
    evm.env.tx.data = calldata.into();

    let start = std::time::Instant::now();
    match inspector {
        Inspector::Print => {
            let inspector = CustomPrintTracer::default();
            let _ = evm.inspect(inspector)?;
        }
        Inspector::Jumpdest => {
            let mut inspector = JumpdestInspector::new();
            let _ = evm.inspect(&mut inspector)?;
            println!("Jumpdest hotspots: {:#?}", inspector);
        }
    }
    println!("EVM took {} msec", start.elapsed().as_millis());

    Ok(())
}

impl RunInspectCmd {
    pub fn run(self) -> eyre::Result<()> {
        let file = File::open(self.init_state)?;
        let reader = BufReader::new(file);

        let image: DeployImage = serde_json::from_reader(reader)?;

        run_inspector(&image, self.calldata.clone(), self.inspector)?;

        Ok(())
    }
}
