use crate::util::{DeployImage, parse_bytes};

use revm_primitives::Bytes;
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
