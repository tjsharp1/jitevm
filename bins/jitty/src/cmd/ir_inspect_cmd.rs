use crate::util::DeployImage;

use jit_contract::code::{EvmCode, EvmOpParserMode};
use revm_primitives::LatestSpec;
use std::{fs::File, io::BufReader, path::PathBuf};

#[derive(Clone, Copy, Debug, clap::ValueEnum)]
pub enum Emit {
    Blocks,
    LlvmIr,
    Asm,
}

/// Dump EVM blocks
#[derive(clap::Args, Debug)]
#[command(author, version, about, long_about = None)]
pub struct IrInspectCmd {
    /// Inital contract image (as DeployImage)
    #[arg(long)]
    init_state: PathBuf,
    /// IR to dump
    #[arg(value_enum, default_value_t = Emit::Blocks)]
    emit: Emit,
}

// TODO:
//fn dump_jit_ir(image: &DeployImage, ir_filename: &str, context: &Context) -> eyre::Result<()> {
//    //JitContractBuilder::with_context("jit-instructions", &context)
//    //    .expect("Could not create builder")
//    //    .dump_ir(&image.code, ir_filename)
//    //    .expect("Could not dump IR.")
//    Ok(())
//}

impl IrInspectCmd {
    pub fn run(self) -> eyre::Result<()> {
        let file = File::open(self.init_state)?;
        let reader = BufReader::new(file);

        let image: DeployImage = serde_json::from_reader(reader)?;

        match self.emit {
            Emit::Blocks => {
                let blocks = EvmCode::new_from_bytes(&image.code, EvmOpParserMode::Strict)?
                    .augment()
                    .blocks::<LatestSpec>();

                let output = serde_json::to_string(&blocks)?;
                println!("{}", output);
            }
            o => unimplemented!("{:?} not implemented yet!", o),
        }

        Ok(())
    }
}
