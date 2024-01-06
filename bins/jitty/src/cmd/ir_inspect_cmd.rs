use crate::util::DeployImage;
use inkwell::context::Context;

use jit_contract::{
    code::{EvmCode, EvmOpParserMode},
    jit::contract::JitContractBuilder,
};
use revm_primitives::LatestSpec;
use std::{fs::File, io::{BufReader, Write}, path::PathBuf};

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
    /// Filename for output
    #[arg(long, short)]
    output: PathBuf,
    /// IR to dump
    #[arg(value_enum, default_value_t = Emit::Blocks)]
    emit: Emit,
}

fn dump_jit_ir(image: &DeployImage, ir_filename: &str, context: &Context) -> eyre::Result<()> {
    JitContractBuilder::with_context("jit-instructions", &context)
        .expect("Could not create builder")
        .debug_ir(ir_filename)
        .build(LatestSpec, &image.code, EvmOpParserMode::Strict)
        .expect("Could not dump IR.");
    Ok(())
}

fn dump_jit_asm(image: &DeployImage, ir_filename: &str, context: &Context) -> eyre::Result<()> {
    JitContractBuilder::with_context("jit-instructions", &context)
        .expect("Could not create builder")
        .debug_asm(ir_filename)
        .build(LatestSpec, &image.code, EvmOpParserMode::Strict)
        .expect("Could not dump ASM.");
    Ok(())
}

impl IrInspectCmd {
    pub fn run(self) -> eyre::Result<()> {
        let file = File::open(self.init_state)?;
        let reader = BufReader::new(file);

        let image: DeployImage = serde_json::from_reader(reader)?;

        let output_path = if self.output.is_relative() {
            let mut current = std::env::current_dir()?;
            current.push(self.output);
            current
        } else {
            self.output.clone()
        };

        let mut output = File::create(output_path.clone())?;

        match self.emit {
            Emit::Blocks => {
                let blocks = EvmCode::new_from_bytes(&image.code, EvmOpParserMode::Strict)?
                    .augment()
                    .blocks::<LatestSpec>();

                output.write_all(&serde_json::to_vec(&blocks)?)?;
            }
            Emit::LlvmIr => {
                let context = Context::create();

                dump_jit_ir(&image, output_path.to_str().unwrap(), &context)?;
            }
            Emit::Asm => {
                let context = Context::create();

                dump_jit_asm(&image, output_path.to_str().unwrap(), &context)?;
            }
        }

        Ok(())
    }
}
