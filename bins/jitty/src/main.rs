use clap::{Parser, Subcommand};
use jitty::cmd;

#[derive(Debug, Parser)]
#[command(author, version, about = "JIT tools", long_about = None)]
pub struct Args {
    #[command(subcommand)]
    command: SubCommands,
}

#[derive(Debug, Subcommand)]
pub enum SubCommands {
    /// Run a transaction with the specified contract address
    Run(cmd::RunCmd),
    /// Get the post-deploy state of a contract.
    Deploy(cmd::DeployCmd),
    /// Run a transaction from an initial state.
    ExecuteTx(cmd::ExecuteTxCmd),
    /// Dump JIT IR.
    IrInspect(cmd::IrInspectCmd),
    /// Inspect with REVM.
    RunInspect(cmd::RunInspectCmd),
}

fn main() -> eyre::Result<()> {
    let args = Args::parse();

    match args.command {
        SubCommands::Run(cmd) => cmd.run(),
        SubCommands::Deploy(cmd) => cmd.run(),
        SubCommands::ExecuteTx(cmd) => cmd.run(),
        SubCommands::IrInspect(cmd) => cmd.run(),
        SubCommands::RunInspect(cmd) => cmd.run(),
    }
}
