use std::io::stderr;
use std::path::PathBuf;

use assumls::{run_lint, run_stdio};
use clap::{ArgAction, Parser, Subcommand};
use tracing::{error, info};
use tracing_subscriber::{EnvFilter, fmt};

#[derive(Parser)]
#[command(
    name = "assumls",
    version,
    about = "Assumption language server and checker",
    propagate_version = true
)]
struct Cli {
    #[arg(short, long, action = ArgAction::Count, global = true, help = "Increase log verbosity")]
    verbose: u8,
    #[arg(
        long,
        global = true,
        value_name = "PATH",
        help = "Workspace root override"
    )]
    root: Option<PathBuf>,
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Run the language server.
    Lsp,
    /// Run a one-off static check.
    Check {
        #[arg(value_name = "PATH", help = "Workspace root to check")]
        path: Option<PathBuf>,
    },
}

fn init_tracing(verbose: u8) {
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| {
        let level = match verbose {
            0 => "warn",
            1 => "info",
            2 => "debug",
            _ => "trace",
        };
        EnvFilter::new(level)
    });
    match fmt()
        .with_env_filter(filter.clone())
        .with_ansi(false)
        .with_writer(stderr)
        .try_init()
    {
        Ok(_) => info!(filter = %filter, verbose, "tracing initialized"),
        Err(err) => eprintln!("assumls tracing init failed ({filter}): {err}"),
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let cli = Cli::parse();
    init_tracing(cli.verbose);
    let root_override = cli.root;
    match cli.command {
        Command::Lsp => {
            if let Err(err) = run_stdio(root_override.clone()).await {
                error!(error = %err, "AssumLS LSP failed");
                std::process::exit(1);
            }
        }
        Command::Check { path } => {
            let root = path
                .or(root_override)
                .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));
            match run_lint(root).await {
                Ok(code) => std::process::exit(code),
                Err(err) => {
                    error!(error = %err, "AssumLS check failed");
                    std::process::exit(1);
                }
            }
        }
    }
}
