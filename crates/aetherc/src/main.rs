use anyhow::{anyhow, Result};
use clap::{Parser, ValueEnum};
use std::path::PathBuf;
use std::fs;
use aether_frontend::parse_file_with_imports;
use aether_codegen::{CodeGenerator, Target, TargetArch, TargetOs};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_backend_aarch64::AArch64Codegen;

#[derive(Debug, Clone, ValueEnum)]
enum Arch {
    #[value(alias = "x86_64")]
    X86_64,
    #[value(alias = "aarch64")]
    AArch64,
}

#[derive(Debug, Clone, ValueEnum)]
enum Os {
    Linux,
    Windows,
    Macos,
}

#[derive(Parser, Debug)]
#[command(name = "aetherc")]
#[command(about = "Aether compiler", long_about = None)]
struct Cli {
    #[arg(value_name = "INPUT")]
    input: PathBuf,

    #[arg(short, long, value_name = "FILE", default_value = "out.s")]
    out: PathBuf,

    #[arg(long, value_enum, default_value = "x86_64")]
    arch: Arch,

    #[arg(long, value_enum, default_value = "linux")]
    os: Os,
}

fn detect_target(arch: &Arch, os: &Os) -> Target {
    let arch = match arch {
        Arch::X86_64 => TargetArch::X86_64,
        Arch::AArch64 => TargetArch::AArch64,
    };
    let os = match os {
        Os::Linux => TargetOs::Linux,
        Os::Windows => TargetOs::Windows,
        Os::Macos => TargetOs::MacOs,
    };
    Target { arch, os }
}

fn main() -> Result<()> {
    env_logger::init();
    let cli = Cli::parse();

    let module = parse_file_with_imports(&cli.input)?;

    let target = detect_target(&cli.arch, &cli.os);

    let mut asm = String::new();
    match (target.arch, target.os) {
        (TargetArch::X86_64, TargetOs::Linux) => {
            let mut gen = X86_64LinuxCodegen::new_linux();
            asm = gen.generate(&module)?;
        }
        (TargetArch::X86_64, TargetOs::Windows) => {
            let mut gen = X86_64LinuxCodegen::new_windows();
            asm = gen.generate(&module)?;
        }
        (TargetArch::AArch64, TargetOs::Linux) => {
            let mut gen = AArch64Codegen::new_linux();
            asm = gen.generate(&module)?;
        }
        _ => {
            return Err(anyhow!("Unsupported target for now"));
        }
    }

    fs::write(&cli.out, asm)?;
    println!("Wrote {}", cli.out.display());
    Ok(())
}
