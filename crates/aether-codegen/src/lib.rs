use anyhow::Result;
use aether_frontend::ast::Module;

#[derive(Debug, Clone, Copy)]
pub enum TargetArch {
    X86_64,
    AArch64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetOs {
    Linux,
    Windows,
    MacOs,
    Unknown,
}

#[derive(Debug, Clone)]
pub struct Target {
    pub arch: TargetArch,
    pub os: TargetOs,
}

impl Target {
    pub fn triple(&self) -> &'static str {
        match (self.arch, self.os) {
            (TargetArch::X86_64, TargetOs::Linux) => "x86_64-unknown-linux-gnu",
            (TargetArch::X86_64, TargetOs::Windows) => "x86_64-pc-windows-gnu",
            (TargetArch::AArch64, TargetOs::Linux) => "aarch64-unknown-linux-gnu",
            (TargetArch::AArch64, TargetOs::MacOs) => "aarch64-apple-darwin",
            _ => "x86_64-pc-windows-gnu",
        }
    }
}

pub trait CodeGenerator {
    fn target(&self) -> &Target;
    fn generate(&mut self, module: &Module) -> Result<String>;
}
