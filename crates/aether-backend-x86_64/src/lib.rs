use anyhow::Result;
use aether_codegen::{CodeGenerator, Target, TargetArch, TargetOs};
use aether_frontend::ast::Module;

pub struct X86_64LinuxCodegen {
    target: Target,
}

impl X86_64LinuxCodegen {
    pub fn new_linux() -> Self {
        Self {
            target: Target { arch: TargetArch::X86_64, os: TargetOs::Linux }
        }
    }
    pub fn new_windows() -> Self {
        Self {
            target: Target { arch: TargetArch::X86_64, os: TargetOs::Windows }
        }
    }
}

impl CodeGenerator for X86_64LinuxCodegen {
    fn target(&self) -> &Target {
        &self.target
    }

    fn generate(&mut self, _module: &Module) -> Result<String> {
        match self.target.os {
            TargetOs::Linux => Ok(r#"
        .global _start
        .text
_start:
        mov $60, %rax
        xor %rdi, %rdi
        syscall
"#.trim_start().to_string()),
            TargetOs::Windows => Ok(r#"
        .intel_syntax noprefix
        .global main
        .text
main:
        xor rax, rax
        ret
"#.trim_start().to_string()),
            _ => Ok(String::from("; unsupported OS placeholder")),
        }
    }
}
