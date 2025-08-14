use anyhow::Result;
use aether_codegen::{CodeGenerator, Target, TargetArch, TargetOs};
use aether_frontend::ast::{Module, Item, Stmt, Expr, Value, BinOpKind};

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

fn eval_int_expr(expr: &Expr) -> Option<i64> {
    match expr {
        Expr::Lit(Value::Int(v)) => Some(*v),
        Expr::BinOp(a, op, b) => {
            let lv = eval_int_expr(a)?;
            let rv = eval_int_expr(b)?;
            match op {
                BinOpKind::Add => Some(lv + rv),
                BinOpKind::Sub => Some(lv - rv),
                BinOpKind::Mul => Some(lv * rv),
                BinOpKind::Div => {
                    if rv == 0 { None } else { Some(lv / rv) }
                }
            }
        }
        _ => None,
    }
}

impl CodeGenerator for X86_64LinuxCodegen {
    fn target(&self) -> &Target {
        &self.target
    }

    fn generate(&mut self, module: &Module) -> Result<String> {
        let mut exit_code: i64 = 0;
        for item in &module.items {
            if let Item::Function(func) = item {
                if func.name == "main" {
                    for stmt in &func.body {
                        if let Stmt::Return(expr) = stmt {
                            if let Some(v) = eval_int_expr(expr) {
                                exit_code = v.clamp(0, 255);
                            }
                        }
                    }
                }
            }
        }
        match self.target.os {
            TargetOs::Linux => Ok(format!(r#"
        .global _start
        .text
_start:
        mov $60, %rax
        mov ${}, %rdi
        syscall
"#, exit_code).trim_start().to_string()),
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
