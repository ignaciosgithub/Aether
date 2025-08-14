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

fn eval_f64_expr(expr: &Expr) -> Option<f64> {
    match expr {
        Expr::Lit(Value::Float64(v)) => Some(*v),
        Expr::Lit(Value::Float32(v)) => Some(*v as f64),
        Expr::Lit(Value::Int(v)) => Some(*v as f64),
        Expr::BinOp(a, op, b) => {
            let lv = eval_f64_expr(a)?;
            let rv = eval_f64_expr(b)?;
            match op {
                BinOpKind::Add => Some(lv + rv),
                BinOpKind::Sub => Some(lv - rv),
                BinOpKind::Mul => Some(lv * rv),
                BinOpKind::Div => {
                    if rv == 0.0 { None } else { Some(lv / rv) }
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
        let mut f64_ret: Option<f64> = None;
        for item in &module.items {
            let func = match item {
                Item::Function(f) => f,
            };
            if func.name == "main" {
                for stmt in &func.body {
                    if let Stmt::Return(expr) = stmt {
                        if let Some(v) = eval_int_expr(expr) {
                            exit_code = v.clamp(0, 255);
                        }
                        if let Some(fv) = eval_f64_expr(expr) {
                            f64_ret = Some(fv);
                        }
                    }
                }
            }
        }
        match self.target.os {
            TargetOs::Linux => {
                if let Some(fv) = f64_ret {
                    let bits = fv.to_bits();
                    let lo = bits as u32;
                    let hi = (bits >> 32) as u32;
                    Ok(format!(r#"
        .global _start
        .text
_start:
        leaq .LC0(%rip), %rax
        movsd (%rax), %xmm0
        mov $60, %rax
        mov ${}, %rdi
        syscall
        .section .rodata
.LC0:
        .long {}
        .long {}
        "#, exit_code, lo, hi).trim_start().to_string())
                } else {
                    Ok(format!(r#"
        .global _start
        .text
_start:
        mov $60, %rax
        mov ${}, %rdi
        syscall
"#, exit_code).trim_start().to_string())
                }
            }
            TargetOs::Windows => {
                if let Some(fv) = f64_ret {
                    let bits = fv.to_bits();
                    let lo = bits as u32;
                    let hi = (bits >> 32) as u32;
                    Ok(format!(r#"
        .intel_syntax noprefix
        .global main
        .text
main:
        lea rax, [rip+LC0]
        movsd xmm0, qword ptr [rax]
        xor eax, eax
        ret
        .data
LC0:
        .long {}
        .long {}
"#, lo, hi).trim_start().to_string())
                } else {
                    Ok(r#"
        .intel_syntax noprefix
        .global main
        .text
main:
        xor rax, rax
        ret
"#.trim_start().to_string())
                }
            }
            _ => Ok(String::from("; unsupported OS placeholder")),
        }
    }
}
