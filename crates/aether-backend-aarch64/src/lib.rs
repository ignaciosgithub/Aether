use anyhow::Result;
use aether_codegen::{CodeGenerator, Target, TargetArch, TargetOs};
use aether_frontend::ast::{Module, Item, Stmt, Expr, Value, BinOpKind};

pub struct AArch64Codegen {
    target: Target,
}

impl AArch64Codegen {
    pub fn new_linux() -> Self {
        Self {
            target: Target { arch: TargetArch::AArch64, os: TargetOs::Linux }
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

impl CodeGenerator for AArch64Codegen {
    fn target(&self) -> &Target {
        &self.target
    }

    fn generate(&mut self, module: &Module) -> Result<String> {
        let mut exit_code: i64 = 0;
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
                    }
                }
            }
        }
        Ok(format!(r#"
        .global _start
        .text
_start:
        mov x8, #93
        mov x0, #{}
        svc #0
"#, exit_code).trim_start().to_string())
    }
}
