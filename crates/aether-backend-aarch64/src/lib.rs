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

impl CodeGenerator for AArch64Codegen {
    fn target(&self) -> &Target {
        &self.target
    }

    fn generate(&mut self, module: &Module) -> Result<String> {
        let mut exit_code: i64 = 0;
        let mut f64_ret: Option<f64> = None;
        let mut prints: Vec<(String, usize)> = Vec::new();
        for item in &module.items {
            let func = match item {
                Item::Function(f) => f,
            };
            if func.name == "main" {
                for stmt in &func.body {
                    match stmt {
                        Stmt::Return(expr) => {
                            if let Some(v) = eval_int_expr(expr) {
                                exit_code = v.clamp(0, 255);
                            }
                            if let Some(fv) = eval_f64_expr(expr) {
                                f64_ret = Some(fv);
                            }
                        }
                        Stmt::Println(s) => {
                            let mut bytes = s.clone().into_bytes();
                            bytes.push(b'\n');
                            prints.push((String::from_utf8(bytes).unwrap(), s.as_bytes().len() + 1));
                        }
                        _ => {}
                    }
                }
            }
        }
        let mut out = String::new();
        out.push_str(
r#"
        .global _start
        .text
_start:
"#);
        if let Some(fv) = f64_ret {
            let bits = fv.to_bits();
            out.push_str(
r#"        adrp x1, .LC0
        add x1, x1, :lo12:.LC0
        ldr d0, [x1]
"#);
            for (idx, (_s, len)) in prints.iter().enumerate() {
                out.push_str(&format!(
"        mov x8, #64
        mov x0, #1
        adrp x1, .LS{0}
        add x1, x1, :lo12:.LS{0}
        mov x2, #{1}
        svc #0
", idx, len));
            }
            out.push_str(&format!(
"        mov x8, #93
        mov x0, #{}
        svc #0
", exit_code));
            let lo = bits as u32;
            let hi = (bits >> 32) as u32;
            out.push_str("\n        .section .rodata\n.LC0:\n");
            out.push_str(&format!("        .long {}\n        .long {}\n", lo, hi));
            for (idx, (s, _len)) in prints.iter().enumerate() {
                out.push_str(&format!(".LS{}:\n        .ascii \"", idx));
                for b in s.as_bytes() {
                    let ch = *b as char;
                    match ch {
                        '\n' => out.push_str("\\n"),
                        '\t' => out.push_str("\\t"),
                        '\"' => out.push_str("\\\""),
                        '\\' => out.push_str("\\\\"),
                        _ => out.push(ch),
                    }
                }
                out.push_str("\"\n");
            }
        } else {
            for (idx, (_s, len)) in prints.iter().enumerate() {
                out.push_str(&format!(
"        mov x8, #64
        mov x0, #1
        adrp x1, .LS{0}
        add x1, x1, :lo12:.LS{0}
        mov x2, #{1}
        svc #0
", idx, len));
            }
            out.push_str(&format!(
"        mov x8, #93
        mov x0, #{}
        svc #0
", exit_code));
            if !prints.is_empty() {
                out.push_str("\n        .section .rodata\n");
                for (idx, (s, _len)) in prints.iter().enumerate() {
                    out.push_str(&format!(".LS{}:\n        .ascii \"", idx));
                    for b in s.as_bytes() {
                        let ch = *b as char;
                        match ch {
                            '\n' => out.push_str("\\n"),
                            '\t' => out.push_str("\\t"),
                            '\"' => out.push_str("\\\""),
                            '\\' => out.push_str("\\\\"),
                            _ => out.push(ch),
                        }
                    }
                    out.push_str("\"\n");
                }
            }
        }
        Ok(out.trim_start().to_string())
    }
}
