use anyhow::Result;
use aether_codegen::{CodeGenerator, Target, TargetArch, TargetOs};
use aether_frontend::ast::{Module, Item, Stmt, Expr, Value, BinOpKind, Type};

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
                BinOpKind::Eq => Some(if lv == rv { 1 } else { 0 }),
                BinOpKind::Lt => Some(if lv < rv { 1 } else { 0 }),
                BinOpKind::Le => Some(if lv <= rv { 1 } else { 0 }),
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
                BinOpKind::Eq | BinOpKind::Lt | BinOpKind::Le => None,
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
        let mut calls: Vec<(String, Vec<Expr>)> = Vec::new();
        let mut main_ret_call: Option<(String, Vec<Expr>)> = None;
        let mut other_funcs: Vec<&aether_frontend::ast::Function> = Vec::new();
        let mut main_print_calls: Vec<(String, Vec<Expr>)> = Vec::new();
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
                            if let Expr::Call(name, args) = expr {
                                calls.push((name.clone(), args.clone()));
                                main_ret_call = Some((name.clone(), args.clone()));
                            }
                        }
                        Stmt::Println(s) => {
                            let mut bytes = s.clone().into_bytes();
                            bytes.push(b'\n');
                            prints.push((String::from_utf8(bytes).unwrap(), s.as_bytes().len() + 1));
                        }
                        Stmt::Expr(Expr::Call(name, args)) => {
                            calls.push((name.clone(), args.clone()));
                        }
                        Stmt::PrintExpr(Expr::Call(name, args)) => {
                            main_print_calls.push((name.clone(), args.clone()));
                        }
                        Stmt::PrintExpr(e) => {
                            if let Expr::IfElse { cond, then_expr, else_expr } = e {
                                if let Some(cv) = eval_int_expr(cond) {
                                    let chosen = if cv != 0 { then_expr } else { else_expr };
                                    if let Expr::Lit(Value::String(s)) = &**chosen {
                                        let mut bytes = s.clone().into_bytes();
                                        bytes.push(b'\n');
                                        prints.push((String::from_utf8(bytes).unwrap(), s.as_bytes().len() + 1));
                                    }
                                }
                            }
                        }

                        _ => {}
                    }
                }
            } else {
                other_funcs.push(func);
            }
        }
        let mut out = String::new();
        out.push_str(
r#"
        .global _start
        .text
_start:
"#);
        for (name, args) in &main_print_calls {
            if args.is_empty() {
                out.push_str(&format!("        bl {}\n", name));
                out.push_str(
"        mov x2, x1
        mov x1, x0
        mov x0, #1
        mov x8, #64
        svc #0
        mov x8, #64
        mov x0, #1
        adrp x1, .LSNL
        add x1, x1, :lo12:.LSNL
        mov x2, #1
        svc #0
");
            }
        }
        let mut call_arg_rodata: Vec<(String, String)> = Vec::new();
        if let Some((ref name, ref args)) = main_ret_call {
            if !args.is_empty() {
                if let Expr::Lit(Value::Int(v0)) = &args[0] {
                    out.push_str(&format!("        mov x0, #{}\n", v0));
                }
            }
            out.push_str(&format!("        bl {}\n", name));
        } else {
            for (cidx, (name, args)) in calls.iter().enumerate() {
                if !args.is_empty() {
                    match &args[0] {
                        Expr::Lit(Value::Int(v0)) => {
                            out.push_str(&format!("        mov x0, #{}\n", v0));
                        }
                        Expr::Lit(Value::String(s)) => {
                            let mut bytes = s.clone().into_bytes();
                            let len = bytes.len();
                            let lbl = format!(".LSARG{}", cidx);
                            out.push_str(&format!(
"        adrp x0, {0}
        add x0, x0, :lo12:{0}
        mov x1, #{1}
", lbl, len));
                            call_arg_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                        }
                        _ => {}
                    }
                }
                out.push_str(&format!("        bl {}\n", name));
            }
        }
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
            if main_ret_call.is_some() {
                out.push_str(
"        mov x8, #93
        svc #0
");
            } else {
                out.push_str(&format!(
"        mov x8, #93
        mov x0, #{}
        svc #0
", exit_code));
            }
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
            if !main_print_calls.is_empty() {
                out.push_str(".LSNL:\n        .byte 10\n");
            }
            for (lbl, s) in &call_arg_rodata {
                out.push_str(&format!("{}:\n        .ascii \"", lbl));
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
            if main_ret_call.is_some() {
                out.push_str(
"        mov x8, #93
        svc #0
");
            } else {
                out.push_str(&format!(
"        mov x8, #93
        mov x0, #{}
        svc #0
", exit_code));
            }
            if !prints.is_empty() || !call_arg_rodata.is_empty() {
                out.push_str("\n        .section .rodata\n");
                for (idx, (s, _len)) in prints.iter().enumerate() {
                    out.push_str(&format!(".LS{}:\n        .ascii \"", idx));
                }
                for (lbl, s) in &call_arg_rodata {
                    out.push_str(&format!("{}:\n        .ascii \"", lbl));
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
        out.push_str("\n        .text\n");
        let mut func_rodata: Vec<(String, String)> = Vec::new();
        let mut need_nl = bool::from(false);
        for func in other_funcs {
            out.push_str("\n");
            out.push_str(&format!("{}:\n", func.name));
            let mut ret_i: i64 = 0;
            let mut ret_f: Option<f64> = None;
            let mut fi: usize = 0;
            for stmt in &func.body {
                match stmt {
                    Stmt::Println(s) => {
                        let mut bytes = s.clone().into_bytes();
                        bytes.push(b'\n');
                        let len = s.as_bytes().len() + 1;
                        let lbl = format!(".LSF_{}_{}", func.name, fi);
                        out.push_str(&format!(
"        mov x8, #64
        mov x0, #1
        adrp x1, {0}
        add x1, x1, :lo12:{0}
        mov x2, #{1}
        svc #0
", lbl, len));
                        func_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                        fi += 1;
                    }
                    Stmt::PrintExpr(e) => {
                        match e {
                            Expr::Lit(Value::String(s)) => {
                                let mut bytes = s.clone().into_bytes();
                                bytes.push(b'\n');
                                let len = s.as_bytes().len() + 1;
                                let lbl = format!(".LSF_{}_{}", func.name, fi);
                                out.push_str(&format!(
"        mov x8, #64
        mov x0, #1
        adrp x1, {0}
        add x1, x1, :lo12:{0}
        mov x2, #{1}
        svc #0
", lbl, len));
                                func_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                fi += 1;
                            }
                            Expr::Var(name) => {
                                let regs = ["x0","x1","x2","x3","x4","x5","x6","x7"];
                                let mut slot = 0usize;
                                for p in &func.params {
                                    if p.name == *name {
                                        if let Type::String = p.ty {
                                            if slot + 1 < regs.len() {
                                                let ptr_reg = regs[slot];
                                                let len_reg = regs[slot + 1];
                                                out.push_str(&format!(
"        mov x2, {len}
        mov x1, {ptr}
        mov x0, #1
        mov x8, #64
        svc #0
", len=len_reg, ptr=ptr_reg));
                                                out.push_str(
"        mov x8, #64
        mov x0, #1
        adrp x1, .LSNL
        add x1, x1, :lo12:.LSNL
        mov x2, #1
        svc #0
");
                                                need_nl = true;
                                            }
                                        }
                                        break;
                                    } else {
                                        match p.ty {
                                            Type::String => slot += 2,
                                            _ => slot += 1,
                                        }
                                    }
                                }
                            }
                            Expr::Call(name, args) => {
                                if args.is_empty() {
                                    out.push_str(&format!("        bl {}\n", name));
                                    out.push_str(
"        mov x2, x1
        mov x1, x0
        mov x0, #1
        mov x8, #64
        svc #0
");
                                    out.push_str(
"        mov x8, #64
        mov x0, #1
        adrp x1, .LSNL
        add x1, x1, :lo12:.LSNL
        mov x2, #1
        svc #0
");
                                    need_nl = true;
                                }
                            }
                            Expr::IfElse { cond, then_expr, else_expr } => {
                                if let Some(cv) = eval_int_expr(cond) {
                                    let chosen = if cv != 0 { then_expr } else { else_expr };
                                    match &**chosen {
                                        Expr::Lit(Value::String(s)) => {
                                            let mut bytes = s.clone().into_bytes();
                                            bytes.push(b'\n');
                                            let len = s.as_bytes().len() + 1;
                                            let lbl = format!(".LSF_{}_{}", func.name, fi);
                                            out.push_str(&format!(
"        mov x8, #64
        mov x0, #1
        adrp x1, {0}
        add x1, x1, :lo12:{0}
        mov x2, #{1}
        svc #0
", lbl, len));
                                            func_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                            fi += 1;
                                        }
                                        Expr::Var(name) => {
                                            let regs = ["x0","x1","x2","x3","x4","x5","x6","x7"];
                                            let mut slot = 0usize;
                                            for p in &func.params {
                                                if p.name == *name {
                                                    if let Type::String = p.ty {
                                                        if slot + 1 < regs.len() {
                                                            let ptr_reg = regs[slot];
                                                            let len_reg = regs[slot + 1];
                                                            out.push_str(&format!(
"        mov x2, {len}
        mov x1, {ptr}
        mov x0, #1
        mov x8, #64
        svc #0
", len=len_reg, ptr=ptr_reg));
                                                            out.push_str(
"        mov x8, #64
        mov x0, #1
        adrp x1, .LSNL
        add x1, x1, :lo12:.LSNL
        mov x2, #1
        svc #0
");
                                                            need_nl = true;
                                                        }
                                                    }
                                                    break;
                                                } else {
                                                    match p.ty {
                                                        Type::String => slot += 2,
                                                        _ => slot += 1,
                                                    }
                                                }
                                            }
                                        }
                                        Expr::Call(name, args) => {
                                            if args.is_empty() {
                                                out.push_str(&format!("        bl {}\n", name));
                                                out.push_str(
"        mov x2, x1
        mov x1, x0
        mov x0, #1
        mov x8, #64
        svc #0
");
                                                out.push_str(
"        mov x8, #64
        mov x0, #1
        adrp x1, .LSNL
        add x1, x1, :lo12:.LSNL
        mov x2, #1
        svc #0
");
                                                need_nl = true;
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    Stmt::Return(expr) => {
                        if let Some(v) = eval_int_expr(expr) {
                            ret_i = v;
                        }
                        if let Some(fv) = eval_f64_expr(expr) {
                            ret_f = Some(fv);
                        }
                        if let Expr::Lit(Value::String(s)) = expr {
                            let bytes = s.clone().into_bytes();
                            let len = bytes.len();
                            let lbl = format!(".LSRET_{}_{}", func.name, fi);
                            out.push_str(&format!(
"        adrp x0, {0}
        add x0, x0, :lo12:{0}
        mov x1, #{1}
        ret
", lbl, len));
                            func_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                            fi += 1;
                        }
                    }
                    _ => {}
                }
            }
            if let Some(fv) = ret_f {
                let bits = fv.to_bits();
                out.push_str(
r#"        adrp x1, .LC1
        add x1, x1, :lo12:.LC1
        ldr d0, [x1]
        ret
"#);
                let lo = bits as u32;
                let hi = (bits >> 32) as u32;
                if !out.contains("\n.LC1:\n") {
                    out.push_str("\n        .section .rodata\n.LC1:\n");
                    out.push_str(&format!("        .long {}\n        .long {}\n", lo, hi));
                }
            } else {
                out.push_str(&format!("        mov x0, #{}\n        ret\n", ret_i));
            }
        }
        if !func_rodata.is_empty() || need_nl {
            out.push_str("\n        .section .rodata\n");
            for (lbl, s) in &func_rodata {
                out.push_str(&format!("{}:\n        .ascii \"", lbl));
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
            if need_nl && !out.contains("\n.LSNL:\n") {
                out.push_str(".LSNL:\n        .byte 10\n");
            }
            out.push_str("\n        .text\n");
        }
        Ok(out.trim_start().to_string())
    }
}
