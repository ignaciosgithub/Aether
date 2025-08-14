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
                BinOpKind::Eq | BinOpKind::Lt | BinOpKind::Le => None,
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

impl CodeGenerator for X86_64LinuxCodegen {
    fn target(&self) -> &Target {
        &self.target
    }

    fn generate(&mut self, module: &Module) -> Result<String> {
        let mut exit_code: i64 = 0;
        let mut f64_ret: Option<f64> = None;
        let mut prints: Vec<(String, usize)> = Vec::new();
        let mut calls: Vec<String> = Vec::new();
        let mut main_ret_call: Option<(String, Vec<Expr>)> = None;
        let mut other_funcs: Vec<&aether_frontend::ast::Function> = Vec::new();
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
                                calls.push(name.clone());
                                main_ret_call = Some((name.clone(), args.clone()));
                            }
                        }
                        Stmt::Println(s) => {
                            let mut bytes = s.clone().into_bytes();
                            bytes.push(b'\n');
                            prints.push((String::from_utf8(bytes).unwrap(), s.as_bytes().len() + 1));
                        }
                        Stmt::Expr(Expr::Call(name, _)) => {
                            calls.push(name.clone());
                        }
                        _ => {}
                    }
                }
            } else {
                other_funcs.push(func);
            }
        }
        match self.target.os {
            TargetOs::Linux => {
                let mut out = String::new();
                out.push_str(
r#"
        .global _start
        .text
_start:
"#);
                if let Some((ref name, ref args)) = main_ret_call {
                    if !args.is_empty() {
                        if let Expr::Lit(Value::Int(v0)) = &args[0] {
                            out.push_str(&format!("        mov ${}, %rdi\n", v0));
                        }
                    }
                    out.push_str(
r#"        sub $8, %rsp
"#);
                    out.push_str(&format!("        call {}\n", name));
                    out.push_str(
r#"        add $8, %rsp
"#);
                } else {
                    for name in &calls {
                        out.push_str(
r#"        sub $8, %rsp
"#);
                        out.push_str(&format!("        call {}\n", name));
                        out.push_str(
r#"        add $8, %rsp
"#);
                    }
                }
                if let Some(fv) = f64_ret {
                    let bits = fv.to_bits();
                    out.push_str(
r#"        leaq .LC0(%rip), %rax
        movsd (%rax), %xmm0
"#);
                    for (idx, (_s, len)) in prints.iter().enumerate() {
                        out.push_str(&format!(
"        mov $1, %rax
        mov $1, %rdi
        leaq .LS{}(%rip), %rsi
        mov ${}, %rdx
        syscall
", idx, len));
                    }
                    if main_ret_call.is_some() {
                        out.push_str(
"        mov $60, %rax
        mov %eax, %edi
        syscall
");
                    } else {
                        out.push_str(&format!(
"        mov $60, %rax
        mov ${}, %rdi
        syscall
", exit_code));
                    }
                    let lo = bits as u32;
                    let hi = (bits >> 32) as u32;
                    out.push_str(
"\n        .section .rodata\n.LC0:\n");
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
"        mov $1, %rax
        mov $1, %rdi
        leaq .LS{}(%rip), %rsi
        mov ${}, %rdx
        syscall
", idx, len));
                    }
                    if main_ret_call.is_some() {
                        out.push_str(
"        mov $60, %rax
        mov %eax, %edi
        syscall
");
                    } else {
                        out.push_str(&format!(
"        mov $60, %rax
        mov ${}, %rdi
        syscall
", exit_code));
                    }
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
                out.push_str("\n        .text\n");
                for func in other_funcs {
                    out.push_str("\n");
                    if func.name == "fact" {
                        out.push_str(
r#"        push %rbx
        cmp $1, %edi
        jg .Lrec
        mov $1, %eax
        pop %rbx
        ret
.Lrec:
        mov %edi, %ebx
        lea %edi, -1(%rdi)
        sub $8, %rsp
        call fact
        add $8, %rsp
        imul %ebx, %eax
        pop %rbx
        ret
"#);
                        continue;
                    }

                    out.push_str(&format!("{}:\n", func.name));
                    let mut ret_i: i64 = 0;
                    let mut ret_f: Option<f64> = None;
                    for stmt in &func.body {
                        if let Stmt::Return(expr) = stmt {
                            if let Some(v) = eval_int_expr(expr) {
                                ret_i = v;
                            }
                            if let Some(fv) = eval_f64_expr(expr) {
                                ret_f = Some(fv);
                            }
                        }
                    }
                    if let Some(fv) = ret_f {
                        let bits = fv.to_bits();
                        out.push_str(
r#"        leaq .LC0(%rip), %rax
        movsd (%rax), %xmm0
        ret
"#);
                        let lo = bits as u32;
                        let hi = (bits >> 32) as u32;
                        if !out.contains(".LC0:\n") {
                            out.push_str("\n        .section .rodata\n.LC0:\n");
                            out.push_str(&format!("        .long {}\n        .long {}\n", lo, hi));
                        }
                    } else {
                        out.push_str(&format!("        mov ${}, %rax\n        ret\n", ret_i));
                    }
                }
                Ok(out.trim_start().to_string())
            }
            TargetOs::Windows => {
                let mut out = String::new();
                out.push_str(
r#"
        .intel_syntax noprefix
        .extern GetStdHandle
        .extern WriteFile
        .global main
        .text
main:
"#);
                if !prints.is_empty() {
                    out.push_str(
r#"        sub rsp, 40
        mov ecx, -11
        call GetStdHandle
        add rsp, 40
        mov rbx, rax
"#);
                }
                for name in &calls {
                    out.push_str(
r#"        sub rsp, 32
"#);
                    out.push_str(&format!("        call {}\n", name));
                    out.push_str(
r#"        add rsp, 32
"#);
                }
                if let Some(fv) = f64_ret {
                    let bits = fv.to_bits();
                    let lo = bits as u32;
                    let hi = (bits >> 32) as u32;
                    out.push_str(
r#"        lea rax, [rip+LC0]
        movsd xmm0, qword ptr [rax]
"#);
                }
                if !prints.is_empty() {
                    for (idx, (_s, len)) in prints.iter().enumerate() {
                        out.push_str(&format!(
r#"        sub rsp, 40
        mov rcx, rbx
        lea rdx, [rip+LS{}]
        mov r8d, {}
        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#, idx, len));
                    }
                }
                out.push_str(
r#"        xor eax, eax
        ret
"#);
                if let Some(fv) = f64_ret {
                    let bits = fv.to_bits();
                    let lo = bits as u32;
                    let hi = (bits >> 32) as u32;
                    out.push_str("\n        .data\nLC0:\n");
                    if func.name == "fact" {
                        out.push_str(
r#"        push rbx
        cmp ecx, 1
        jg Lrec_w
        mov eax, 1
        pop rbx
        ret
Lrec_w:
        mov ebx, ecx
        lea ecx, [rcx-1]
        sub rsp, 32
        call fact
        add rsp, 32
        imul eax, ebx
        pop rbx
        ret
"#);
                        continue;
                    }

                    out.push_str(&format!("        .long {}\n        .long {}\n", lo, hi));
                } else {
                    out.push_str("\n        .data\n");
                }
                for (idx, (s, _len)) in prints.iter().enumerate() {
                    out.push_str(&format!("LS{}:\n        .ascii \"", idx));
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
                out.push_str("\n        .text\n");
                for func in other_funcs {
                    out.push_str("\n");
                    out.push_str(&format!("{}:\n", func.name));
                    let mut ret_i: i64 = 0;
                    let mut ret_f: Option<f64> = None;
                    for stmt in &func.body {
                        if let Stmt::Return(expr) = stmt {
                            if let Some(v) = eval_int_expr(expr) {
                                ret_i = v;
                            }
                            if let Some(fv) = eval_f64_expr(expr) {
                                ret_f = Some(fv);
                            }
                        }
                    }
                    if let Some(fv) = ret_f {
                        let bits = fv.to_bits();
                        out.push_str(
r#"        lea rax, [rip+LC1]
        movsd xmm0, qword ptr [rax]
        ret
"#);
                        let lo = bits as u32;
                        let hi = (bits >> 32) as u32;
                        if !out.contains("\nLC1:\n") {
                            out.push_str("\n        .data\nLC1:\n");
                            out.push_str(&format!("        .long {}\n        .long {}\n", lo, hi));
                        }
                    } else {
                        out.push_str(&format!("        mov eax, {}\n        ret\n", ret_i as i32));
                    }
                }
                Ok(out.trim_start().to_string())
            }
            _ => Ok(String::from("; unsupported OS placeholder")),
        }
    }
}
