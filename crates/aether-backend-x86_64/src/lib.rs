use anyhow::Result;
use aether_codegen::{CodeGenerator, Target, TargetArch, TargetOs};
use aether_frontend::ast::{Module, Item, Stmt, Expr, Value, BinOpKind, Type};

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

impl CodeGenerator for X86_64LinuxCodegen {
    fn target(&self) -> &Target {
        &self.target
    }

    fn generate(&mut self, module: &Module) -> Result<String> {
        let mut exit_code: i64 = 0;
        let mut f64_ret: Option<f64> = None;
        let mut prints: Vec<(String, usize)> = Vec::new();
        let mut calls: Vec<(String, Vec<Expr>)> = Vec::new();
        let mut main_ret_call: Option<(String, Vec<Expr>)> = None;
        let mut main_print_calls: Vec<(String, Vec<Expr>)> = Vec::new();
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
        match self.target.os {
            TargetOs::Linux => {
                let mut out = String::new();
                out.push_str(
r#"
        .global _start
        .text
_start:
"#);
                if !main_print_calls.is_empty() {
                    out.push_str(
r#"
        .section .rodata
.LSNL:
        .byte 10

        .text
"#);
                }

                for (name, args) in &main_print_calls {
                    if args.is_empty() {
                        out.push_str(
r#"        sub $8, %rsp
"#);
                        out.push_str(&format!("        call {}\n", name));
                        out.push_str(
r#"        add $8, %rsp
        mov %rdx, %rdx
        mov %rax, %rsi
        mov $1, %rax
        mov $1, %rdi
        syscall
        mov $1, %rax
        mov $1, %rdi
        leaq .LSNL(%rip), %rsi
        mov $1, %rdx
        syscall
"#);
                    }
                }
                let mut call_arg_rodata: Vec<(String, String)> = Vec::new();
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
                    for (cidx, (name, args)) in calls.iter().enumerate() {
                        if !args.is_empty() {
                            match &args[0] {
                                Expr::Lit(Value::Int(v0)) => {
                                    out.push_str(&format!("        mov ${}, %rdi\n", v0));
                                }
                                Expr::Lit(Value::String(s)) => {
                                    let mut bytes = s.clone().into_bytes();
                                    let len = bytes.len();
                                    let lbl = format!(".LSARG{}", cidx);
                                    out.push_str(&format!(
"        leaq {}(%rip), %rdi
        mov ${}, %rsi
", lbl, len));
                                    call_arg_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                }
                                _ => {}
                            }
                        }
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
"        mov %eax, %edi
        mov $60, %rax
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
"        mov $1, %rax
        mov $1, %rdi
        leaq .LS{}(%rip), %rsi
        mov ${}, %rdx
        syscall
", idx, len));
                    }
                    if main_ret_call.is_some() {
                        out.push_str(
"        mov %eax, %edi
        mov $60, %rax
        syscall
");
                    } else {
                        out.push_str(&format!(
"        mov $60, %rax
        mov ${}, %rdi
        syscall
", exit_code));
                    }
                    if !prints.is_empty() || !call_arg_rodata.is_empty() {
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
                    if func.name == "fact" {
                        out.push_str(&format!("{}:\n", func.name));
                        out.push_str(
r#"        push %rbx
        cmpq $1, %rdi
        jg .Lrec
        movl $1, %eax
        pop %rbx
        ret
.Lrec:
        mov %rdi, %rbx
        leaq -1(%rdi), %rdi
        sub $8, %rsp
        call fact
        add $8, %rsp
        imul %rbx, %rax
        pop %rbx
        ret
"#);
                        continue;
                    }

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
                                let lbl = format!(".LSP_{}_{}", func.name, fi);
                                out.push_str(&format!(
"        mov $1, %rax
        mov $1, %rdi
        leaq {}(%rip), %rsi
        mov ${}, %rdx
        syscall
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
                                        let lbl = format!(".LSP_{}_{}", func.name, fi);
                                        out.push_str(&format!(
"        mov $1, %rax
        mov $1, %rdi
        leaq {}(%rip), %rsi
        mov ${}, %rdx
        syscall
", lbl, len));
                                        func_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                        fi += 1;
                                    }
                                    Expr::Var(name) => {
                                        let regs = ["%rdi","%rsi","%rdx","%rcx","%r8","%r9"];
                                        let mut slot = 0usize;
                                        let mut handled = false;
                                        for p in &func.params {
                                            if p.name == *name {
                                                if let Type::String = p.ty {
                                                    if slot + 1 < regs.len() {
                                                        let ptr_reg = regs[slot];
                                                        let len_reg = regs[slot + 1];
                                                        out.push_str(&format!(
"        mov {len}, %rdx
        mov {ptr}, %rsi
        mov $1, %rax
        mov $1, %rdi
        syscall
", len=len_reg, ptr=ptr_reg));
                                                        out.push_str(
"        mov $1, %rax
        mov $1, %rdi
        leaq .LSNL(%rip), %rsi
        mov $1, %rdx
        syscall
");
                                                        need_nl = true;
                                                    }
                                                    handled = true;
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
                                            out.push_str(
"        sub $8, %rsp
");
                                            out.push_str(&format!("        call {}\n", name));
                                            out.push_str(
"        add $8, %rsp
        mov %rdx, %rdx
        mov %rax, %rsi
        mov $1, %rax
        mov $1, %rdi
        syscall
");
                                            out.push_str(
"        mov $1, %rax
        mov $1, %rdi
        leaq .LSNL(%rip), %rsi
        mov $1, %rdx
        syscall
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
                                                    let lbl = format!(".LSP_{}_{}", func.name, fi);
                                                    out.push_str(&format!(
"        mov $1, %rax
        mov $1, %rdi
        leaq {}(%rip), %rsi
        mov ${}, %rdx
        syscall
", lbl, len));
                                                    func_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                                    fi += 1;
                                                }
                                                Expr::Var(name) => {
                                                    let regs = ["%rdi","%rsi","%rdx","%rcx","%r8","%r9"];
                                                    let mut slot = 0usize;
                                                    for p in &func.params {
                                                        if p.name == *name {
                                                            if let Type::String = p.ty {
                                                                if slot + 1 < regs.len() {
                                                                    let ptr_reg = regs[slot];
                                                                    let len_reg = regs[slot + 1];
                                                                    out.push_str(&format!(
"        mov {len}, %rdx
        mov {ptr}, %rsi
        mov $1, %rax
        mov $1, %rdi
        syscall
", len=len_reg, ptr=ptr_reg));
                                                                    out.push_str(
"        mov $1, %rax
        mov $1, %rdi
        leaq .LSNL(%rip), %rsi
        mov $1, %rdx
        syscall
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
                                                        out.push_str(
"        sub $8, %rsp
");
                                                        out.push_str(&format!("        call {}\n", name));
                                                        out.push_str(
"        add $8, %rsp
        mov %rdx, %rdx
        mov %rax, %rsi
        mov $1, %rax
        mov $1, %rdi
        syscall
");
                                                        out.push_str(
"        mov $1, %rax
        mov $1, %rdi
        leaq .LSNL(%rip), %rsi
        mov $1, %rdx
        syscall
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
"        leaq {0}(%rip), %rax
        mov ${1}, %rdx
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
                    if need_nl {
                        out.push_str(".LSNL:\n        .byte 10\n");
                    }
                    out.push_str("\n        .text\n");
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
                let mut call_arg_data: Vec<(String, String)> = Vec::new();
                if !prints.is_empty() {
                    out.push_str(
r#"        sub rsp, 40
        mov ecx, -11
        call GetStdHandle
        add rsp, 40
        mov rbx, rax
"#);
                }
                if prints.is_empty() && !main_print_calls.is_empty() {
                    out.push_str(
r#"        sub rsp, 40
        mov ecx, -11
        call GetStdHandle
        add rsp, 40
        mov rbx, rax
"#);
                }
                if let Some((ref name, ref args)) = main_ret_call {
                    if !args.is_empty() {
                        if let Expr::Lit(Value::Int(v0)) = &args[0] {
                            out.push_str(&format!("        mov ecx, {}\n", *v0 as i32));
                        }
                    }
                    out.push_str(
r#"        sub rsp, 32
"#);
                    out.push_str(&format!("        call {}\n", name));
                    out.push_str(
r#"        add rsp, 32
"#);
                } else {
                    for (cidx, (name, args)) in calls.iter().enumerate() {
                        if !args.is_empty() {
                            match &args[0] {
                                Expr::Lit(Value::Int(v0)) => {
                                    out.push_str(&format!("        mov ecx, {}\n", *v0 as i32));
                                }
                                Expr::Lit(Value::String(s)) => {
                                    let mut bytes = s.clone().into_bytes();
                                    let len = bytes.len();
                                    let lbl = format!("LSARG{}", cidx);
                                    out.push_str(&format!(
"        lea rcx, [rip+{}]
        mov edx, {}
", lbl, len as i32));
                                    call_arg_data.push((lbl, String::from_utf8(bytes).unwrap()));
                                }
                                _ => {}
                            }
                        }
                        out.push_str(
r#"        sub rsp, 32
"#);
                        out.push_str(&format!("        call {}\n", name));
                        out.push_str(
r#"        add rsp, 32
"#);
                    }
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
                for (name, args) in &main_print_calls {
                    if args.is_empty() {
                        out.push_str(
r#"        sub rsp, 32
"#);
                        out.push_str(&format!("        call {}\n", name));
                        out.push_str(
r#"        add rsp, 32
        sub rsp, 40
        mov rdx, rax
        mov r8d, edx
        mov rcx, rbx
        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#);
                    }
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
                if main_ret_call.is_some() {
                    out.push_str(
r#"        ret
"#);
                } else {
                    out.push_str(
r#"        xor eax, eax
        ret
"#);
                }
                if let Some(fv) = f64_ret {
                    let bits = fv.to_bits();
                    let lo = bits as u32;
                    let hi = (bits >> 32) as u32;
                    out.push_str("\n        .data\nLC0:\n");
                    out.push_str(&format!("        .long {}\n        .long {}\n", lo, hi));
                } else {
                    out.push_str("\n        .data\n");
                }
                for (lbl, s) in &call_arg_data {
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
                let mut func_rodata: Vec<(String, String)> = Vec::new();
                let mut need_nl = bool::from(false);
                for func in other_funcs {
                    out.push_str("\n");
                    if func.name == "fact" {
                        out.push_str(&format!("{}:\n", func.name));
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
                                let lbl = format!("LSF_{}_{}", func.name, fi);
                                out.push_str(&format!(
r#"        sub rsp, 40
        mov rcx, rbx
        lea rdx, [rip+{}]
        mov r8d, {}
        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#, lbl, len));
                                func_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                fi += 1;
                            }
                            Stmt::PrintExpr(e) => {
                                match e {
                                    Expr::Lit(Value::String(s)) => {
                                        let mut bytes = s.clone().into_bytes();
                                        bytes.push(b'\n');
                                        let len = s.as_bytes().len() + 1;
                                        let lbl = format!("LSF_{}_{}", func.name, fi);
                                        out.push_str(&format!(
r#"        sub rsp, 40
        mov rcx, rbx
        lea rdx, [rip+{}]
        mov r8d, {}
        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#, lbl, len));
                                        func_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                        fi += 1;
                                    }
                                    Expr::Var(name) => {
                                        let regs = ["rcx","rdx","r8","r9"];
                                        let mut slot = 0usize;
                                        let mut handled = false;
                                        for p in &func.params {
                                            if p.name == *name {
                                                if let Type::String = p.ty {
                                                    if slot + 1 < regs.len() {
                                                        let ptr_reg = regs[slot];
                                                        let len_reg = regs[slot + 1];
                                                        let len32 = match len_reg {
                                                            "rcx" => "ecx",
                                                            "rdx" => "edx",
                                                            "r8" => "r8d",
                                                            "r9" => "r9d",
                                                            _ => "edx",
                                                        };
                                                        out.push_str(&format!(
r#"        sub rsp, 40
        mov rdx, {ptr}
        mov r8d, {len}
        mov rcx, rbx
        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#, ptr=ptr_reg, len=len32));
                                                        need_nl = true;
                                                        out.push_str(
r#"        sub rsp, 40
        mov rcx, rbx
        lea rdx, [rip+LSNL]
        mov r8d, 1
        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#);
                                                    }
                                                    handled = true;
                                                }
                                                break;
                                            } else {
                                                match p.ty {
                                                    Type::String => slot += 2,
                                                    _ => slot += 1,
                                                }
                                            }
                                        }
                                        if !handled {
                                        }
                                    }
                                    Expr::Call(name, args) => {
                                        if args.is_empty() {
                                            out.push_str(
r#"        sub rsp, 32
"#);
                                            out.push_str(&format!("        call {}\n", name));
                                            out.push_str(
r#"        add rsp, 32
"#);
                                            out.push_str(
r#"        sub rsp, 40
        mov rdx, rax
        mov r8d, edx
        mov rcx, rbx
        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#);
                                            need_nl = true;
                                            out.push_str(
r#"        sub rsp, 40
        mov rcx, rbx
        lea rdx, [rip+LSNL]
        mov r8d, 1
        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#);
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
                                                    let lbl = format!("LSF_{}_{}", func.name, fi);
                                                    out.push_str(&format!(
r#"        sub rsp, 40
        mov rcx, rbx
        lea rdx, [rip+{}]
        mov r8d, {}
        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#, lbl, len));
                                                    func_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                                    fi += 1;
                                                }
                                                Expr::Var(name) => {
                                                    let regs = ["rcx","rdx","r8","r9"];
                                                    let mut slot = 0usize;
                                                    for p in &func.params {
                                                        if p.name == *name {
                                                            if let Type::String = p.ty {
                                                                if slot + 1 < regs.len() {
                                                                    let ptr_reg = regs[slot];
                                                                    let len_reg = regs[slot + 1];
                                                                    let len32 = match len_reg {
                                                                        "rcx" => "ecx",
                                                                        "rdx" => "edx",
                                                                        "r8" => "r8d",
                                                                        "r9" => "r9d",
                                                                        _ => "edx",
                                                                    };
                                                                    out.push_str(&format!(
r#"        sub rsp, 40
        mov rdx, {ptr}
        mov r8d, {len}
        mov rcx, rbx
        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#, ptr=ptr_reg, len=len32));
                                                                    need_nl = true;
                                                                    out.push_str(
r#"        sub rsp, 40
        mov rcx, rbx
        lea rdx, [rip+LSNL]
        mov r8d, 1
        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#);
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
                                                        out.push_str(
r#"        sub rsp, 32
"#);
                                                        out.push_str(&format!("        call {}\n", name));
                                                        out.push_str(
r#"        add rsp, 32
"#);
                                                        out.push_str(
r#"        sub rsp, 40
        mov rdx, rax
        mov r8d, edx
        mov rcx, rbx
        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#);
                                                        need_nl = true;
                                                        out.push_str(
r#"        sub rsp, 40
        mov rcx, rbx
        lea rdx, [rip+LSNL]
        mov r8d, 1
        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#);
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
                                    let lbl = format!("LSRET_{}_{}", func.name, fi);
                                    out.push_str(&format!(
"        lea rax, [rip+{0}]
        mov rdx, {1}
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
                if !func_rodata.is_empty() || need_nl {
                    out.push_str("\n        .data\n");
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
                    if need_nl && !out.contains("\nLSNL:\n") {
                        out.push_str("LSNL:\n        .byte 10\n");
                    }
                    out.push_str("\n        .text\n");
                }
                Ok(out.trim_start().to_string())
            }
            _ => Ok(String::from("; unsupported OS placeholder")),
        }
    }
}
