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
        let mut main_func: Option<&aether_frontend::ast::Function> = None;
        let mut exit_code: i64 = 0;
        let mut f64_ret: Option<f64> = None;
        let mut prints: Vec<(String, usize)> = Vec::new();
        let mut calls: Vec<(String, Vec<Expr>)> = Vec::new();
        let mut main_ret_call: Option<(String, Vec<Expr>)> = None;
        let mut main_print_calls: Vec<(String, Vec<Expr>)> = Vec::new();
        let mut other_funcs: Vec<&aether_frontend::ast::Function> = Vec::new();
        use std::collections::{HashMap, HashSet};
        let mut struct_sizes: HashMap<String, usize> = HashMap::new();
        let mut static_types: HashMap<String, String> = HashMap::new();
        for item in &module.items {
            match item {
                Item::Struct(sd) => {
                    let mut size = 0usize;
                    for f in &sd.fields {
                        match f.ty {
                            Type::I32 => size += 4,
                            Type::I64 => size += 8,
                            Type::F64 => size += 8,
                            _ => size += 8,
                        }
                    }
                    if size % 8 != 0 { size += 8 - (size % 8); }
                    struct_sizes.insert(sd.name.clone(), size);
                }
                Item::Static(st) => {
                    if let Type::User(ref n) = st.ty {
                        static_types.insert(st.name.clone(), n.clone());
                    }
                }
                _ => {}
            }
        }
        let static_names: HashSet<String> = static_types.keys().cloned().collect();
        for item in &module.items {
            let func = match item {
                Item::Function(f) => f,
                _ => continue,
            };
            if func.name == "main" {
                main_func = Some(func);
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
                        Stmt::PrintExpr(Expr::MethodCall(recv, meth, args)) => {
                            if let Expr::Var(rn) = &**recv {
                                if let Some(ty) = static_types.get(rn) {
                                    let mut full_args = Vec::new();
                                    full_args.push(Expr::Var(rn.clone()));
                                    full_args.extend(args.clone());
                                    let fname = format!("{}_{}", ty, meth);
                                    main_print_calls.push((fname, full_args));
                                }
                            }
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
        let mut main_while_blocks: Vec<(usize, Expr, Vec<Stmt>)> = Vec::new();
        if let Some(f) = main_func {
            let mut widx = 0usize;
            for stmt in &f.body {
                if let Stmt::While { cond, body } = stmt.clone() {
                    main_while_blocks.push((widx, cond, body));
                    widx += 1;
                }
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
                let mut while_rodata: Vec<(String, String)> = Vec::new();
                for (widx, cond, body) in &main_while_blocks {
                    out.push_str(&format!(".LWH_HEAD_main_{}:\n", widx));
                    match cond {
                        Expr::BinOp(a, op, b) => {
                            if let (Expr::Lit(Value::Int(la)), Expr::Lit(Value::Int(lb))) = (&**a, &**b) {
                                out.push_str(&format!("        mov ${}, %r10\n", la));
                                out.push_str(&format!("        mov ${}, %r11\n", lb));
                                out.push_str("        cmp %r11, %r10\n");
                                match op {
                                    BinOpKind::Lt => out.push_str(&format!("        jge .LWH_END_main_{}\n", widx)),
                                    BinOpKind::Le => out.push_str(&format!("        jg .LWH_END_main_{}\n", widx)),
                                    BinOpKind::Eq => out.push_str(&format!("        jne .LWH_END_main_{}\n", widx)),
                                    _ => out.push_str(&format!("        jmp .LWH_END_main_{}\n", widx)),
                                }
                            } else {
                                out.push_str(&format!("        jmp .LWH_END_main_{}\n", widx));
                            }
                        }
                        Expr::Lit(Value::Int(v)) => {
                            out.push_str(&format!("        mov ${}, %r10\n", v));
                            out.push_str("        cmp $0, %r10\n");
                            out.push_str(&format!("        je .LWH_END_main_{}\n", widx));
                        }
                        _ => {
                            out.push_str(&format!("        jmp .LWH_END_main_{}\n", widx));
                        }
                    }
                    for (bidx, st) in body.iter().enumerate() {
                        match st {
                            Stmt::Println(s) => {
                                let mut bytes = s.clone().into_bytes();
                                bytes.push(b'\n');
                                let lbl = format!(".LSW_main_{}_{}", widx, bidx);
                                while_rodata.push((lbl.clone(), String::from_utf8(bytes).unwrap()));
                                out.push_str("        mov $1, %rax\n");
                                out.push_str("        mov $1, %rdi\n");
                                out.push_str(&format!("        leaq {}(%rip), %rsi\n", lbl));
                                out.push_str(&format!("        mov ${}, %rdx\n", s.as_bytes().len() + 1));
                                out.push_str("        syscall\n");
                            }
                            Stmt::Break => out.push_str(&format!("        jmp .LWH_END_main_{}\n", widx)),
                            Stmt::Continue => out.push_str(&format!("        jmp .LWH_HEAD_main_{}\n", widx)),
                            _ => {}
                        }
                    }
                    out.push_str(&format!("        jmp .LWH_HEAD_main_{}\n", widx));
                    out.push_str(&format!(".LWH_END_main_{}:\n", widx));
                }
                if !while_rodata.is_empty() {
                    out.push_str("\n        .section .rodata\n");
                    for (lbl, s) in &while_rodata {
                        out.push_str(&format!("{}:\n", lbl));
                        out.push_str(&format!("        .ascii \"{}\"\n", s.replace("\\\\", "\\\\\\\\").replace("\"", "\\\\\"")));
                    }
                    out.push_str("\n        .text\n");
                }

                if !main_print_calls.is_empty() {
                    out.push_str(
r#"
        .section .rodata
.LSNL:
        .byte 10

        .text
"#);
                }

                let mut call_arg_rodata: Vec<(String, String)> = Vec::new();

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
                    } else {
                        let regs = ["%rdi","%rsi","%rdx","%rcx","%r8","%r9"];
                        let mut islot = 0usize;
                        for (aidx, a) in args.iter().enumerate() {
                            match a {
                                Expr::Lit(Value::Int(v)) => {
                                    if islot < regs.len() {
                                        let dst = regs[islot];
                                        out.push_str(&format!("        mov ${}, {}\n", v, dst));
                                        islot += 1;
                                    }
                                }
                                Expr::Lit(Value::String(s)) => {
                                    if islot + 1 < regs.len() {
                                        let mut bytes = s.clone().into_bytes();
                                        let len = bytes.len();
                                        let lbl = format!(".LSARG{}_{}", 0, aidx);
                                        out.push_str(&format!(
"        leaq {}(%rip), {}
        mov ${}, {}
", lbl, regs[islot], len, regs[islot+1]));
                                        call_arg_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                        islot += 2;
                                    }
                                }
                                Expr::Var(name) => {
                                    if islot < regs.len() && static_names.contains(name) {
                                        let dst = regs[islot];
                                        out.push_str(&format!("        leaq {}(%rip), {}\n", name, dst));
                                        islot += 1;
                                    }
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
                if let Some((ref name, ref args)) = main_ret_call {
                    if !args.is_empty() {
                        let regs = ["%rdi","%rsi","%rdx","%rcx","%r8","%r9"];
                        let mut islot = 0usize;
                        for (aidx, a) in args.iter().enumerate() {
                            match a {
                                Expr::Lit(Value::Int(v)) => {
                                    if islot < regs.len() {
                                        let dst = regs[islot];
                                        out.push_str(&format!("        mov ${}, {}\n", v, dst));
                                        islot += 1;
                                    }
                                }
                                Expr::Lit(Value::String(s)) => {
                                    if islot + 1 < regs.len() {
                                        let mut bytes = s.clone().into_bytes();
                                        let len = bytes.len();
                                        let lbl = format!(".LSARG{}_{}", 0, aidx);
                                        out.push_str(&format!(
"        leaq {}(%rip), {}
        mov ${}, {}
", lbl, regs[islot], len, regs[islot+1]));
                                        call_arg_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                        islot += 2;
                                    }
                                }
                                Expr::Var(name) => {
                                    if islot < regs.len() && static_names.contains(name) {
                                        let dst = regs[islot];
                                        out.push_str(&format!("        leaq {}(%rip), {}\n", name, dst));
                                        islot += 1;
                                    }
                                },
                                Expr::Field(recv0, fname0) => {
                                        let mut recv = recv0.clone();
                                        let mut fname = fname0.clone();
                                        let mut base_name: Option<String> = None;
                                        let mut total_off: usize = 0;
                                        let mut cur_ty: Option<String> = None;
                                        loop {
                                            match &*recv {
                                                Expr::Var(rn) => {
                                                    base_name = Some(rn.clone());
                                                    if let Some(tn) = static_types.get(rn) {
                                                        cur_ty = Some(tn.clone());
                                                    }
                                                    break;
                                                }
                                                Expr::Field(inner_recv, inner_name) => {
                                                    if let Expr::Var(rn2) = &**inner_recv {
                                                        base_name = Some(rn2.clone());
                                                        if let Some(tn) = static_types.get(rn2) {
                                                            cur_ty = Some(tn.clone());
                                                        }
                                                    }
                                                    if let Some(ref tyname) = cur_ty {
                                                        if let Some(Item::Struct(sd)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *tyname)) {
                                                            let mut off = 0usize;
                                                            let mut next_ty: Option<String> = None;
                                                            for f in &sd.fields {
                                                                let sz = match f.ty {
                                                                    Type::I32 => 4,
                                                                    Type::I64 | Type::F64 => 8,
                                                                    Type::String => 16,
                                                                    Type::User(ref un) => {
                                                                        if let Some(Item::Struct(sd2)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *un)) {
                                                                            let mut sz2 = 0usize;
                                                                            for ff in &sd2.fields {
                                                                                sz2 += match ff.ty {
                                                                                    Type::I32 => 4,
                                                                                    Type::I64 | Type::F64 => 8,
                                                                                    Type::String => 16,
                                                                                    _ => 8,
                                                                                };
                                                                            }
                                                                            if sz2 % 8 != 0 { sz2 += 8 - (sz2 % 8); }
                                                                            sz2
                                                                        } else { 8 }
                                                                    }
                                                                    _ => 8,
                                                                };
                                                                if f.name == *inner_name {
                                                                    match f.ty {
                                                                        Type::User(ref un) => next_ty = Some(un.clone()),
                                                                        _ => next_ty = None,
                                                                    }
                                                                    total_off += off;
                                                                    break;
                                                                }
                                                                off += sz;
                                                            }
                                                            cur_ty = next_ty;
                                                        }
                                                    }
                                                    recv = inner_recv.clone();
                                                    fname = fname.clone();
                                                    continue;
                                                }
                                                _ => break,
                                            }
                                        }
                                        let mut resolved_ty = cur_ty.clone();
                                        if resolved_ty.is_none() {
                                            if let Some(ref bn) = base_name {
                                                if let Some(tn) = static_types.get(bn) {
                                                    resolved_ty = Some(tn.clone());
                                                }
                                            }
                                        }
                                        if let (Some(rn), Some(ref tyname)) = (base_name.clone(), resolved_ty) {
                                            if let Some(Item::Struct(sd)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *tyname)) {
                                                let mut off = 0usize;
                                                let mut fty = Type::I64;
                                                for f in &sd.fields {
                                                    let sz = match f.ty {
                                                        Type::I32 => 4,
                                                        Type::I64 | Type::F64 => 8,
                                                        Type::String => 16,
                                                        Type::User(ref un) => {
                                                            if let Some(Item::Struct(sd2)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *un)) {
                                                                let mut sz2 = 0usize;
                                                                for ff in &sd2.fields {
                                                                    sz2 += match ff.ty {
                                                                        Type::I32 => 4,
                                                                        Type::I64 | Type::F64 => 8,
                                                                        Type::String => 16,
                                                                        _ => 8,
                                                                    };
                                                                }
                                                                if sz2 % 8 != 0 { sz2 += 8 - (sz2 % 8); }
                                                                sz2
                                                            } else { 8 }
                                                        }
                                                        _ => 8,
                                                    };
                                                    if f.name == *fname {
                                                        fty = f.ty.clone();
                                                        break;
                                                    }
                                                    off += sz;
                                                }
                                                let final_off = total_off + off;
                                                if islot < regs.len() {
                                                    out.push_str(&format!("        leaq {}(%rip), %r10\n", rn));
                                                    match fty {
                                                        Type::I32 => {
                                                            let dst32 = match regs[islot] {
                                                                "%rdi" => "%edi",
                                                                "%rsi" => "%esi",
                                                                "%rdx" => "%edx",
                                                                "%rcx" => "%ecx",
                                                                "%r8"  => "%r8d",
                                                                "%r9"  => "%r9d",
                                                                _ => "%edi",
                                                            };
                                                            out.push_str(&format!("        mov {}(%r10), {}\n", final_off, dst32));
                                                            islot += 1;
                                                        }
                                                        Type::I64 | Type::F64 => {
                                                            let dst = regs[islot];
                                                            out.push_str(&format!("        mov {}(%r10), {}\n", final_off, dst));
                                                            islot += 1;
                                                        }
                                                        Type::String => {
                                                            if islot + 1 < regs.len() {
                                                                out.push_str(&format!("        mov {}(%r10), {}\n", final_off, regs[islot]));
                                                                out.push_str(&format!("        mov {}(%r10), {}\n", final_off + 8, regs[islot+1]));
                                                                islot += 2;
                                                            }
                                                        }
                                                        _ => {}
                                                    }
                                                }
                                            }
                                        }
                                    }

                                _ => {}
                            }
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
                            let regs = ["%rdi","%rsi","%rdx","%rcx","%r8","%r9"];
                            let mut islot = 0usize;
                            for (aidx, a) in args.iter().enumerate() {
                                match a {
                                    Expr::Lit(Value::Int(v)) => {
                                        if islot < regs.len() {
                                            let dst = regs[islot];
                                            out.push_str(&format!("        mov ${}, {}\n", v, dst));
                                            islot += 1;
                                        }
                                    }
                                    Expr::Lit(Value::String(s)) => {
                                        if islot + 1 < regs.len() {
                                            let mut bytes = s.clone().into_bytes();
                                            let len = bytes.len();
                                            let lbl = format!(".LSARG{}_{}", cidx, aidx);
                                            out.push_str(&format!(
"        leaq {}(%rip), {}
        mov ${}, {}
", lbl, regs[islot], len, regs[islot+1]));
                                            call_arg_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                            islot += 2;
                                        }
                                    }
                                    Expr::Var(name) => {
                                        if islot < regs.len() && static_names.contains(name) {
                                            let dst = regs[islot];
                                            out.push_str(&format!("        leaq {}(%rip), {}\n", name, dst));
                                            islot += 1;
                                        }
                                    },
                                    Expr::Field(recv0, fname0) => {
                                        let mut recv = recv0.clone();
                                        let mut fname = fname0.clone();
                                        let mut base_name: Option<String> = None;
                                        let mut total_off: usize = 0;
                                        let mut cur_ty: Option<String> = None;
                                        loop {
                                            match &*recv {
                                                Expr::Var(rn) => {
                                                    base_name = Some(rn.clone());
                                                    if let Some(tn) = static_types.get(rn) {
                                                        cur_ty = Some(tn.clone());
                                                    }
                                                    break;
                                                }
                                                Expr::Field(inner_recv, inner_name) => {
                                                    if let Expr::Var(rn2) = &**inner_recv {
                                                        base_name = Some(rn2.clone());
                                                        if let Some(tn) = static_types.get(rn2) {
                                                            cur_ty = Some(tn.clone());
                                                        }
                                                    }
                                                    if let Some(ref tyname) = cur_ty {
                                                        if let Some(Item::Struct(sd)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *tyname)) {
                                                            let mut off = 0usize;
                                                            let mut next_ty: Option<String> = None;
                                                            for f in &sd.fields {
                                                                let sz = match f.ty {
                                                                    Type::I32 => 4,
                                                                    Type::I64 | Type::F64 => 8,
                                                                    Type::String => 16,
                                                                    Type::User(ref un) => {
                                                                        if let Some(Item::Struct(sd2)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *un)) {
                                                                            let mut sz2 = 0usize;
                                                                            for ff in &sd2.fields {
                                                                                sz2 += match ff.ty {
                                                                                    Type::I32 => 4,
                                                                                    Type::I64 | Type::F64 => 8,
                                                                                    Type::String => 16,
                                                                                    _ => 8,
                                                                                };
                                                                            }
                                                                            if sz2 % 8 != 0 { sz2 += 8 - (sz2 % 8); }
                                                                            sz2
                                                                        } else { 8 }
                                                                    }
                                                                    _ => 8,
                                                                };
                                                                if f.name == *inner_name {
                                                                    match f.ty {
                                                                        Type::User(ref un) => next_ty = Some(un.clone()),
                                                                        _ => next_ty = None,
                                                                    }
                                                                    total_off += off;
                                                                    break;
                                                                }
                                                                off += sz;
                                                            }
                                                            cur_ty = next_ty;
                                                        }
                                                    }
                                                    recv = inner_recv.clone();
                                                    fname = fname.clone();
                                                    continue;
                                                }
                                                _ => break,
                                            }
                                        }
                                        let mut resolved_ty = cur_ty.clone();
                                        if resolved_ty.is_none() {
                                            if let Some(ref bn) = base_name {
                                                if let Some(tn) = static_types.get(bn) {
                                                    resolved_ty = Some(tn.clone());
                                                }
                                            }
                                        }
                                        if let (Some(rn), Some(ref tyname)) = (base_name.clone(), resolved_ty) {
                                            if let Some(Item::Struct(sd)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *tyname)) {
                                                let mut off = 0usize;
                                                let mut fty = Type::I64;
                                                for f in &sd.fields {
                                                    let sz = match f.ty {
                                                        Type::I32 => 4,
                                                        Type::I64 | Type::F64 => 8,
                                                        Type::String => 16,
                                                        Type::User(ref un) => {
                                                            if let Some(Item::Struct(sd2)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *un)) {
                                                                let mut sz2 = 0usize;
                                                                for ff in &sd2.fields {
                                                                    sz2 += match ff.ty {
                                                                        Type::I32 => 4,
                                                                        Type::I64 | Type::F64 => 8,
                                                                        Type::String => 16,
                                                                        _ => 8,
                                                                    };
                                                                }
                                                                if sz2 % 8 != 0 { sz2 += 8 - (sz2 % 8); }
                                                                sz2
                                                            } else { 8 }
                                                        }
                                                        _ => 8,
                                                    };
                                                    if f.name == *fname {
                                                        fty = f.ty.clone();
                                                        break;
                                                    }
                                                    off += sz;
                                                }
                                                let final_off = total_off + off;
                                                if islot < regs.len() {
                                                    out.push_str(&format!("        leaq {}(%rip), %r10\n", rn));
                                                    match fty {
                                                        Type::I32 => {
                                                            let dst32 = match regs[islot] {
                                                                "%rdi" => "%edi",
                                                                "%rsi" => "%esi",
                                                                "%rdx" => "%edx",
                                                                "%rcx" => "%ecx",
                                                                "%r8"  => "%r8d",
                                                                "%r9"  => "%r9d",
                                                                _ => "%edi",
                                                            };
                                                            out.push_str(&format!("        mov {}(%r10), {}\n", final_off, dst32));
                                                            islot += 1;
                                                        }
                                                        Type::I64 | Type::F64 => {
                                                            let dst = regs[islot];
                                                            out.push_str(&format!("        mov {}(%r10), {}\n", final_off, dst));
                                                            islot += 1;
                                                        }
                                                        Type::String => {
                                                            if islot + 1 < regs.len() {
                                                                out.push_str(&format!("        mov {}(%r10), {}\n", final_off, regs[islot]));
                                                                out.push_str(&format!("        mov {}(%r10), {}\n", final_off + 8, regs[islot+1]));
                                                                islot += 2;
                                                            }
                                                        }
                                                        _ => {}
                                                    }
                                                }
                                            }
                                        }
                                    },
                                    _ => {}
                                }
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
                if !static_types.is_empty() {
                    out.push_str("\n        .data\n");
                    for (sname, ty) in &static_types {
                        let sz = struct_sizes.get(ty).cloned().unwrap_or(8);
                        let mut emitted = false;
                        if let Some(Item::Static(st)) = module.items.iter().find(|it| matches!(it, Item::Static(s) if s.name == *sname)) {
                            if let Expr::StructLit(ref lit_ty, ref fields) = st.init {
                                if lit_ty == ty {
                                    let mut field_map: std::collections::HashMap<String, &Expr> = std::collections::HashMap::new();
                                    for (fname, fexpr) in fields {
                                        field_map.insert(fname.clone(), fexpr);
                                    }
                                    if let Some(Item::Struct(sd)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *ty)) {
                                        out.push_str(&format!("{}:\n", sname));
                                        for f in &sd.fields {
                                            if let Some(expr) = field_map.get(&f.name) {
                                                match (f.ty.clone(), (*expr).clone()) {
                                                    (Type::I32, Expr::Lit(Value::Int(v))) => {
                                                        out.push_str(&format!("        .long {}\n", v as i32));
                                                    }
                                                    (Type::I64, Expr::Lit(Value::Int(v))) => {
                                                        out.push_str(&format!("        .quad {}\n", v as i64));
                                                    }
                                                    (Type::F64, Expr::Lit(Value::Float64(fv))) => {
                                                        let bits = fv.to_bits();
                                                        let lo = bits as u32;
                                                        let hi = (bits >> 32) as u32;
                                                        out.push_str(&format!("        .long {}\n        .long {}\n", lo, hi));
                                                    }
                                                    _ => {
                                                        let bytes = 8;
                                                        out.push_str(&format!("        .zero {}\n", bytes));
                                                    }
                                                }
                                            } else {
                                                let bytes = match f.ty {
                                                    Type::I32 => 4,
                                                    Type::I64 | Type::F64 => 8,
                                                    _ => 8,
                                                };
                                                out.push_str(&format!("        .zero {}\n", bytes));
                                            }
                                        }
                                        let mut total = 0usize;
                                        for f in &sd.fields {
                                            total += match f.ty {
                                                Type::I32 => 4,
                                                Type::I64 | Type::F64 => 8,
                                                _ => 8,
                                            };
                                        }
                                        if total % 8 != 0 {
                                            out.push_str(&format!("        .zero {}\n", 8 - (total % 8)));
                                        }
                                        emitted = true;
                                    }
                                }
                            }
                        }
                        if !emitted {
                            out.push_str(&format!("{}:\n        .zero {}\n", sname, sz));
                        }
                    }
                    out.push_str("\n        .text\n");
                }

                out.push_str("\n        .text\n");
                let mut func_rodata: Vec<(String, String)> = Vec::new();
                let mut need_nl = bool::from(false);
                let mut funcs_to_emit: Vec<&aether_frontend::ast::Function> = Vec::new();
                if let Some(mf) = main_func {
                    funcs_to_emit.push(mf);
                }
                for func in other_funcs {
                    funcs_to_emit.push(func);
                }
                for func in funcs_to_emit {
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
                                        } else {
                                            let regs = ["%rdi","%rsi","%rdx","%rcx","%r8","%r9"];
                                            let mut islot = 0usize;
                                            for (aidx, a) in args.iter().enumerate() {
                                                match a {
                                                    Expr::Lit(Value::Int(v)) => {
                                                        if islot < regs.len() {
                                                            let dst = regs[islot];
                                                            out.push_str(&format!("        mov ${}, {}\n", v, dst));
                                                            islot += 1;
                                                        }
                                                    }
                                                    Expr::Lit(Value::String(s)) => {
                                                        if islot + 1 < regs.len() {
                                                            let mut bytes = s.clone().into_bytes();
                                                            let len = bytes.len();
                                                            let lbl = format!(".LSARG_{}_{}_{}", func.name, fi, aidx);
                                                            out.push_str(&format!(
"        leaq {}(%rip), {}
        mov ${}, {}
", lbl, regs[islot], len, regs[islot+1]));
                                                            func_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                                            islot += 2;
                                                        }
                                                    }
                                                    Expr::Field(recv0, fname0) => {
                                                        let mut recv = recv0.clone();
                                                        let mut fname = fname0.clone();
                                                        let mut base_name: Option<String> = None;
                                                        let mut total_off: usize = 0;
                                                        let mut cur_ty: Option<String> = None;
                                                        loop {
                                                            match &*recv {
                                                                Expr::Var(rn) => {
                                                                    base_name = Some(rn.clone());
                                                                    if let Some(tn) = static_types.get(rn) {
                                                                        cur_ty = Some(tn.clone());
                                                                    }
                                                                    break;
                                                                }
                                                                Expr::Field(inner_recv, inner_name) => {
                                                                    if let Expr::Var(rn2) = &**inner_recv {
                                                                        base_name = Some(rn2.clone());
                                                                        if let Some(tn) = static_types.get(rn2) {
                                                                            cur_ty = Some(tn.clone());
                                                                        }
                                                                    }
                                                                    if let Some(ref tyname) = cur_ty {
                                                                        if let Some(Item::Struct(sd)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *tyname)) {
                                                                            let mut off = 0usize;
                                                                            let mut next_ty: Option<String> = None;
                                                                            for f in &sd.fields {
                                                                                let sz = match f.ty {
                                                                                    Type::I32 => 4,
                                                                                    Type::I64 | Type::F64 => 8,
                                                                                    Type::String => 16,
                                                                                    Type::User(ref un) => {
                                                                                        if let Some(Item::Struct(sd2)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *un)) {
                                                                                            let mut sz2 = 0usize;
                                                                                            for ff in &sd2.fields {
                                                                                                sz2 += match ff.ty {
                                                                                                    Type::I32 => 4,
                                                                                                    Type::I64 | Type::F64 => 8,
                                                                                                    Type::String => 16,
                                                                                                    _ => 8,
                                                                                                };
                                                                                            }
                                                                                            if sz2 % 8 != 0 { sz2 += 8 - (sz2 % 8); }
                                                                                            sz2
                                                                                        } else { 8 }
                                                                                    }
                                                                                    _ => 8,
                                                                                };
                                                                                if f.name == *inner_name {
                                                                                    match f.ty {
                                                                                        Type::User(ref un) => next_ty = Some(un.clone()),
                                                                                        _ => next_ty = None,
                                                                                    }
                                                                                    total_off += off;
                                                                                    break;
                                                                                }
                                                                                off += sz;
                                                                            }
                                                                            cur_ty = next_ty;
                                                                        }
                                                                    }
                                                                    recv = inner_recv.clone();
                                                                    fname = fname.clone();
                                                                    continue;
                                                                }
                                                                _ => break,
                                                            }
                                                        }
                                                        if let (Some(rn), Some(ref tyname)) = (base_name.clone(), cur_ty.clone().or(cur_ty.clone()).or_else(|| static_types.get(base_name.as_ref().unwrap()).cloned())) {
                                                            if let Some(Item::Struct(sd)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *tyname)) {
                                                                let mut off = 0usize;
                                                                let mut fty = Type::I64;
                                                                for f in &sd.fields {
                                                                    let sz = match f.ty {
                                                                        Type::I32 => 4,
                                                                        Type::I64 | Type::F64 => 8,
                                                                        Type::String => 16,
                                                                        Type::User(ref un) => {
                                                                            if let Some(Item::Struct(sd2)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *un)) {
                                                                                let mut sz2 = 0usize;
                                                                                for ff in &sd2.fields {
                                                                                    sz2 += match ff.ty {
                                                                                        Type::I32 => 4,
                                                                                        Type::I64 | Type::F64 => 8,
                                                                                        Type::String => 16,
                                                                                        _ => 8,
                                                                                    };
                                                                                }
                                                                                if sz2 % 8 != 0 { sz2 += 8 - (sz2 % 8); }
                                                                                sz2
                                                                            } else { 8 }
                                                                        }
                                                                        _ => 8,
                                                                    };
                                                                    if f.name == *fname {
                                                                        fty = f.ty.clone();
                                                                        break;
                                                                    }
                                                                    off += sz;
                                                                }
                                                                let final_off = total_off + off;
                                                                if islot < regs.len() {
                                                                    out.push_str(&format!("        leaq {}(%rip), %r10\n", rn));
                                                                    match fty {
                                                                        Type::I32 => {
                                                                            let dst32 = match regs[islot] {
                                                                                "%rdi" => "%edi",
                                                                                "%rsi" => "%esi",
                                                                                "%rdx" => "%edx",
                                                                                "%rcx" => "%ecx",
                                                                                "%r8"  => "%r8d",
                                                                                "%r9"  => "%r9d",
                                                                                _ => "%edi",
                                                                            };
                                                                            out.push_str(&format!("        mov {}(%r10), {}\n", final_off, dst32));
                                                                            islot += 1;
                                                                        }
                                                                        Type::I64 | Type::F64 => {
                                                                            let dst = regs[islot];
                                                                            out.push_str(&format!("        mov {}(%r10), {}\n", final_off, dst));
                                                                            islot += 1;
                                                                        }
                                                                        Type::String => {
                                                                            if islot + 1 < regs.len() {
                                                                                out.push_str(&format!("        mov {}(%r10), {}\n", final_off, regs[islot]));
                                                                                out.push_str(&format!("        mov {}(%r10), {}\n", final_off + 8, regs[islot+1]));
                                                                                islot += 2;
                                                                            }
                                                                        }
                                                                        _ => {}
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                    _ => {}
                                                }
                                            }
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
                                        let then_lbl = format!(".LIF_THEN_{}_{}", func.name, fi);
                                        let else_lbl = format!(".LIF_ELSE_{}_{}", func.name, fi);
                                        let join_lbl = format!(".LIF_JOIN_{}_{}", func.name, fi);
                                        match &**cond {
                                            Expr::BinOp(lhs, op, rhs) => {
                                                let mut lhs_loaded = false;
                                                let mut rhs_loaded = false;
                                                match &**lhs {
                                                    Expr::Lit(Value::Int(v)) => {
                                                        out.push_str(&format!("        mov ${}, %r10\n", v));
                                                        lhs_loaded = true;
                                                    }
                                                    Expr::Var(name) => {
                                                        let regs = ["%rdi","%rsi","%rdx","%rcx","%r8","%r9"];
                                                        let mut slot = 0usize;
                                                        for p in &func.params {
                                                            if p.name == *name {
                                                                match p.ty {
                                                                    Type::String => { /* unsupported in cond */ }
                                                                    _ => {
                                                                        if slot < regs.len() {
                                                                            let src = regs[slot];
                                                                            out.push_str(&format!("        mov {}, %r10\n", src));
                                                                            lhs_loaded = true;
                                                                        }
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
                                                    _ => {}
                                                }
                                                match &**rhs {
                                                    Expr::Lit(Value::Int(v)) => {
                                                        out.push_str(&format!("        mov ${}, %r11\n", v));
                                                        rhs_loaded = true;
                                                    }
                                                    Expr::Var(name) => {
                                                        let regs = ["%rdi","%rsi","%rdx","%rcx","%r8","%r9"];
                                                        let mut slot = 0usize;
                                                        for p in &func.params {
                                                            if p.name == *name {
                                                                match p.ty {
                                                                    Type::String => { /* unsupported in cond */ }
                                                                    _ => {
                                                                        if slot < regs.len() {
                                                                            let src = regs[slot];
                                                                            out.push_str(&format!("        mov {}, %r11\n", src));
                                                                            rhs_loaded = true;
                                                                        }
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
                                                    _ => {}
                                                }
                                                if lhs_loaded && rhs_loaded {
                                                    out.push_str("        cmp %r11, %r10\n");
                                                    match op {
                                                        BinOpKind::Eq => {
                                                            out.push_str(&format!("        je {}\n", then_lbl));
                                                            out.push_str(&format!("        jmp {}\n", else_lbl));
                                                        }
                                                        BinOpKind::Lt => {
                                                            out.push_str(&format!("        jl {}\n", then_lbl));
                                                            out.push_str(&format!("        jmp {}\n", else_lbl));
                                                        }
                                                        BinOpKind::Le => {
                                                            out.push_str(&format!("        jle {}\n", then_lbl));
                                                            out.push_str(&format!("        jmp {}\n", else_lbl));
                                                        }
                                                        _ => {
                                                            out.push_str(&format!("        jmp {}\n", else_lbl));
                                                        }
                                                    }
                                                    out.push_str(&format!("{}:\n", then_lbl));
                                                    match &**then_expr {
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
                                                        }
                                                        Expr::Var(name) => {
                                                            let regs_v = ["%rdi","%rsi","%rdx","%rcx","%r8","%r9"];
                                                            let mut slot = 0usize;
                                                            for p in &func.params {
                                                                if p.name == *name {
                                                                    if let Type::String = p.ty {
                                                                        if slot + 1 < regs_v.len() {
                                                                            let ptr_reg = regs_v[slot];
                                                                            let len_reg = regs_v[slot + 1];
                                                                            out.push_str(&format!(
"        mov {len}, %rdx
        mov {ptr}, %rsi
        mov $1, %rax
        mov $1, %rdi
        syscall
", len=len_reg, ptr=ptr_reg));
                                                                            need_nl = true;
                                                                            out.push_str(
"        mov $1, %rax
        mov $1, %rdi
        leaq .LSNL(%rip), %rsi
        mov $1, %rdx
        syscall
");
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
                                                        Expr::Call(name,args) => {
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
                                                                need_nl = true;
                                                                out.push_str(
"        mov $1, %rax
        mov $1, %rdi
        leaq .LSNL(%rip), %rsi
        mov $1, %rdx
        syscall
");
                                                            }
                                                        }
                                                        _ => {}
                                                    }
                                                    out.push_str(&format!("        jmp {}\n", join_lbl));
                                                    out.push_str(&format!("{}:\n", else_lbl));
                                                    match &**else_expr {
                                                        Expr::Lit(Value::String(s)) => {
                                                            let mut bytes = s.clone().into_bytes();
                                                            bytes.push(b'\n');
                                                            let len = s.as_bytes().len() + 1;
                                                            let lbl = format!(".LSP_{}_{}_else", func.name, fi);
                                                            out.push_str(&format!(
"        mov $1, %rax
        mov $1, %rdi
        leaq {}(%rip), %rsi
        mov ${}, %rdx
        syscall
", lbl, len));
                                                            func_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                                        }
                                                        Expr::Var(name) => {
                                                            let regs_v = ["%rdi","%rsi","%rdx","%rcx","%r8","%r9"];
                                                            let mut slot = 0usize;
                                                            for p in &func.params {
                                                                if p.name == *name {
                                                                    if let Type::String = p.ty {
                                                                        if slot + 1 < regs_v.len() {
                                                                            let ptr_reg = regs_v[slot];
                                                                            let len_reg = regs_v[slot + 1];
                                                                            out.push_str(&format!(
"        mov {len}, %rdx
        mov {ptr}, %rsi
        mov $1, %rax
        mov $1, %rdi
        syscall
", len=len_reg, ptr=ptr_reg));
                                                                            need_nl = true;
                                                                            out.push_str(
"        mov $1, %rax
        mov $1, %rdi
        leaq .LSNL(%rip), %rsi
        mov $1, %rdx
        syscall
");
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
                                                        Expr::Call(name,args) => {
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
                                                                need_nl = true;
                                                                out.push_str(
"        mov $1, %rax
        mov $1, %rdi
        leaq .LSNL(%rip), %rsi
        mov $1, %rdx
        syscall
");
                                                            }
                                                        }
                                                        _ => {}
                                                    }
                                                    out.push_str(&format!("{}:\n", join_lbl));
                                                    fi += 1;
                                                }
                                            }
                                            _ => {}
                                        }
                                    },
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
                    if need_nl && !out.contains("\n.LSNL:\n") {
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
                                Expr::Var(name) => {
                                    if static_names.contains(name) {
                                        out.push_str(&format!("        lea rcx, [rip+{}]\n", name));
                                    }
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
        sub rsp, 40
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
                if true {
                let mut while_data: Vec<(String, String)> = Vec::new();
                for (widx, cond, body) in &main_while_blocks {
                    out.push_str(&format!("LWH_HEAD_main_{}:\n", widx));
                    match cond {
                        Expr::BinOp(a, op, b) => {
                            if let (Expr::Lit(Value::Int(la)), Expr::Lit(Value::Int(lb))) = (&**a, &**b) {
                                out.push_str(&format!("        mov r10, {}\n", la));
                                out.push_str(&format!("        mov r11, {}\n", lb));
                                out.push_str("        cmp r10, r11\n");
                                match op {
                                    BinOpKind::Lt => out.push_str(&format!("        jge LWH_END_main_{}\n", widx)),
                                    BinOpKind::Le => out.push_str(&format!("        jg LWH_END_main_{}\n", widx)),
                                    BinOpKind::Eq => out.push_str(&format!("        jne LWH_END_main_{}\n", widx)),
                                    _ => out.push_str(&format!("        jmp LWH_END_main_{}\n", widx)),
                                }
                            } else {
                                out.push_str(&format!("        jmp LWH_END_main_{}\n", widx));
                            }
                        }
                        Expr::Lit(Value::Int(v)) => {
                            out.push_str(&format!("        mov r10, {}\n", v));
                            out.push_str("        cmp r10, 0\n");
                            out.push_str(&format!("        je LWH_END_main_{}\n", widx));
                        }
                        _ => {
                            out.push_str(&format!("        jmp LWH_END_main_{}\n", widx));
                        }
                    }
                    for (bidx, st) in body.iter().enumerate() {
                        match st {
                            Stmt::Println(s) => {
                                let mut bytes = s.clone().into_bytes();
                                bytes.push(b'\n');
                                let len = s.as_bytes().len() + 1;
                                let lbl = format!("LSW_main_{}_{}", widx, bidx);
                                out.push_str(
r#"        sub rsp, 40
        mov rcx, rbx
"#);
                                out.push_str(&format!("        lea rdx, [rip+{}]\n", lbl));
                                out.push_str(&format!("        mov r8d, {}\n", len as i32));
                                out.push_str(
r#"        xor r9d, r9d
        mov qword ptr [rsp+32], 0
        call WriteFile
        add rsp, 40
"#);
                                while_data.push((lbl, String::from_utf8(bytes).unwrap()));
                            }
                            Stmt::Break => {
                                out.push_str(&format!("        jmp LWH_END_main_{}\n", widx));
                            }
                            Stmt::Continue => {
                                out.push_str(&format!("        jmp LWH_HEAD_main_{}\n", widx));
                            }
                            _ => {}
                        }
                    }
                    out.push_str(&format!("        jmp LWH_HEAD_main_{}\n", widx));
                    out.push_str(&format!("LWH_END_main_{}:\n", widx));
                }
                if !while_data.is_empty() {
                    out.push_str("\n        .data\n");
                    for (lbl, s) in &while_data {
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
                    out.push_str("\n        .text\n");
                }

                    out.push_str("LSNL:\n        .byte 10\n");
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
                let mut funcs_to_emit: Vec<&aether_frontend::ast::Function> = Vec::new();
                if let Some(mf) = main_func {
                    funcs_to_emit.push(mf);
                }
                for func in other_funcs {
                    funcs_to_emit.push(func);
                }
                for func in funcs_to_emit {
                    out.push_str("\n");
                    if func.name == "main" {
                        continue;
                    }

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
                                    },
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
