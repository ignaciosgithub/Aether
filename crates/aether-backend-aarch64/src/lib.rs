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
                BinOpKind::Gt => Some(if lv > rv { 1 } else { 0 }),
                BinOpKind::Ge => Some(if lv >= rv { 1 } else { 0 }),
                BinOpKind::BitAnd => Some(lv & rv),
                BinOpKind::BitOr => Some(lv | rv),
                BinOpKind::BitXor => Some(lv ^ rv),
                BinOpKind::Shl => Some(lv << rv),
                BinOpKind::Shr => Some(lv >> rv),
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
                BinOpKind::Eq | BinOpKind::Lt | BinOpKind::Le | BinOpKind::Gt | BinOpKind::Ge => None,
                BinOpKind::BitAnd | BinOpKind::BitOr | BinOpKind::BitXor | BinOpKind::Shl | BinOpKind::Shr => None,
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
        let mut out = String::new();

        let mut exit_code: i64 = 0;
        let mut f64_ret: Option<f64> = None;
        let mut prints: Vec<(String, usize)> = Vec::new();
        let mut calls: Vec<(String, Vec<Expr>)> = Vec::new();
        let mut main_ret_call: Option<(String, Vec<Expr>)> = None;
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
        let mut main_field_prints: Vec<(String, usize)> = Vec::new();
        let mut main_print_calls: Vec<(String, Vec<Expr>)> = Vec::new();
        let mut spawn_sites: Vec<(String, String, Option<i64>)> = Vec::new();
        let mut join_sites: Vec<(String, String)> = Vec::new();
        let mut destroy_sites: Vec<(String, String)> = Vec::new();

        for item in &module.items {
            let func = match item {
                Item::Function(f) => f,
                _ => continue,
            };
            if func.name == "main" {
                let mut local_strings: std::collections::HashMap<String, std::collections::HashMap<String, String>> = std::collections::HashMap::new();
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
                        Stmt::Let { name, ty, init } => {
                            if let aether_frontend::ast::Type::User(_) = ty {
                                if let Expr::StructLit(_, fields) = init {
                                    let mut fmap: std::collections::HashMap<String, String> = std::collections::HashMap::new();
                                    for (fname, fexpr) in fields {
                                        if let Expr::Lit(Value::String(sv)) = fexpr {
                                            fmap.insert(fname.clone(), sv.clone());
                                        }
                                    }
                                    if !fmap.is_empty() {
                                        local_strings.insert(name.clone(), fmap);
                                    }
                                }

                                if let Expr::Call(cname, cargs) = init {
                                    if cname == "spawn" {
                                        if cargs.len() == 2 {
                                            if let Expr::Lit(Value::String(fname)) = &cargs[0] {
                                                let mut arg_i: Option<i64> = None;
                                                if let Expr::Lit(Value::Int(v)) = &cargs[1] {
                                                    arg_i = Some(*v);
                                                }
                                                spawn_sites.push((name.clone(), fname.clone(), arg_i));
                                            }
                                        }
                                    } else if cname == "join" {
                                        if cargs.len() == 1 {
                                            if let Expr::Var(hn) = &cargs[0] {
                                                join_sites.push((name.clone(), hn.clone()));
                                            }
                                        }
                                    } else if cname == "destroy" {
                                        if cargs.len() == 1 {
                                            if let Expr::Var(hn) = &cargs[0] {
                                                destroy_sites.push((name.clone(), hn.clone()));
                                            }
                                        }
                                    }
                                }

                            }
                        }
                        Stmt::Assign { target, value } => {
                            if let Expr::Field(recv, fname) = target {
                                if let Expr::Var(rn) = &**recv {
                                    if let Expr::Lit(Value::String(sv)) = value {
                                        if let Some(map) = local_strings.get_mut(rn) {
                                            map.insert(fname.clone(), sv.clone());
                                        } else {
                                            let mut fmap: std::collections::HashMap<String, String> = std::collections::HashMap::new();
                                            fmap.insert(fname.clone(), sv.clone());
                                            local_strings.insert(rn.clone(), fmap);
                                        }
                                    }
                                }
                            }
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
                            } else if let Expr::Field(recv_any, fname_any) = e {
                                if let Expr::Var(rn) = &**recv_any {
                                    if let Some(map) = local_strings.get(rn) {
                                        if let Some(s) = map.get(fname_any) {
                                            let mut bytes = s.clone().into_bytes();
                                            bytes.push(b'\n');
                                            prints.push((String::from_utf8(bytes).unwrap(), s.as_bytes().len() + 1));
                                        }
                                    }
                                }
                            }
                        }
                        Stmt::PrintExpr(Expr::Field(recv0, fname0)) => {
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
                            if let (Some(rn), Some(ref tyname)) = (base_name.clone(), cur_ty.clone().or_else(|| base_name.as_ref().and_then(|bn| static_types.get(bn)).cloned())) {
                                if let Some(Item::Struct(sd)) = module.items.iter().find(|it| matches!(it, Item::Struct(s) if s.name == *tyname)) {
                                    let mut off = 0usize;
                                    let mut is_string = false;
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
                                            if let Type::String = f.ty {
                                                is_string = true;
                                            }
                                            break;
                                        }
                                        off += sz;
                                    }
                                    if is_string {
                                        let final_off = total_off + off;
                                        main_field_prints.push((rn.clone(), final_off));
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
        out.push_str(
r#"
        .global _start
        .text
_start:
"#);
        let mut main_while_blocks: Vec<(usize, Expr, Vec<Stmt>)> = Vec::new();
        if let Some(func) = module.items.iter().find_map(|it| if let Item::Function(f)=it { if f.name=="main" { Some(f) } else { None } } else { None }) {
            let mut widx = 0usize;
            for stmt in &func.body {
                if let Stmt::While { cond, body } = stmt.clone() {
                    main_while_blocks.push((widx, cond, body));
                    widx += 1;
                }
            }
        }
        let mut while_rodata: Vec<(String, String)> = Vec::new();
        for (widx, cond, body) in &main_while_blocks {
            out.push_str(&format!(".LWH_HEAD_main_{}:\n", widx));
            match cond {
                Expr::BinOp(a, op, b) => {
                    if let (Expr::Lit(Value::Int(la)), Expr::Lit(Value::Int(lb))) = (&**a, &**b) {
                        out.push_str(&format!("        mov x10, #{}\n", la));
                        out.push_str(&format!("        mov x11, #{}\n", lb));
                        out.push_str("        cmp x10, x11\n");
                        match op {
                            BinOpKind::Lt => out.push_str(&format!("        b.ge .LWH_END_main_{}\n", widx)),
                            BinOpKind::Le => out.push_str(&format!("        b.gt .LWH_END_main_{}\n", widx)),
                            BinOpKind::Eq => out.push_str(&format!("        b.ne .LWH_END_main_{}\n", widx)),
                            _ => out.push_str(&format!("        b .LWH_END_main_{}\n", widx)),
                        }
                    } else {
                        out.push_str(&format!("        b .LWH_END_main_{}\n", widx));
                    }
                }
                Expr::Lit(Value::Int(v)) => {
                    out.push_str(&format!("        mov x10, #{}\n", v));
                    out.push_str("        cmp x10, #0\n");
                    out.push_str(&format!("        b.eq .LWH_END_main_{}\n", widx));
                }
                _ => {
                    out.push_str(&format!("        b .LWH_END_main_{}\n", widx));
                }
            }
            for (bidx, st) in body.iter().enumerate() {
                match st {
                    Stmt::Println(s) => {
                        let mut bytes = s.clone().into_bytes();
                        bytes.push(b'\n');
                        let lbl = format!(".LSW_main_{}_{}", widx, bidx);
                        while_rodata.push((lbl.clone(), String::from_utf8(bytes).unwrap()));
                        out.push_str("        mov x0, #1\n");
                        out.push_str("        mov x8, #64\n");
                        out.push_str(&format!("        adrp x1, {}\n", lbl));
                        out.push_str(&format!("        add x1, x1, :lo12:{}\n", lbl));
                        out.push_str(&format!("        mov x2, #{}\n", s.as_bytes().len() + 1));
                        out.push_str("        svc #0\n");
                    }
                    Stmt::Break => out.push_str(&format!("        b .LWH_END_main_{}\n", widx)),
                    Stmt::Continue => out.push_str(&format!("        b .LWH_HEAD_main_{}\n", widx)),
                    _ => {}
                }
            }
            out.push_str(&format!("        b .LWH_HEAD_main_{}\n", widx));
            out.push_str(&format!(".LWH_END_main_{}:\n", widx));
        }
        if !while_rodata.is_empty() {
            out.push_str("\n        .section .rodata\n");
            for (lbl, s) in &while_rodata {
                out.push_str(&format!("{}:\n", lbl));
                out.push_str(&format!("        .ascii \"{}\"\n", s.replace("\\", "\\\\").replace("\"", "\\\"")));
            }
        }
        if !spawn_sites.is_empty() || !join_sites.is_empty() || !destroy_sites.is_empty() {
            out.push_str("\n        .bss\n");
            for (sidx, _) in spawn_sites.iter().enumerate() {
                out.push_str(&format!("TSTACK{}:\n        .zero 65536\n", sidx));
                out.push_str(&format!("THANDLE{}:\n        .quad 0\n", sidx));
            }
            for (jidx, _) in join_sites.iter().enumerate() {
                out.push_str(&format!("TRESJ{}:\n        .long 0\n        .long 0\n", jidx));
            }
            for (didx, _) in destroy_sites.iter().enumerate() {
                out.push_str(&format!("TRESD{}:\n        .long 0\n        .long 0\n", didx));
            }
            out.push_str("\n        .text\n");
            for (sidx, (_hname, fname, arg_opt)) in spawn_sites.iter().enumerate() {
                out.push_str(&format!(
"        adrp x1, TSTACK{0}
        add x1, x1, :lo12:TSTACK{0}
        add x1, x1, #65536
        mov x0, #17
        mov x2, xzr
        mov x3, xzr
        mov x4, xzr
        mov x8, #220
        svc #0
        cbnz x0, .LPARENT_{0}
        ", sidx));
                if let Some(arg) = arg_opt {
                    out.push_str(&format!("        mov x0, #{0}\n", *arg));
                } else {
                    out.push_str("        mov x0, #0\n");
                }
                out.push_str(&format!(
"        bl {0}
        mov x8, #93
        svc #0
.LPARENT_{1}:
        adrp x10, THANDLE{1}
        add x10, x10, :lo12:THANDLE{1}
        str x0, [x10]
", fname, sidx));
            }
            if !join_sites.is_empty() {
                for (jidx, (_rname, hname)) in join_sites.iter().enumerate() {
                    let mut found = None;
                    for (sidx, (hvar, _fnm, _)) in spawn_sites.iter().enumerate() {
                        if hvar == hname { found = Some(sidx); break; }
                    }
                    if let Some(sidx) = found {
                        out.push_str(
"        sub sp, sp, #16
");
                        out.push_str(&format!(
"        adrp x10, THANDLE{0}
        add x10, x10, :lo12:THANDLE{0}
        ldr x0, [x10]
        mov x8, #260
        add x1, sp, #0
        mov x2, xzr
        mov x3, xzr
        svc #0
        ldr w0, [sp]
        lsr w0, w0, #8
        and w0, w0, #0xff
        add sp, sp, #16
", sidx));
                        out.push_str(&format!(
"        adrp x11, TRESJ{0}
        add x11, x11, :lo12:TRESJ{0}
        str w0, [x11]
", jidx));
                    }
                }
            }
            if !destroy_sites.is_empty() {
                for (didx, (_rname, hname)) in destroy_sites.iter().enumerate() {
                    let mut found = None;
                    for (sidx, (hvar, _fnm, _)) in spawn_sites.iter().enumerate() {
                        if hvar == hname { found = Some(sidx); break; }
                    }
                    if let Some(sidx) = found {
                        out.push_str(&format!(
"        adrp x10, THANDLE{0}
        add x10, x10, :lo12:THANDLE{0}
        ldr x0, [x10]
        mov x1, #9
        mov x8, #129
        svc #0
        cmp x0, #0
        cset w0, eq
", sidx));
                        out.push_str(
"        sub sp, sp, #16
");
                        out.push_str(&format!(
"        adrp x10, THANDLE{0}
        add x10, x10, :lo12:THANDLE{0}
        ldr x0, [x10]
        mov x8, #260
        add x1, sp, #0
        mov x2, xzr
        mov x3, xzr
        svc #0
        add sp, sp, #16
", sidx));
                        out.push_str(&format!(
"        adrp x11, TRESD{0}
        add x11, x11, :lo12:TRESD{0}
        str w0, [x11]
", didx));
                    }
                }
            }
        }

            out.push_str("\n        .text\n");
        if !static_types.is_empty() {
            out.push_str("\n        .data\n");
            for (sname, ty) in &static_types {
                let sz = struct_sizes.get(ty).cloned().unwrap_or(8);
                out.push_str(&format!("{}:\n        .zero {}\n", sname, sz));
            }
            out.push_str("\n        .text\n");
        }

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
            } else {
                let regs = ["x0","x1","x2","x3","x4","x5","x6","x7"];
                let mut islot = 0usize;
                for a in args {
                    match a {
                        Expr::Lit(Value::Int(v)) => {
                            if islot < regs.len() {
                                let dst = regs[islot];
                                out.push_str(&format!("        mov {}, #{}\n", dst, v));
                                islot += 1;
                            }
                        }
                        Expr::Var(name) => {
                            if islot < regs.len() && static_names.contains(name) {
                                let dst = regs[islot];
                                out.push_str(&format!("        adrp {0}, {1}\n        add {0}, {0}, :lo12:{1}\n", dst, name));
                                islot += 1;
                            }
                        }
                        _ => {}
                    }
                }
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
                let regs = ["x0","x1","x2","x3","x4","x5","x6","x7"];
                let mut islot = 0usize;
                for (aidx, a) in args.iter().enumerate() {
                    match a {
                        Expr::Lit(Value::Int(v)) => {
                            if islot < regs.len() {
                                let dst = regs[islot];
                                out.push_str(&format!("        mov {}, #{}\n", dst, v));
                                islot += 1;
                            }
                        }
                        Expr::Lit(Value::String(s)) => {
                            if islot + 1 < regs.len() {
                                let mut bytes = s.clone().into_bytes();
                                let len = bytes.len();
                                let lbl = format!(".LSARG{}_{}", 0, aidx);
                                out.push_str(&format!(
"        adrp {0}, {1}
        add {0}, {0}, :lo12:{1}
        mov {2}, #{3}
", regs[islot], lbl, regs[islot+1], len));
                                call_arg_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                islot += 2;
                            }
                        }
                        Expr::Var(name) => {
                            if islot < regs.len() && static_names.contains(name) {
                                let dst = regs[islot];
                                out.push_str(&format!("        adrp {0}, {1}\n        add {0}, {0}, :lo12:{1}\n", dst, name));
                                islot += 1;
                            }
                        }
                        _ => {}
                    }
                }
            }
            out.push_str(&format!("        bl {}\n", name));
        } else {
            for (cidx, (name, args)) in calls.iter().enumerate() {
                if !args.is_empty() {
                    let regs = ["x0","x1","x2","x3","x4","x5","x6","x7"];
                    let mut islot = 0usize;
                    for (aidx, a) in args.iter().enumerate() {
                        match a {
                            Expr::Lit(Value::Int(v)) => {
                                if islot < regs.len() {
                                    let dst = regs[islot];
                                    out.push_str(&format!("        mov {}, #{}\n", dst, v));
                                    islot += 1;
                                }
                            }
                            Expr::Lit(Value::String(s)) => {
                                if islot + 1 < regs.len() {
                                    let mut bytes = s.clone().into_bytes();
                                    let len = bytes.len();
                                    let lbl = format!(".LSARG{}_{}", cidx, aidx);
                                    out.push_str(&format!(
"        adrp {0}, {1}
        add {0}, {0}, :lo12:{1}
        mov {2}, #{3}
", regs[islot], lbl, regs[islot+1], len));
                                    call_arg_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                    islot += 2;
                                }
                            }
                            Expr::Var(name) => {
                                if islot < regs.len() && static_names.contains(name) {
                                    let dst = regs[islot];
                                    out.push_str(&format!("        adrp {0}, {1}\n        add {0}, {0}, :lo12:{1}\n", dst, name));
                                    islot += 1;
                                }
                            }
                            _ => {}
                        }
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
            if !main_print_calls.is_empty() || !main_field_prints.is_empty() {
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
                                let then_lbl = format!(".LIF_THEN_{}_{}", func.name, fi);
                                let else_lbl = format!(".LIF_ELSE_{}_{}", func.name, fi);
                                let join_lbl = format!(".LIF_JOIN_{}_{}", func.name, fi);
                                match &**cond {
                                    Expr::BinOp(lhs, op, rhs) => {
                                        let mut lhs_loaded = false;
                                        let mut rhs_loaded = false;
                                        match &**lhs {
                                            Expr::Lit(Value::Int(v)) => {
                                                out.push_str(&format!("        mov x10, #{}\n", v));
                                                lhs_loaded = true;
                                            }
                                            Expr::Var(name) => {
                                                let regs = ["x0","x1","x2","x3","x4","x5","x6","x7"];
                                                let mut slot = 0usize;
                                                for p in &func.params {
                                                    if p.name == *name {
                                                        match p.ty {
                                                            Type::String => {}
                                                            _ => {
                                                                if slot < regs.len() {
                                                                    let src = regs[slot];
                                                                    out.push_str(&format!("        mov x10, {}\n", src));
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
                                                out.push_str(&format!("        mov x11, #{}\n", v));
                                                rhs_loaded = true;
                                            }
                                            Expr::Var(name) => {
                                                let regs = ["x0","x1","x2","x3","x4","x5","x6","x7"];
                                                let mut slot = 0usize;
                                                for p in &func.params {
                                                    if p.name == *name {
                                                        match p.ty {
                                                            Type::String => {}
                                                            _ => {
                                                                if slot < regs.len() {
                                                                    let src = regs[slot];
                                                                    out.push_str(&format!("        mov x11, {}\n", src));
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
                                            out.push_str("        cmp x10, x11\n");
                                            match op {
                                                BinOpKind::Eq => {
                                                    out.push_str(&format!("        b.eq {}\n", then_lbl));
                                                    out.push_str(&format!("        b {}\n", else_lbl));
                                                }
                                                BinOpKind::Lt => {
                                                    out.push_str(&format!("        b.lt {}\n", then_lbl));
                                                    out.push_str(&format!("        b {}\n", else_lbl));
                                                }
                                                BinOpKind::Le => {
                                                    out.push_str(&format!("        b.le {}\n", then_lbl));
                                                    out.push_str(&format!("        b {}\n", else_lbl));
                                                }
                                                _ => {
                                                    out.push_str(&format!("        b {}\n", else_lbl));
                                                }
                                            }
                                            out.push_str(&format!("{}:\n", then_lbl));
                                            match &**then_expr {
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
                                                }
                                                Expr::Var(name) => {
                                                    let regs_v = ["x0","x1","x2","x3","x4","x5","x6","x7"];
                                                    let mut slot = 0usize;
                                                    for p in &func.params {
                                                        if p.name == *name {
                                                            if let Type::String = p.ty {
                                                                if slot + 1 < regs_v.len() {
                                                                    let ptr_reg = regs_v[slot];
                                                                    let len_reg = regs_v[slot + 1];
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
                                                Expr::Call(name,args) => {
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
                                            out.push_str(&format!("        b {}\n", join_lbl));
                                            out.push_str(&format!("{}:\n", else_lbl));
                                            match &**else_expr {
                                                Expr::Lit(Value::String(s)) => {
                                                    let mut bytes = s.clone().into_bytes();
                                                    bytes.push(b'\n');
                                                    let len = s.as_bytes().len() + 1;
                                                    let lbl = format!(".LSF_{}_{}_else", func.name, fi);
                                                    out.push_str(&format!(
"        mov x8, #64
        mov x0, #1
        adrp x1, {0}
        add x1, x1, :lo12:{0}
        mov x2, #{1}
        svc #0
", lbl, len));
                                                    func_rodata.push((lbl, String::from_utf8(bytes).unwrap()));
                                                }
                                                Expr::Var(name) => {
                                                    let regs_v = ["x0","x1","x2","x3","x4","x5","x6","x7"];
                                                    let mut slot = 0usize;
                                                    for p in &func.params {
                                                        if p.name == *name {
                                                            if let Type::String = p.ty {
                                                                if slot + 1 < regs_v.len() {
                                                                    let ptr_reg = regs_v[slot];
                                                                    let len_reg = regs_v[slot + 1];
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
                                                Expr::Call(name,args) => {
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
                                            out.push_str(&format!("{}:\n", join_lbl));
                                            fi += 1;
                                        }
                                    }
                                    _ => {}
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
