//! General-purpose, in-source-order code generator for x86_64 Windows.
//!
//! Mirrors `linux_gen` but emits Intel-syntax assembly following the Win64
//! calling convention: the first four parameters go in rcx/rdx/r8/r9 (or the
//! positionally matching xmm0-xmm3 for floats), every call reserves 32 bytes
//! of shadow space, and the stack is kept 16-byte aligned at each call.
//! Printing goes through kernel32 WriteFile with the stdout handle that the
//! module prologue leaves in r12 (callee-saved, preserved everywhere).
//!
//! Functions using constructs not yet covered by the shared analysis in
//! `gen_common` fall back to the legacy Windows emitter in lib.rs.

use std::collections::HashMap;

use aether_frontend::ast::{BinOpKind, Expr, Function, Stmt, Type, UnaryOpKind, Value};

use crate::gen_common::{class_of_type, collect_locals, infer_class, Class, Env, WIN64_LIMITS};
use crate::{win_emit_print_f64_value, win_emit_print_i64};

/// Whether `func` can be fully compiled by this general emitter.
pub(crate) fn can_compile(func: &Function, funcs: &HashMap<String, &Function>) -> bool {
    crate::gen_common::can_compile(func, funcs, WIN64_LIMITS)
}

struct Emitter<'m> {
    env: Env<'m>,
    func_name: String,
    ret_ty: Type,
    /// main returns by exiting the process rather than `ret`.
    is_main: bool,
    locals_size: usize,
    label_counter: usize,
    /// (head_label, end_label) of enclosing while loops, innermost last.
    loop_stack: Vec<(String, String)>,
    rodata: Vec<(String, String)>,
    /// Number of live 8-byte temporaries currently parked on the stack.
    /// At depth 0 the stack is 8 modulo 16 (odd number of pushed qwords
    /// since the 16-aligned call boundary), so calls pad to realign.
    depth: usize,
}

impl<'m> Emitter<'m> {
    fn fresh(&mut self, kind: &str) -> String {
        let l = format!("LG_{}_{}_{}", kind, self.func_name, self.label_counter);
        self.label_counter += 1;
        l
    }

    /// Shadow-space allocation that leaves rsp 16-aligned at the call.
    fn shadow_bytes(&self) -> usize {
        if self.depth % 2 == 0 { 40 } else { 32 }
    }

    fn emit_epilogue(&self, out: &mut String) {
        if self.is_main {
            // Exit the process with the return value in eax.
            out.push_str("        mov ecx, eax\n");
            out.push_str("        sub rsp, 40\n");
            out.push_str("        call ExitProcess\n");
            return;
        }
        if self.locals_size > 0 {
            out.push_str("        mov rsp, rbp\n");
        }
        out.push_str("        pop rbp\n");
        out.push_str("        ret\n");
    }

    /// Evaluate `expr` into rax (Int) or xmm0 (Float). Returns its class.
    fn emit_expr(&mut self, expr: &Expr, out: &mut String) -> Class {
        match expr {
            Expr::Lit(Value::Int(v)) => {
                out.push_str(&format!("        movabs rax, {}\n", v));
                Class::Int
            }
            Expr::Lit(Value::Bool(b)) => {
                out.push_str(&format!("        mov rax, {}\n", if *b { 1 } else { 0 }));
                Class::Int
            }
            Expr::Lit(Value::Float64(f)) => {
                out.push_str(&format!("        movabs rax, {}\n", f.to_bits() as i64));
                out.push_str("        movq xmm0, rax\n");
                Class::Float
            }
            Expr::Lit(Value::Float32(f)) => {
                let d = *f as f64;
                out.push_str(&format!("        movabs rax, {}\n", d.to_bits() as i64));
                out.push_str("        movq xmm0, rax\n");
                Class::Float
            }
            Expr::Var(name) => {
                let off = self.env.offsets[name];
                match self.env.class_of_var(name).unwrap() {
                    Class::Int => {
                        out.push_str(&format!("        mov rax, qword ptr [rbp-{}]\n", off));
                        Class::Int
                    }
                    Class::Float => {
                        out.push_str(&format!("        movsd xmm0, qword ptr [rbp-{}]\n", off));
                        Class::Float
                    }
                }
            }
            Expr::UnaryOp(UnaryOpKind::BitNot, inner) => {
                self.emit_expr(inner, out);
                out.push_str("        not rax\n");
                Class::Int
            }
            Expr::BinOp(a, op, b) => {
                let cls = infer_class(a, &self.env).unwrap();
                match cls {
                    Class::Int => {
                        self.emit_expr(a, out);
                        out.push_str("        push rax\n");
                        self.depth += 1;
                        self.emit_expr(b, out);
                        out.push_str("        mov r11, rax\n");
                        out.push_str("        pop rax\n");
                        self.depth -= 1;
                        match op {
                            BinOpKind::Add => out.push_str("        add rax, r11\n"),
                            BinOpKind::Sub => out.push_str("        sub rax, r11\n"),
                            BinOpKind::Mul => out.push_str("        imul rax, r11\n"),
                            BinOpKind::Div => {
                                out.push_str("        cqo\n");
                                out.push_str("        idiv r11\n");
                            }
                            BinOpKind::BitAnd => out.push_str("        and rax, r11\n"),
                            BinOpKind::BitOr => out.push_str("        or rax, r11\n"),
                            BinOpKind::BitXor => out.push_str("        xor rax, r11\n"),
                            BinOpKind::Shl => {
                                out.push_str("        mov rcx, r11\n");
                                out.push_str("        shl rax, cl\n");
                            }
                            BinOpKind::Shr => {
                                out.push_str("        mov rcx, r11\n");
                                out.push_str("        sar rax, cl\n");
                            }
                            cmp => {
                                out.push_str("        cmp rax, r11\n");
                                let set = match cmp {
                                    BinOpKind::Eq => "sete",
                                    BinOpKind::Lt => "setl",
                                    BinOpKind::Le => "setle",
                                    BinOpKind::Gt => "setg",
                                    _ => "setge",
                                };
                                out.push_str(&format!("        {} al\n", set));
                                out.push_str("        movzx rax, al\n");
                            }
                        }
                        Class::Int
                    }
                    Class::Float => {
                        self.emit_expr(a, out);
                        out.push_str("        sub rsp, 8\n");
                        out.push_str("        movsd qword ptr [rsp], xmm0\n");
                        self.depth += 1;
                        self.emit_expr(b, out);
                        out.push_str("        movapd xmm1, xmm0\n");
                        out.push_str("        movsd xmm0, qword ptr [rsp]\n");
                        out.push_str("        add rsp, 8\n");
                        self.depth -= 1;
                        match op {
                            BinOpKind::Add => {
                                out.push_str("        addsd xmm0, xmm1\n");
                                Class::Float
                            }
                            BinOpKind::Sub => {
                                out.push_str("        subsd xmm0, xmm1\n");
                                Class::Float
                            }
                            BinOpKind::Mul => {
                                out.push_str("        mulsd xmm0, xmm1\n");
                                Class::Float
                            }
                            BinOpKind::Div => {
                                out.push_str("        divsd xmm0, xmm1\n");
                                Class::Float
                            }
                            cmp => {
                                out.push_str("        ucomisd xmm0, xmm1\n");
                                let set = match cmp {
                                    BinOpKind::Eq => "sete",
                                    BinOpKind::Lt => "setb",
                                    BinOpKind::Le => "setbe",
                                    BinOpKind::Gt => "seta",
                                    _ => "setae",
                                };
                                out.push_str(&format!("        {} al\n", set));
                                out.push_str("        movzx rax, al\n");
                                Class::Int
                            }
                        }
                    }
                }
            }
            Expr::Call(name, args) => {
                let callee = self.env.funcs[name.as_str()];
                let ret_cls = class_of_type(&callee.ret);
                // Evaluate all arguments left to right, parking each on the
                // stack, then load them into their positional Win64 registers.
                let int_regs = ["rcx", "rdx", "r8", "r9"];
                let mut classes: Vec<Class> = Vec::new();
                for (p, a) in callee.params.iter().zip(args) {
                    let cls = class_of_type(&p.ty).unwrap();
                    classes.push(cls);
                    self.emit_expr(a, out);
                    out.push_str("        sub rsp, 8\n");
                    self.depth += 1;
                    match cls {
                        Class::Int => out.push_str("        mov qword ptr [rsp], rax\n"),
                        Class::Float => out.push_str("        movsd qword ptr [rsp], xmm0\n"),
                    }
                }
                // Args are on the stack, last argument on top. Registers are
                // positional in the Win64 ABI.
                for (i, cls) in classes.iter().enumerate().rev() {
                    match cls {
                        Class::Int => {
                            out.push_str(&format!(
                                "        mov {}, qword ptr [rsp]\n",
                                int_regs[i]
                            ));
                        }
                        Class::Float => {
                            out.push_str(&format!("        movsd xmm{}, qword ptr [rsp]\n", i));
                        }
                    }
                    out.push_str("        add rsp, 8\n");
                    self.depth -= 1;
                }
                let shadow = self.shadow_bytes();
                out.push_str(&format!("        sub rsp, {}\n", shadow));
                out.push_str(&format!("        call {}\n", name));
                out.push_str(&format!("        add rsp, {}\n", shadow));
                ret_cls.unwrap_or(Class::Int)
            }
            Expr::Cast(inner, ty) => {
                let from = self.emit_expr(inner, out);
                let to = class_of_type(ty).unwrap();
                match (from, to) {
                    (Class::Int, Class::Float) => {
                        out.push_str("        cvtsi2sd xmm0, rax\n");
                    }
                    (Class::Float, Class::Int) => {
                        out.push_str("        cvttsd2si rax, xmm0\n");
                        if matches!(ty, Type::I32 | Type::Bool) {
                            out.push_str("        cdqe\n");
                        }
                    }
                    (Class::Int, Class::Int) => {
                        if matches!(ty, Type::I32) {
                            out.push_str("        cdqe\n");
                        }
                    }
                    (Class::Float, Class::Float) => {}
                }
                to
            }
            Expr::IfElse { cond, then_expr, else_expr } => {
                let cls = infer_class(expr, &self.env).unwrap();
                let else_l = self.fresh("IFE");
                let end_l = self.fresh("IFX");
                self.emit_expr(cond, out);
                out.push_str("        test rax, rax\n");
                out.push_str(&format!("        jz {}\n", else_l));
                self.emit_expr(then_expr, out);
                out.push_str(&format!("        jmp {}\n", end_l));
                out.push_str(&format!("{}:\n", else_l));
                self.emit_expr(else_expr, out);
                out.push_str(&format!("{}:\n", end_l));
                cls
            }
            _ => unreachable!("expr rejected by can_compile"),
        }
    }

    /// Store rax/xmm0 into the slot of `name`, wrapping i32 values.
    fn emit_store_var(&mut self, name: &str, out: &mut String) {
        let off = self.env.offsets[name];
        match self.env.types.get(name).unwrap() {
            Type::F32 | Type::F64 => {
                out.push_str(&format!("        movsd qword ptr [rbp-{}], xmm0\n", off));
            }
            ty => {
                if matches!(ty, Type::I32) {
                    out.push_str("        cdqe\n");
                }
                out.push_str(&format!("        mov qword ptr [rbp-{}], rax\n", off));
            }
        }
    }

    fn emit_stmt(&mut self, stmt: &Stmt, out: &mut String) {
        match stmt {
            Stmt::Let { name, init, .. } => {
                self.emit_expr(init, out);
                self.emit_store_var(name, out);
            }
            Stmt::Assign { target: Expr::Var(name), value } => {
                self.emit_expr(value, out);
                self.emit_store_var(name, out);
            }
            Stmt::Return(e) => {
                let cls = self.emit_expr(e, out);
                if matches!(self.ret_ty, Type::I32) {
                    out.push_str("        cdqe\n");
                }
                if self.is_main && cls == Class::Float {
                    // The process exit status is an integer.
                    out.push_str("        cvttsd2si rax, xmm0\n");
                }
                self.emit_epilogue(out);
            }
            Stmt::Expr(e) => {
                self.emit_expr(e, out);
            }
            Stmt::Println(s) => self.emit_print_str(s, out),
            Stmt::PrintExpr(Expr::Lit(Value::String(s))) => self.emit_print_str(s, out),
            Stmt::PrintExpr(e) => match self.emit_expr(e, out) {
                // The shared print helpers expect rsp = 8 mod 16 (statement
                // level, depth 0) and the stdout handle in r12.
                Class::Int => win_emit_print_i64(out),
                Class::Float => win_emit_print_f64_value(out, ""),
            },
            Stmt::While { cond, body } => {
                let head = self.fresh("WH");
                let end = self.fresh("WE");
                out.push_str(&format!("{}:\n", head));
                self.emit_expr(cond, out);
                out.push_str("        test rax, rax\n");
                out.push_str(&format!("        jz {}\n", end));
                self.loop_stack.push((head.clone(), end.clone()));
                for s in body {
                    self.emit_stmt(s, out);
                }
                self.loop_stack.pop();
                out.push_str(&format!("        jmp {}\n", head));
                out.push_str(&format!("{}:\n", end));
            }
            Stmt::Break => {
                if let Some((_, end)) = self.loop_stack.last() {
                    out.push_str(&format!("        jmp {}\n", end));
                }
            }
            Stmt::Continue => {
                if let Some((head, _)) = self.loop_stack.last() {
                    out.push_str(&format!("        jmp {}\n", head));
                }
            }
            _ => unreachable!("stmt rejected by can_compile"),
        }
    }

    fn emit_print_str(&mut self, s: &str, out: &mut String) {
        let lbl = self.fresh("STR");
        let mut text = s.to_string();
        text.push('\n');
        let len = text.as_bytes().len();
        self.rodata.push((lbl.clone(), text));
        let shadow = self.shadow_bytes();
        out.push_str(&format!("        sub rsp, {}\n", shadow));
        out.push_str("        mov rcx, r12\n");
        out.push_str(&format!("        lea rdx, [rip+{}]\n", lbl));
        out.push_str(&format!("        mov r8d, {}\n", len));
        out.push_str("        xor r9d, r9d\n");
        out.push_str("        mov qword ptr [rsp+32], 0\n");
        out.push_str("        call WriteFile\n");
        out.push_str(&format!("        add rsp, {}\n", shadow));
    }
}

fn escape_ascii(s: &str) -> String {
    let mut r = String::new();
    for b in s.bytes() {
        match b {
            b'\\' => r.push_str("\\\\"),
            b'"' => r.push_str("\\\""),
            b'\n' => r.push_str("\\n"),
            b'\t' => r.push_str("\\t"),
            0x20..=0x7e => r.push(b as char),
            _ => r.push_str(&format!("\\{:03o}", b)),
        }
    }
    r
}

fn emit_rodata_inline(rodata: &[(String, String)], out: &mut String) {
    if rodata.is_empty() {
        return;
    }
    out.push_str("\n        .data\n");
    for (lbl, s) in rodata {
        out.push_str(&format!("{}:\n        .ascii \"{}\"\n", lbl, escape_ascii(s)));
    }
    out.push_str("        .text\n");
}

/// Emit `func` in full (label, prologue, body, epilogue) plus its string data.
pub(crate) fn emit(
    func: &Function,
    funcs: &HashMap<String, &Function>,
    out: &mut String,
    label_counter: &mut usize,
) {
    emit_inner(func, funcs, out, label_counter, false);
}

/// Emit the body of `main` (after the module prologue that set up rbp,
/// saved rbx/r12/r13, and left the stdout handle in r12): identical to
/// `emit` except that returning exits the process via ExitProcess and local
/// slots start below the three saved registers.
pub(crate) fn emit_main(
    func: &Function,
    funcs: &HashMap<String, &Function>,
    out: &mut String,
    label_counter: &mut usize,
) {
    emit_inner(func, funcs, out, label_counter, true);
}

fn emit_inner(
    func: &Function,
    funcs: &HashMap<String, &Function>,
    out: &mut String,
    label_counter: &mut usize,
    is_main: bool,
) {
    let mut env =
        Env { funcs, types: HashMap::new(), offsets: HashMap::new(), limits: WIN64_LIMITS };
    // In main, rbx/r12/r13 saved by the module prologue occupy [rbp-8..-24].
    let mut cur_off = if is_main { 24usize } else { 0usize };

    let int_regs = ["rcx", "rdx", "r8", "r9"];
    let mut spills: Vec<(usize, String)> = Vec::new();
    for (i, p) in func.params.iter().enumerate() {
        cur_off += 8;
        env.types.insert(p.name.clone(), p.ty.clone());
        env.offsets.insert(p.name.clone(), cur_off);
        match class_of_type(&p.ty).unwrap() {
            Class::Int => spills.push((cur_off, int_regs[i].to_string())),
            Class::Float => spills.push((cur_off, format!("xmm{}", i))),
        }
    }
    collect_locals(&func.body, &mut env, &mut cur_off);

    let base = if is_main { 24 } else { 0 };
    let mut locals_size = cur_off - base;
    if locals_size % 16 != 0 {
        locals_size += 16 - (locals_size % 16);
    }

    if !is_main {
        out.push_str(&format!("{}:\n", func.name));
        out.push_str("        push rbp\n");
        out.push_str("        mov rbp, rsp\n");
        // Extra 8 bytes so rsp = 8 mod 16 at statement level, matching the
        // main body and the parity the shared print helpers align against.
        locals_size += 8;
    }
    if locals_size > 0 {
        out.push_str(&format!("        sub rsp, {}\n", locals_size));
    }
    for (off, reg) in &spills {
        if reg.starts_with("xmm") {
            out.push_str(&format!("        movsd qword ptr [rbp-{}], {}\n", off, reg));
        } else {
            out.push_str(&format!("        mov qword ptr [rbp-{}], {}\n", off, reg));
        }
    }

    let mut em = Emitter {
        env,
        func_name: func.name.clone(),
        ret_ty: func.ret.clone(),
        is_main,
        locals_size,
        label_counter: *label_counter,
        loop_stack: Vec::new(),
        rodata: Vec::new(),
        depth: 0,
    };
    for stmt in &func.body {
        em.emit_stmt(stmt, out);
    }
    // Implicit return 0 if control falls off the end.
    out.push_str("        xor eax, eax\n");
    em.emit_epilogue(out);

    *label_counter = em.label_counter;
    emit_rodata_inline(&em.rodata, out);
}
