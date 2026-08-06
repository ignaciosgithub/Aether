//! General-purpose, in-source-order code generator for x86_64 Linux.
//!
//! Emits any function whose body is composed of supported statements and
//! expressions by walking the AST directly: an environment maps every
//! parameter and local to a stack slot, expressions are evaluated
//! recursively into %rax (integers) or %xmm0 (floats), and statements are
//! emitted strictly in the order they appear in the source.
//!
//! Functions using constructs not yet covered here (structs, vectors,
//! heterogeneous lists, string manipulation, threads, try/except) are
//! reported as unsupported by `can_compile` and handled by the legacy
//! emitter in lib.rs. Coverage grows here over time; new language features
//! should be added to this module, not as pattern-matched special cases.

use std::collections::HashMap;

use aether_frontend::ast::{BinOpKind, Expr, Function, Stmt, Type, UnaryOpKind, Value};

use crate::{linux_emit_print_f64_value, linux_emit_print_i64};

/// Value class an expression evaluates to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Class {
    Int,
    Float,
}

fn class_of_type(ty: &Type) -> Option<Class> {
    match ty {
        Type::Bool | Type::I32 | Type::I64 => Some(Class::Int),
        Type::F32 | Type::F64 => Some(Class::Float),
        _ => None,
    }
}

struct Env<'m> {
    funcs: &'m HashMap<String, &'m Function>,
    types: HashMap<String, Type>,
    offsets: HashMap<String, usize>,
}

impl<'m> Env<'m> {
    fn class_of_var(&self, name: &str) -> Option<Class> {
        self.types.get(name).and_then(class_of_type)
    }
}

fn infer_class(expr: &Expr, env: &Env) -> Option<Class> {
    match expr {
        Expr::Lit(Value::Int(_)) | Expr::Lit(Value::Bool(_)) => Some(Class::Int),
        Expr::Lit(Value::Float64(_)) | Expr::Lit(Value::Float32(_)) => Some(Class::Float),
        Expr::Var(name) => env.class_of_var(name),
        Expr::UnaryOp(UnaryOpKind::BitNot, inner) => {
            (infer_class(inner, env)? == Class::Int).then_some(Class::Int)
        }
        Expr::BinOp(a, op, b) => {
            let ca = infer_class(a, env)?;
            let cb = infer_class(b, env)?;
            if ca != cb {
                return None;
            }
            match op {
                BinOpKind::Eq
                | BinOpKind::Lt
                | BinOpKind::Le
                | BinOpKind::Gt
                | BinOpKind::Ge => Some(Class::Int),
                BinOpKind::BitAnd
                | BinOpKind::BitOr
                | BinOpKind::BitXor
                | BinOpKind::Shl
                | BinOpKind::Shr => (ca == Class::Int).then_some(Class::Int),
                BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div => Some(ca),
            }
        }
        Expr::Call(name, _) => {
            let callee = env.funcs.get(name)?;
            class_of_type(&callee.ret)
        }
        Expr::Cast(_, ty) => class_of_type(ty),
        Expr::IfElse { cond, then_expr, else_expr } => {
            if infer_class(cond, env)? != Class::Int {
                return None;
            }
            let ct = infer_class(then_expr, env)?;
            let ce = infer_class(else_expr, env)?;
            (ct == ce).then_some(ct)
        }
        _ => None,
    }
}

fn expr_supported(expr: &Expr, env: &Env) -> bool {
    match expr {
        Expr::Lit(Value::Int(_))
        | Expr::Lit(Value::Bool(_))
        | Expr::Lit(Value::Float64(_))
        | Expr::Lit(Value::Float32(_)) => true,
        Expr::Var(name) => env.class_of_var(name).is_some(),
        Expr::UnaryOp(UnaryOpKind::BitNot, inner) => {
            expr_supported(inner, env) && infer_class(inner, env) == Some(Class::Int)
        }
        Expr::BinOp(a, _, b) => {
            expr_supported(a, env) && expr_supported(b, env) && infer_class(expr, env).is_some()
        }
        Expr::Call(name, args) => {
            let Some(callee) = env.funcs.get(name) else { return false };
            if callee.params.len() != args.len() {
                return false;
            }
            if class_of_type(&callee.ret).is_none() && !matches!(callee.ret, Type::Void) {
                return false;
            }
            // f32 in call signatures is excluded: this emitter models floats as
            // f64 while the legacy emitter passes f32 differently.
            if matches!(callee.ret, Type::F32) {
                return false;
            }
            let mut ints = 0usize;
            let mut floats = 0usize;
            for (p, a) in callee.params.iter().zip(args) {
                if matches!(p.ty, Type::F32) {
                    return false;
                }
                let Some(pc) = class_of_type(&p.ty) else { return false };
                match pc {
                    Class::Int => ints += 1,
                    Class::Float => floats += 1,
                }
                if !expr_supported(a, env) || infer_class(a, env) != Some(pc) {
                    return false;
                }
            }
            ints <= 6 && floats <= 8
        }
        Expr::Cast(inner, ty) => {
            class_of_type(ty).is_some()
                && expr_supported(inner, env)
                && infer_class(inner, env).is_some()
        }
        Expr::IfElse { cond, then_expr, else_expr } => {
            expr_supported(cond, env)
                && expr_supported(then_expr, env)
                && expr_supported(else_expr, env)
                && infer_class(expr, env).is_some()
        }
        _ => false,
    }
}

fn stmt_supported(stmt: &Stmt, env: &mut Env) -> bool {
    match stmt {
        Stmt::Return(e) => expr_supported(e, env),
        Stmt::Expr(e) => expr_supported(e, env),
        Stmt::Println(_) => true,
        Stmt::PrintExpr(Expr::Lit(Value::String(_))) => true,
        Stmt::PrintExpr(e) => expr_supported(e, env),
        Stmt::While { cond, body } => {
            expr_supported(cond, env)
                && infer_class(cond, env) == Some(Class::Int)
                && body.iter().all(|s| stmt_supported(s, env))
        }
        Stmt::Break | Stmt::Continue => true,
        Stmt::Let { name, ty, init } => {
            let Some(cls) = class_of_type(ty) else { return false };
            if !expr_supported(init, env) || infer_class(init, env) != Some(cls) {
                return false;
            }
            // Register so later statements can reference it during the check.
            env.types.insert(name.clone(), ty.clone());
            true
        }
        Stmt::Assign { target: Expr::Var(name), value } => {
            let Some(cls) = env.class_of_var(name) else { return false };
            expr_supported(value, env) && infer_class(value, env) == Some(cls)
        }
        _ => false,
    }
}

/// Whether `func` can be fully compiled by this general emitter.
pub(crate) fn can_compile(func: &Function, funcs: &HashMap<String, &Function>) -> bool {
    // f32 in the function's own signature is excluded for ABI compatibility
    // with the legacy emitter (floats are modeled as f64 here).
    if !matches!(func.ret, Type::Void | Type::Bool | Type::I32 | Type::I64 | Type::F64) {
        return false;
    }
    let mut env = Env { funcs, types: HashMap::new(), offsets: HashMap::new() };
    let mut ints = 0usize;
    let mut floats = 0usize;
    for p in &func.params {
        if matches!(p.ty, Type::F32) {
            return false;
        }
        let Some(cls) = class_of_type(&p.ty) else { return false };
        match cls {
            Class::Int => ints += 1,
            Class::Float => floats += 1,
        }
        env.types.insert(p.name.clone(), p.ty.clone());
    }
    if ints > 6 || floats > 8 {
        return false;
    }
    func.body.iter().all(|s| stmt_supported(s, &mut env))
}

fn collect_locals(body: &[Stmt], env: &mut Env, cur_off: &mut usize) {
    for stmt in body {
        match stmt {
            Stmt::Let { name, ty, .. } => {
                *cur_off += 8;
                env.types.insert(name.clone(), ty.clone());
                env.offsets.insert(name.clone(), *cur_off);
            }
            Stmt::While { body, .. } => collect_locals(body, env, cur_off),
            Stmt::Try { body, handler, .. } => {
                collect_locals(body, env, cur_off);
                collect_locals(handler, env, cur_off);
            }
            _ => {}
        }
    }
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
    /// Number of live 8-byte temporaries currently parked on the stack,
    /// used to keep %rsp 16-byte aligned at every call.
    depth: usize,
}

impl<'m> Emitter<'m> {
    fn fresh(&mut self, kind: &str) -> String {
        let l = format!(".LG_{}_{}_{}", kind, self.func_name, self.label_counter);
        self.label_counter += 1;
        l
    }

    fn emit_epilogue(&self, out: &mut String) {
        if self.is_main {
            // Exit the process with the return value in %rax.
            out.push_str("        mov %rax, %rdi\n");
            out.push_str("        mov $60, %rax\n");
            out.push_str("        syscall\n");
            return;
        }
        if self.locals_size > 0 {
            out.push_str("        mov %rbp, %rsp\n");
        }
        out.push_str("        pop %rbp\n");
        out.push_str("        ret\n");
    }

    /// Evaluate `expr` into %rax (Int) or %xmm0 (Float). Returns its class.
    fn emit_expr(&mut self, expr: &Expr, out: &mut String) -> Class {
        match expr {
            Expr::Lit(Value::Int(v)) => {
                out.push_str(&format!("        movabsq ${}, %rax\n", v));
                Class::Int
            }
            Expr::Lit(Value::Bool(b)) => {
                out.push_str(&format!("        mov ${}, %rax\n", if *b { 1 } else { 0 }));
                Class::Int
            }
            Expr::Lit(Value::Float64(f)) => {
                out.push_str(&format!("        movabsq ${}, %rax\n", f.to_bits() as i64));
                out.push_str("        movq %rax, %xmm0\n");
                Class::Float
            }
            Expr::Lit(Value::Float32(f)) => {
                let d = *f as f64;
                out.push_str(&format!("        movabsq ${}, %rax\n", d.to_bits() as i64));
                out.push_str("        movq %rax, %xmm0\n");
                Class::Float
            }
            Expr::Var(name) => {
                let off = self.env.offsets[name];
                match self.env.class_of_var(name).unwrap() {
                    Class::Int => {
                        out.push_str(&format!("        mov -{}(%rbp), %rax\n", off));
                        Class::Int
                    }
                    Class::Float => {
                        out.push_str(&format!("        movsd -{}(%rbp), %xmm0\n", off));
                        Class::Float
                    }
                }
            }
            Expr::UnaryOp(UnaryOpKind::BitNot, inner) => {
                self.emit_expr(inner, out);
                out.push_str("        not %rax\n");
                Class::Int
            }
            Expr::BinOp(a, op, b) => {
                let cls = infer_class(a, &self.env).unwrap();
                match cls {
                    Class::Int => {
                        self.emit_expr(a, out);
                        out.push_str("        push %rax\n");
                        self.depth += 1;
                        self.emit_expr(b, out);
                        out.push_str("        mov %rax, %rbx\n");
                        out.push_str("        pop %rax\n");
                        self.depth -= 1;
                        match op {
                            BinOpKind::Add => out.push_str("        add %rbx, %rax\n"),
                            BinOpKind::Sub => out.push_str("        sub %rbx, %rax\n"),
                            BinOpKind::Mul => out.push_str("        imul %rbx, %rax\n"),
                            BinOpKind::Div => {
                                out.push_str("        cqo\n");
                                out.push_str("        idiv %rbx\n");
                            }
                            BinOpKind::BitAnd => out.push_str("        and %rbx, %rax\n"),
                            BinOpKind::BitOr => out.push_str("        or %rbx, %rax\n"),
                            BinOpKind::BitXor => out.push_str("        xor %rbx, %rax\n"),
                            BinOpKind::Shl => {
                                out.push_str("        mov %rbx, %rcx\n");
                                out.push_str("        shl %cl, %rax\n");
                            }
                            BinOpKind::Shr => {
                                out.push_str("        mov %rbx, %rcx\n");
                                out.push_str("        sar %cl, %rax\n");
                            }
                            cmp => {
                                out.push_str("        cmp %rbx, %rax\n");
                                let set = match cmp {
                                    BinOpKind::Eq => "sete",
                                    BinOpKind::Lt => "setl",
                                    BinOpKind::Le => "setle",
                                    BinOpKind::Gt => "setg",
                                    _ => "setge",
                                };
                                out.push_str(&format!("        {} %al\n", set));
                                out.push_str("        movzbq %al, %rax\n");
                            }
                        }
                        Class::Int
                    }
                    Class::Float => {
                        self.emit_expr(a, out);
                        out.push_str("        sub $8, %rsp\n");
                        out.push_str("        movsd %xmm0, (%rsp)\n");
                        self.depth += 1;
                        self.emit_expr(b, out);
                        out.push_str("        movapd %xmm0, %xmm1\n");
                        out.push_str("        movsd (%rsp), %xmm0\n");
                        out.push_str("        add $8, %rsp\n");
                        self.depth -= 1;
                        match op {
                            BinOpKind::Add => {
                                out.push_str("        addsd %xmm1, %xmm0\n");
                                Class::Float
                            }
                            BinOpKind::Sub => {
                                out.push_str("        subsd %xmm1, %xmm0\n");
                                Class::Float
                            }
                            BinOpKind::Mul => {
                                out.push_str("        mulsd %xmm1, %xmm0\n");
                                Class::Float
                            }
                            BinOpKind::Div => {
                                out.push_str("        divsd %xmm1, %xmm0\n");
                                Class::Float
                            }
                            cmp => {
                                out.push_str("        ucomisd %xmm1, %xmm0\n");
                                let set = match cmp {
                                    BinOpKind::Eq => "sete",
                                    BinOpKind::Lt => "setb",
                                    BinOpKind::Le => "setbe",
                                    BinOpKind::Gt => "seta",
                                    _ => "setae",
                                };
                                out.push_str(&format!("        {} %al\n", set));
                                out.push_str("        movzbq %al, %rax\n");
                                Class::Int
                            }
                        }
                    }
                }
            }
            Expr::Call(name, args) => {
                let callee = self.env.funcs[name.as_str()];
                let ret_cls = class_of_type(&callee.ret);
                // Evaluate all arguments left to right, parking each on the stack,
                // then load them into their ABI registers.
                let int_regs = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
                // Pad so %rsp is 16-byte aligned at the call instruction.
                let pad = self.depth % 2 == 1;
                if pad {
                    out.push_str("        sub $8, %rsp\n");
                    self.depth += 1;
                }
                let mut classes: Vec<Class> = Vec::new();
                for (p, a) in callee.params.iter().zip(args) {
                    let cls = class_of_type(&p.ty).unwrap();
                    classes.push(cls);
                    self.emit_expr(a, out);
                    out.push_str("        sub $8, %rsp\n");
                    self.depth += 1;
                    match cls {
                        Class::Int => out.push_str("        mov %rax, (%rsp)\n"),
                        Class::Float => out.push_str("        movsd %xmm0, (%rsp)\n"),
                    }
                }
                // Args are on the stack, last argument on top.
                let mut int_i = classes.iter().filter(|c| **c == Class::Int).count();
                let mut float_i = classes.iter().filter(|c| **c == Class::Float).count();
                for cls in classes.iter().rev() {
                    match cls {
                        Class::Int => {
                            int_i -= 1;
                            out.push_str(&format!("        mov (%rsp), {}\n", int_regs[int_i]));
                        }
                        Class::Float => {
                            float_i -= 1;
                            out.push_str(&format!("        movsd (%rsp), %xmm{}\n", float_i));
                        }
                    }
                    out.push_str("        add $8, %rsp\n");
                    self.depth -= 1;
                }
                out.push_str(&format!("        call {}\n", name));
                if pad {
                    out.push_str("        add $8, %rsp\n");
                    self.depth -= 1;
                }
                ret_cls.unwrap_or(Class::Int)
            }
            Expr::Cast(inner, ty) => {
                let from = self.emit_expr(inner, out);
                let to = class_of_type(ty).unwrap();
                match (from, to) {
                    (Class::Int, Class::Float) => {
                        out.push_str("        cvtsi2sdq %rax, %xmm0\n");
                    }
                    (Class::Float, Class::Int) => {
                        out.push_str("        cvttsd2si %xmm0, %rax\n");
                        if matches!(ty, Type::I32 | Type::Bool) {
                            out.push_str("        cltq\n");
                        }
                    }
                    (Class::Int, Class::Int) => {
                        if matches!(ty, Type::I32) {
                            out.push_str("        cltq\n");
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
                out.push_str("        test %rax, %rax\n");
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

    /// Store %rax/%xmm0 into the slot of `name`, wrapping i32 values.
    fn emit_store_var(&mut self, name: &str, out: &mut String) {
        let off = self.env.offsets[name];
        match self.env.types.get(name).unwrap() {
            Type::F32 | Type::F64 => {
                out.push_str(&format!("        movsd %xmm0, -{}(%rbp)\n", off));
            }
            ty => {
                if matches!(ty, Type::I32) {
                    out.push_str("        cltq\n");
                }
                out.push_str(&format!("        mov %rax, -{}(%rbp)\n", off));
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
                    out.push_str("        cltq\n");
                }
                if self.is_main && cls == Class::Float {
                    // The process exit status is an integer.
                    out.push_str("        cvttsd2si %xmm0, %rax\n");
                }
                self.emit_epilogue(out);
            }
            Stmt::Expr(e) => {
                self.emit_expr(e, out);
            }
            Stmt::Println(s) => self.emit_print_str(s, out),
            Stmt::PrintExpr(Expr::Lit(Value::String(s))) => self.emit_print_str(s, out),
            Stmt::PrintExpr(e) => match self.emit_expr(e, out) {
                Class::Int => linux_emit_print_i64(out),
                Class::Float => linux_emit_print_f64_value(out, ""),
            },
            Stmt::While { cond, body } => {
                let head = self.fresh("WH");
                let end = self.fresh("WE");
                out.push_str(&format!("{}:\n", head));
                self.emit_expr(cond, out);
                out.push_str("        test %rax, %rax\n");
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
        out.push_str(&format!(
            "        mov $1, %rax\n        mov $1, %rdi\n        leaq {}(%rip), %rsi\n        mov ${}, %rdx\n        syscall\n",
            lbl, len
        ));
    }
}

/// Emit `func` in full. Returns rodata (label, contents-with-escapes-unapplied)
/// entries to append to the module's .rodata section.
pub(crate) fn emit(
    func: &Function,
    funcs: &HashMap<String, &Function>,
    out: &mut String,
    label_counter: &mut usize,
) -> Vec<(String, String)> {
    emit_inner(func, funcs, out, label_counter, false)
}

/// Emit the body of `main` (after the `_start:` label): identical to `emit`
/// except that returning exits the process via the exit syscall.
pub(crate) fn emit_main(
    func: &Function,
    funcs: &HashMap<String, &Function>,
    out: &mut String,
    label_counter: &mut usize,
) -> Vec<(String, String)> {
    emit_inner(func, funcs, out, label_counter, true)
}

fn emit_inner(
    func: &Function,
    funcs: &HashMap<String, &Function>,
    out: &mut String,
    label_counter: &mut usize,
    is_main: bool,
) -> Vec<(String, String)> {
    let mut env = Env { funcs, types: HashMap::new(), offsets: HashMap::new() };
    let mut cur_off = 0usize;

    let int_regs = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
    let mut spills: Vec<(usize, String)> = Vec::new();
    let mut int_i = 0usize;
    let mut float_i = 0usize;
    for p in &func.params {
        cur_off += 8;
        env.types.insert(p.name.clone(), p.ty.clone());
        env.offsets.insert(p.name.clone(), cur_off);
        match class_of_type(&p.ty).unwrap() {
            Class::Int => {
                spills.push((cur_off, int_regs[int_i].to_string()));
                int_i += 1;
            }
            Class::Float => {
                spills.push((cur_off, format!("%xmm{}", float_i)));
                float_i += 1;
            }
        }
    }
    collect_locals(&func.body, &mut env, &mut cur_off);

    let mut locals_size = cur_off;
    if locals_size % 16 != 0 {
        locals_size += 16 - (locals_size % 16);
    }

    if !is_main {
        out.push_str(&format!("{}:\n", func.name));
        out.push_str("        push %rbp\n");
    }
    out.push_str("        mov %rsp, %rbp\n");
    if locals_size > 0 {
        out.push_str(&format!("        sub ${}, %rsp\n", locals_size));
    }
    for (off, reg) in &spills {
        if reg.starts_with("%xmm") {
            out.push_str(&format!("        movsd {}, -{}(%rbp)\n", reg, off));
        } else {
            out.push_str(&format!("        mov {}, -{}(%rbp)\n", reg, off));
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
    out.push_str("        xor %eax, %eax\n");
    em.emit_epilogue(out);

    *label_counter = em.label_counter;
    em.rodata
}
