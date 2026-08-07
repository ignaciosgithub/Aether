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

use crate::gen_common::{
    class_of_type, collect_locals, infer_class, pointee_class, Class, Env, StaticsInfo,
    SYSV_LIMITS,
};
use crate::{linux_emit_print_f64_value, linux_emit_print_i64};

/// Whether `func` can be fully compiled by this general emitter.
pub(crate) fn can_compile(
    func: &Function,
    funcs: &HashMap<String, &Function>,
    statics: &StaticsInfo,
) -> bool {
    crate::gen_common::can_compile(func, funcs, statics, SYSV_LIMITS)
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
    /// Catch labels of enclosing try blocks, innermost last.
    try_stack: Vec<String>,
    /// Exception variable names of enclosing except handlers, innermost last.
    exc_vars: Vec<String>,
    /// Whether the per-function uncaught-exception exit path is referenced.
    needs_uncaught: bool,
    /// Whether the per-function out-of-bounds exit path is referenced.
    needs_oob: bool,
    /// Whether the AE_EXC_PTR/AE_EXC_LEN globals are referenced.
    uses_exc: bool,
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
            Expr::Lit(Value::String(s)) => {
                let lbl = self.fresh("SLIT");
                let len = s.as_bytes().len();
                self.rodata.push((lbl.clone(), s.clone()));
                out.push_str(&format!("        leaq {}(%rip), %rax\n", lbl));
                out.push_str(&format!("        mov ${}, %rdx\n", len));
                Class::Str
            }
            Expr::Var(name) => {
                let off = self.env.offsets[name];
                match self.env.class_of_var(name).unwrap() {
                    Class::Str => unreachable!("string locals rejected by can_compile"),
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
                    Class::Str => unreachable!("string operators rejected by can_compile"),
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
                            BinOpKind::Mul => {
                                out.push_str("        imul %rbx, %rax\n");
                            }
                            BinOpKind::Div => {
                                let ok = self.fresh("DIVOK");
                                out.push_str("        test %rbx, %rbx\n");
                                out.push_str(&format!("        jnz {}\n", ok));
                                self.emit_throw("division by zero", out);
                                out.push_str(&format!("{}:\n", ok));
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
                let ret_cls = if matches!(callee.ret, Type::String) {
                    Some(Class::Str)
                } else {
                    class_of_type(&callee.ret)
                };
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
                        Class::Str => unreachable!("string args rejected by can_compile"),
                        Class::Int => out.push_str("        mov %rax, (%rsp)\n"),
                        Class::Float => out.push_str("        movsd %xmm0, (%rsp)\n"),
                    }
                }
                // Args are on the stack, last argument on top.
                let mut int_i = classes.iter().filter(|c| **c == Class::Int).count();
                let mut float_i = classes.iter().filter(|c| **c == Class::Float).count();
                for cls in classes.iter().rev() {
                    match cls {
                        Class::Str => unreachable!("string args rejected by can_compile"),
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
                    (Class::Str, _) | (_, Class::Str) => {
                        unreachable!("string casts rejected by can_compile")
                    }
                }
                to
            }
            Expr::AddrOf(inner) => {
                let Expr::Var(name) = &**inner else {
                    unreachable!("addr-of target rejected by can_compile")
                };
                let off = self.env.offsets[name];
                out.push_str(&format!("        leaq -{}(%rbp), %rax\n", off));
                Class::Int
            }
            Expr::Deref(inner) => {
                let cls = pointee_class(inner, &self.env).unwrap();
                self.emit_expr(inner, out);
                match cls {
                    Class::Str => unreachable!("string pointees rejected by can_compile"),
                    Class::Int => out.push_str("        mov (%rax), %rax\n"),
                    Class::Float => out.push_str("        movsd (%rax), %xmm0\n"),
                }
                cls
            }
            Expr::Field(recv, fname) => {
                let Expr::Var(rn) = &**recv else {
                    unreachable!("field receiver rejected by can_compile")
                };
                let (off, fty) = match self.env.types.get(rn) {
                    Some(Type::User(sname)) => {
                        let info = self.env.statics.field_of_struct(sname, fname).unwrap();
                        let base = self.env.offsets[rn];
                        out.push_str(&format!("        leaq -{}(%rbp), %rax\n", base));
                        info
                    }
                    _ => {
                        let info = self.env.statics.field_of(rn, fname).unwrap();
                        out.push_str(&format!("        leaq {}(%rip), %rax\n", rn));
                        info
                    }
                };
                match fty {
                    Type::I64 => {
                        out.push_str(&format!("        mov {}(%rax), %rax\n", off));
                        Class::Int
                    }
                    Type::I32 => {
                        out.push_str(&format!("        movslq {}(%rax), %rax\n", off));
                        Class::Int
                    }
                    Type::F64 => {
                        out.push_str(&format!("        movsd {}(%rax), %xmm0\n", off));
                        Class::Float
                    }
                    Type::String => {
                        out.push_str(&format!("        mov {}(%rax), %rdx\n", off + 8));
                        out.push_str(&format!("        mov {}(%rax), %rax\n", off));
                        Class::Str
                    }
                    _ => unreachable!("field type rejected by can_compile"),
                }
            }
            Expr::Index(base, idx) => {
                let Expr::Var(bn) = &**base else {
                    unreachable!("index base rejected by can_compile")
                };
                let Some(Type::Array(elem, n)) = self.env.types.get(bn).cloned() else {
                    unreachable!("index base type rejected by can_compile")
                };
                let esize = crate::gen_common::array_elem_size(&elem).unwrap();
                let boff = self.env.offsets[bn];
                self.emit_expr(idx, out);
                out.push_str(&format!("        cmp ${}, %rax\n", n));
                self.needs_oob = true;
                out.push_str(&format!("        jae .LG_OOB_{}\n", self.func_name));
                out.push_str(&format!("        leaq -{}(%rbp), %rbx\n", boff));
                match *elem {
                    Type::I64 => {
                        out.push_str("        mov (%rbx,%rax,8), %rax\n");
                        Class::Int
                    }
                    Type::I32 => {
                        out.push_str("        movslq (%rbx,%rax,4), %rax\n");
                        Class::Int
                    }
                    Type::F64 => {
                        out.push_str("        movsd (%rbx,%rax,8), %xmm0\n");
                        Class::Float
                    }
                    _ => unreachable!("array element type rejected by can_compile: {:?}", esize),
                }
            }
            Expr::IfElse {
                cond,
                then_expr,
                else_expr,
            } => {
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

    /// Record `msg` as the current exception and transfer control to the
    /// innermost try handler, or to the function's uncaught-exception exit.
    fn emit_throw(&mut self, msg: &str, out: &mut String) {
        self.uses_exc = true;
        let lbl = self.fresh("EXC");
        let len = msg.as_bytes().len();
        self.rodata.push((lbl.clone(), msg.to_string()));
        out.push_str(&format!("        leaq {}(%rip), %r11\n", lbl));
        out.push_str("        mov %r11, AE_EXC_PTR(%rip)\n");
        out.push_str(&format!("        movq ${}, AE_EXC_LEN(%rip)\n", len));
        let target = match self.try_stack.last() {
            Some(catch) => catch.clone(),
            None => {
                self.needs_uncaught = true;
                format!(".LG_UNC_{}", self.func_name)
            }
        };
        out.push_str(&format!("        jmp {}\n", target));
    }

    /// Print the String value in %rax (pointer) / %rdx (length) with a
    /// trailing newline.
    fn emit_print_str_value(&mut self, out: &mut String) {
        out.push_str("        mov %rax, %rsi\n");
        out.push_str("        mov $1, %rax\n");
        out.push_str("        mov $1, %rdi\n");
        out.push_str("        syscall\n");
        let nl = self.fresh("NL");
        self.rodata.push((nl.clone(), "\n".to_string()));
        out.push_str("        mov $1, %rax\n");
        out.push_str("        mov $1, %rdi\n");
        out.push_str(&format!("        leaq {}(%rip), %rsi\n", nl));
        out.push_str("        mov $1, %rdx\n");
        out.push_str("        syscall\n");
    }

    /// Print the current exception message (from the AE_EXC globals) with a
    /// trailing newline.
    fn emit_print_exc(&mut self, out: &mut String) {
        self.uses_exc = true;
        out.push_str("        mov $1, %rax\n");
        out.push_str("        mov $1, %rdi\n");
        out.push_str("        mov AE_EXC_PTR(%rip), %rsi\n");
        out.push_str("        mov AE_EXC_LEN(%rip), %rdx\n");
        out.push_str("        syscall\n");
        let nl = self.fresh("NL");
        self.rodata.push((nl.clone(), "\n".to_string()));
        out.push_str("        mov $1, %rax\n");
        out.push_str("        mov $1, %rdi\n");
        out.push_str(&format!("        leaq {}(%rip), %rsi\n", nl));
        out.push_str("        mov $1, %rdx\n");
        out.push_str("        syscall\n");
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

    /// Store the struct-literal/array-literal `init` into the aggregate
    /// local `name`. Returns false when `init` is not an aggregate literal.
    fn emit_aggregate_let(&mut self, name: &str, init: &Expr, out: &mut String) -> bool {
        let base = match self.env.types.get(name) {
            Some(Type::User(_)) | Some(Type::Array(_, _)) => self.env.offsets[name],
            _ => return false,
        };
        match (self.env.types.get(name).cloned(), init) {
            (Some(Type::User(sname)), Expr::StructLit(_, fields)) => {
                let flat = self.env.statics.flattened_fields[&sname].clone();
                for (fname, fty, foff) in flat {
                    let (_, fexpr) = fields.iter().find(|(n, _)| *n == fname).unwrap();
                    self.emit_expr(fexpr, out);
                    let addr = |extra: usize| {
                        format!("-{}+{}(%rbp)", base, foff + extra)
                    };
                    match fty {
                        Type::I64 => out.push_str(&format!("        mov %rax, {}\n", addr(0))),
                        Type::I32 => out.push_str(&format!("        mov %eax, {}\n", addr(0))),
                        Type::F64 => out.push_str(&format!("        movsd %xmm0, {}\n", addr(0))),
                        Type::String => {
                            out.push_str(&format!("        mov %rax, {}\n", addr(0)));
                            out.push_str(&format!("        mov %rdx, {}\n", addr(8)));
                        }
                        _ => unreachable!("field type rejected by can_compile"),
                    }
                }
                true
            }
            (Some(Type::Array(elem, _)), Expr::ArrayLit(items)) => {
                let esize = crate::gen_common::array_elem_size(&elem).unwrap();
                for (i, item) in items.iter().enumerate() {
                    self.emit_expr(item, out);
                    let addr = format!("-{}+{}(%rbp)", base, i * esize);
                    match *elem {
                        Type::I64 => out.push_str(&format!("        mov %rax, {}\n", addr)),
                        Type::I32 => out.push_str(&format!("        mov %eax, {}\n", addr)),
                        Type::F64 => out.push_str(&format!("        movsd %xmm0, {}\n", addr)),
                        _ => unreachable!("array element type rejected by can_compile"),
                    }
                }
                true
            }
            _ => false,
        }
    }

    fn emit_stmt(&mut self, stmt: &Stmt, out: &mut String) {
        match stmt {
            Stmt::Let { name, init, .. } => {
                if self.emit_aggregate_let(name, init, out) {
                    return;
                }
                self.emit_expr(init, out);
                self.emit_store_var(name, out);
            }
            Stmt::Assign {
                target: Expr::Var(name),
                value,
            } => {
                self.emit_expr(value, out);
                self.emit_store_var(name, out);
            }
            Stmt::Assign {
                target: Expr::Deref(inner),
                value,
            } => {
                self.emit_expr(inner, out);
                out.push_str("        push %rax\n");
                self.depth += 1;
                let cls = self.emit_expr(value, out);
                out.push_str("        pop %rbx\n");
                self.depth -= 1;
                match cls {
                    Class::Str => unreachable!("string stores rejected by can_compile"),
                    Class::Int => out.push_str("        mov %rax, (%rbx)\n"),
                    Class::Float => out.push_str("        movsd %xmm0, (%rbx)\n"),
                }
            }
            Stmt::Return(e) => {
                let cls = self.emit_expr(e, out);
                if matches!(self.ret_ty, Type::I32) && cls == Class::Int {
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
            Stmt::PrintExpr(Expr::Var(n)) if self.exc_vars.iter().any(|v| v == n) => {
                self.emit_print_exc(out);
            }
            Stmt::PrintExpr(e) => match self.emit_expr(e, out) {
                Class::Int => linux_emit_print_i64(out),
                Class::Float => linux_emit_print_f64_value(out, ""),
                Class::Str => self.emit_print_str_value(out),
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
            Stmt::Throw(Expr::Lit(Value::String(msg))) => {
                let msg = msg.clone();
                self.emit_throw(&msg, out);
            }
            Stmt::Try {
                body,
                err_name,
                handler,
            } => {
                let catch = self.fresh("CAT");
                let end = self.fresh("TRE");
                self.try_stack.push(catch.clone());
                for s in body {
                    self.emit_stmt(s, out);
                }
                self.try_stack.pop();
                out.push_str(&format!("        jmp {}\n", end));
                out.push_str(&format!("{}:\n", catch));
                // A throw may fire mid-expression with temporaries parked on
                // the stack; restore the statement-level stack pointer.
                out.push_str("        mov %rbp, %rsp\n");
                if self.locals_size > 0 {
                    out.push_str(&format!("        sub ${}, %rsp\n", self.locals_size));
                }
                self.exc_vars.push(err_name.clone());
                for s in handler {
                    self.emit_stmt(s, out);
                }
                self.exc_vars.pop();
                out.push_str(&format!("{}:\n", end));
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
    statics: &StaticsInfo,
    out: &mut String,
    label_counter: &mut usize,
) -> Vec<(String, String)> {
    emit_inner(func, funcs, statics, out, label_counter, false)
}

/// Emit the body of `main` (after the `_start:` label): identical to `emit`
/// except that returning exits the process via the exit syscall.
pub(crate) fn emit_main(
    func: &Function,
    funcs: &HashMap<String, &Function>,
    statics: &StaticsInfo,
    out: &mut String,
    label_counter: &mut usize,
) -> Vec<(String, String)> {
    emit_inner(func, funcs, statics, out, label_counter, true)
}

fn emit_inner(
    func: &Function,
    funcs: &HashMap<String, &Function>,
    statics: &StaticsInfo,
    out: &mut String,
    label_counter: &mut usize,
    is_main: bool,
) -> Vec<(String, String)> {
    let mut env = Env {
        funcs,
        statics,
        types: HashMap::new(),
        offsets: HashMap::new(),
        limits: SYSV_LIMITS,
    };
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
            Class::Str => unreachable!("string params rejected by can_compile"),
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
        try_stack: Vec::new(),
        exc_vars: Vec::new(),
        needs_uncaught: false,
        needs_oob: false,
        uses_exc: false,
        depth: 0,
    };
    for stmt in &func.body {
        em.emit_stmt(stmt, out);
    }
    // Implicit return 0 if control falls off the end.
    out.push_str("        xor %eax, %eax\n");
    em.emit_epilogue(out);

    if em.needs_uncaught {
        let prefix = em.fresh("EXCPFX");
        em.rodata.push((prefix.clone(), "Exception: ".to_string()));
        out.push_str(&format!(".LG_UNC_{}:\n", em.func_name));
        out.push_str("        mov $1, %rax\n");
        out.push_str("        mov $1, %rdi\n");
        out.push_str(&format!("        leaq {}(%rip), %rsi\n", prefix));
        out.push_str("        mov $11, %rdx\n");
        out.push_str("        syscall\n");
        em.emit_print_exc(out);
        out.push_str("        mov $60, %rax\n");
        out.push_str("        mov $1, %rdi\n");
        out.push_str("        syscall\n");
    }
    if em.needs_oob {
        let msg = "index out of bounds\n";
        let lbl = em.fresh("OOBMSG");
        em.rodata.push((lbl.clone(), msg.to_string()));
        out.push_str(&format!(".LG_OOB_{}:\n", em.func_name));
        out.push_str("        mov $1, %rax\n");
        out.push_str("        mov $2, %rdi\n");
        out.push_str(&format!("        leaq {}(%rip), %rsi\n", lbl));
        out.push_str(&format!("        mov ${}, %rdx\n", msg.len()));
        out.push_str("        syscall\n");
        out.push_str("        mov $60, %rax\n");
        out.push_str("        mov $1, %rdi\n");
        out.push_str("        syscall\n");
    }
    if em.uses_exc {
        // Common symbols merge across functions and modules.
        out.push_str("        .comm AE_EXC_PTR,8\n");
        out.push_str("        .comm AE_EXC_LEN,8\n");
    }

    *label_counter = em.label_counter;
    em.rodata
}
