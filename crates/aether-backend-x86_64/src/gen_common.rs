//! Target-independent analysis shared by the general (in-source-order) code
//! generators. Determines which functions can be compiled generically and
//! computes value classes and stack layouts; the per-OS emitters
//! (`linux_gen`, `windows_gen`) do the actual instruction emission.

use std::collections::HashMap;

use aether_frontend::ast::{BinOpKind, Expr, Function, Stmt, Type, UnaryOpKind, Value};

/// Value class an expression evaluates to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Class {
    Int,
    Float,
    /// A `String` value: pointer in the primary integer register, length in
    /// the secondary one (rax/rdx on both ABIs, matching the legacy emitter).
    Str,
}

/// Per-ABI argument-register limits.
#[derive(Debug, Clone, Copy)]
pub(crate) struct AbiLimits {
    pub max_int_args: usize,
    pub max_float_args: usize,
    /// Maximum total register arguments (positional ABIs like Win64).
    pub max_total_args: usize,
    /// Whether this emitter handles the spawn/join/destroy thread builtins.
    pub threads: bool,
}

pub(crate) const SYSV_LIMITS: AbiLimits = AbiLimits {
    max_int_args: 6,
    max_float_args: 8,
    max_total_args: usize::MAX,
    threads: false,
};
pub(crate) const WIN64_LIMITS: AbiLimits = AbiLimits {
    max_int_args: 4,
    max_float_args: 4,
    max_total_args: 4,
    threads: true,
};

pub(crate) fn class_of_type(ty: &Type) -> Option<Class> {
    match ty {
        Type::Bool | Type::I32 | Type::I64 => Some(Class::Int),
        Type::F32 | Type::F64 => Some(Class::Float),
        // Pointers are machine words; the pointee class is resolved on deref.
        Type::Ptr(inner) => class_of_type(inner).map(|_| Class::Int),
        _ => None,
    }
}

/// Module-level static struct data: which statics exist and the byte
/// offset/type of every (struct, field) pair, inherited fields included.
#[derive(Default)]
pub(crate) struct StaticsInfo {
    /// static variable name -> struct type name
    pub types: HashMap<String, String>,
    /// (struct name, field name) -> (byte offset, field type)
    pub field_offsets: HashMap<(String, String), (usize, Type)>,
}

impl StaticsInfo {
    /// Field (offset, type) for `recv.field` when `recv` names a static.
    pub fn field_of(&self, recv: &str, field: &str) -> Option<(usize, Type)> {
        let sname = self.types.get(recv)?;
        self.field_offsets
            .get(&(sname.clone(), field.to_string()))
            .cloned()
    }
}

pub(crate) struct Env<'m> {
    pub funcs: &'m HashMap<String, &'m Function>,
    pub statics: &'m StaticsInfo,
    pub types: HashMap<String, Type>,
    pub offsets: HashMap<String, usize>,
    pub limits: AbiLimits,
}

impl<'m> Env<'m> {
    pub fn class_of_var(&self, name: &str) -> Option<Class> {
        self.types.get(name).and_then(class_of_type)
    }
}

/// Class of the value obtained by dereferencing `expr`, when `expr` is a
/// pointer whose pointee type is statically known.
pub(crate) fn pointee_class(expr: &Expr, env: &Env) -> Option<Class> {
    match expr {
        Expr::Var(name) => match env.types.get(name)? {
            Type::Ptr(inner) => class_of_type(inner),
            _ => None,
        },
        Expr::AddrOf(inner) => infer_class(inner, env),
        _ => None,
    }
}

pub(crate) fn infer_class(expr: &Expr, env: &Env) -> Option<Class> {
    match expr {
        Expr::Lit(Value::Int(_)) | Expr::Lit(Value::Bool(_)) => Some(Class::Int),
        Expr::Lit(Value::Float64(_)) | Expr::Lit(Value::Float32(_)) => Some(Class::Float),
        Expr::Lit(Value::String(_)) => Some(Class::Str),
        Expr::Var(name) => env.class_of_var(name),
        Expr::UnaryOp(UnaryOpKind::BitNot, inner) => {
            (infer_class(inner, env)? == Class::Int).then_some(Class::Int)
        }
        Expr::BinOp(a, op, b) => {
            let ca = infer_class(a, env)?;
            let cb = infer_class(b, env)?;
            if ca != cb || ca == Class::Str {
                return None;
            }
            match op {
                BinOpKind::Eq | BinOpKind::Lt | BinOpKind::Le | BinOpKind::Gt | BinOpKind::Ge => {
                    Some(Class::Int)
                }
                BinOpKind::BitAnd
                | BinOpKind::BitOr
                | BinOpKind::BitXor
                | BinOpKind::Shl
                | BinOpKind::Shr => (ca == Class::Int).then_some(Class::Int),
                BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div => Some(ca),
            }
        }
        Expr::Call(name, _) if env.limits.threads && is_thread_builtin(name) => Some(Class::Int),
        Expr::Call(name, _) => {
            let callee = env.funcs.get(name)?;
            if matches!(callee.ret, Type::String) {
                return Some(Class::Str);
            }
            class_of_type(&callee.ret)
        }
        Expr::Cast(_, ty) => class_of_type(ty),
        Expr::AddrOf(inner) => match &**inner {
            Expr::Var(name) => env.class_of_var(name).map(|_| Class::Int),
            _ => None,
        },
        Expr::Deref(inner) => pointee_class(inner, env),
        Expr::Field(recv, fname) => {
            let Expr::Var(rn) = &**recv else { return None };
            // Only statics; struct-typed locals are handled by the legacy path.
            if env.types.contains_key(rn) {
                return None;
            }
            let (_, fty) = env.statics.field_of(rn, fname)?;
            match fty {
                Type::String => Some(Class::Str),
                Type::I32 | Type::I64 | Type::F64 => class_of_type(&fty),
                _ => None,
            }
        }
        Expr::IfElse {
            cond,
            then_expr,
            else_expr,
        } => {
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

pub(crate) fn expr_supported(expr: &Expr, env: &Env) -> bool {
    match expr {
        Expr::Lit(Value::Int(_))
        | Expr::Lit(Value::Bool(_))
        | Expr::Lit(Value::Float64(_))
        | Expr::Lit(Value::Float32(_))
        | Expr::Lit(Value::String(_)) => true,
        Expr::Var(name) => env.class_of_var(name).is_some(),
        Expr::UnaryOp(UnaryOpKind::BitNot, inner) => {
            expr_supported(inner, env) && infer_class(inner, env) == Some(Class::Int)
        }
        Expr::BinOp(a, _, b) => {
            expr_supported(a, env) && expr_supported(b, env) && infer_class(expr, env).is_some()
        }
        Expr::Call(name, args) if env.limits.threads && is_thread_builtin(name) => {
            thread_builtin_supported(name, args, env)
        }
        Expr::Call(name, args) => {
            let Some(callee) = env.funcs.get(name) else {
                return false;
            };
            if callee.params.len() != args.len() {
                return false;
            }
            if class_of_type(&callee.ret).is_none()
                && !matches!(callee.ret, Type::Void | Type::String)
            {
                return false;
            }
            // f32 in call signatures is excluded: the general emitters model
            // floats as f64 while the legacy emitter passes f32 differently.
            if matches!(callee.ret, Type::F32) {
                return false;
            }
            let mut ints = 0usize;
            let mut floats = 0usize;
            for (p, a) in callee.params.iter().zip(args) {
                if matches!(p.ty, Type::F32) {
                    return false;
                }
                let Some(pc) = class_of_type(&p.ty) else {
                    return false;
                };
                match pc {
                    // String parameters are not supported by the general
                    // emitters yet.
                    Class::Str => return false,
                    Class::Int => ints += 1,
                    Class::Float => floats += 1,
                }
                if !expr_supported(a, env) || infer_class(a, env) != Some(pc) {
                    return false;
                }
            }
            ints <= env.limits.max_int_args
                && floats <= env.limits.max_float_args
                && args.len() <= env.limits.max_total_args
        }
        Expr::Cast(inner, ty) => {
            class_of_type(ty).is_some()
                && expr_supported(inner, env)
                && matches!(infer_class(inner, env), Some(Class::Int | Class::Float))
        }
        Expr::AddrOf(inner) => {
            matches!(&**inner, Expr::Var(name) if env.class_of_var(name).is_some())
        }
        Expr::Deref(inner) => expr_supported(inner, env) && pointee_class(inner, env).is_some(),
        Expr::Field(_, _) => infer_class(expr, env).is_some(),
        Expr::IfElse {
            cond,
            then_expr,
            else_expr,
        } => {
            expr_supported(cond, env)
                && expr_supported(then_expr, env)
                && expr_supported(else_expr, env)
                && infer_class(expr, env).is_some()
        }
        _ => false,
    }
}

pub(crate) fn is_thread_builtin(name: &str) -> bool {
    matches!(name, "spawn" | "join" | "destroy")
}

/// Worker functions started by `spawn` must have the fixed signature
/// `func name(arg: i64) -> i32`.
fn thread_builtin_supported(name: &str, args: &[Expr], env: &Env) -> bool {
    match (name, args) {
        ("spawn", [Expr::Lit(Value::String(worker)), arg]) => {
            let Some(w) = env.funcs.get(worker) else {
                return false;
            };
            w.params.len() == 1
                && matches!(w.params[0].ty, Type::I64)
                && matches!(w.ret, Type::I32)
                && expr_supported(arg, env)
                && infer_class(arg, env) == Some(Class::Int)
        }
        ("join", [h]) | ("destroy", [h]) => {
            expr_supported(h, env) && infer_class(h, env) == Some(Class::Int)
        }
        _ => false,
    }
}

pub(crate) fn stmt_supported(stmt: &Stmt, env: &mut Env) -> bool {
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
            let Some(cls) = class_of_type(ty) else {
                return false;
            };
            if !expr_supported(init, env) || infer_class(init, env) != Some(cls) {
                return false;
            }
            // Register so later statements can reference it during the check.
            env.types.insert(name.clone(), ty.clone());
            true
        }
        Stmt::Assign {
            target: Expr::Var(name),
            value,
        } => {
            let Some(cls) = env.class_of_var(name) else {
                return false;
            };
            expr_supported(value, env) && infer_class(value, env) == Some(cls)
        }
        Stmt::Assign {
            target: Expr::Deref(inner),
            value,
        } => {
            let Some(cls) = pointee_class(inner, env) else {
                return false;
            };
            expr_supported(inner, env)
                && expr_supported(value, env)
                && infer_class(value, env) == Some(cls)
        }
        Stmt::Throw(Expr::Lit(Value::String(_))) => true,
        Stmt::Try {
            body,
            err_name,
            handler,
        } => {
            body.iter().all(|s| stmt_supported(s, env))
                && handler.iter().all(|s| {
                    // The exception variable is a String printable only via
                    // println(err) inside the handler.
                    matches!(s, Stmt::PrintExpr(Expr::Var(n)) if n == err_name)
                        || stmt_supported(s, env)
                })
        }
        _ => false,
    }
}

/// Whether `func` can be fully compiled by a general emitter with `limits`.
pub(crate) fn can_compile(
    func: &Function,
    funcs: &HashMap<String, &Function>,
    statics: &StaticsInfo,
    limits: AbiLimits,
) -> bool {
    // f32 in the function's own signature is excluded for ABI compatibility
    // with the legacy emitter (floats are modeled as f64 here).
    if !matches!(
        func.ret,
        Type::Void | Type::Bool | Type::I32 | Type::I64 | Type::F64 | Type::String
    ) {
        return false;
    }
    let mut env = Env {
        funcs,
        statics,
        types: HashMap::new(),
        offsets: HashMap::new(),
        limits,
    };
    let mut ints = 0usize;
    let mut floats = 0usize;
    for p in &func.params {
        if matches!(p.ty, Type::F32) {
            return false;
        }
        let Some(cls) = class_of_type(&p.ty) else {
            return false;
        };
        match cls {
            Class::Str => return false,
            Class::Int => ints += 1,
            Class::Float => floats += 1,
        }
        env.types.insert(p.name.clone(), p.ty.clone());
    }
    if ints > limits.max_int_args
        || floats > limits.max_float_args
        || func.params.len() > limits.max_total_args
    {
        return false;
    }
    func.body.iter().all(|s| stmt_supported(s, &mut env))
}

pub(crate) fn collect_locals(body: &[Stmt], env: &mut Env, cur_off: &mut usize) {
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
