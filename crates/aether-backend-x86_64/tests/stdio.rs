use aether_codegen::CodeGenerator;

use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen;

#[test]
fn println_string_call_linux() {
    let func_ret = Function {
        name: "ret_str".to_string(),
        params: vec![],
        ret: Type::String,
        body: vec![
            Stmt::Return(Expr::Lit(Value::String("HelloExpr".to_string()))),
        ],
        is_pub: false,
        is_threaded: false,
    };
    let main = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Call("ret_str".to_string(), vec![])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    let module = Module { items: vec![Item::Function(main.clone()), Item::Function(func_ret.clone())] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("call ret_str"));
    assert!(asm.contains("mov %rax, %rsi"));
    assert!(asm.contains("mov $1, %rax"));
    assert!(asm.contains("syscall"));
}

fn build_module_with_println_and_return() -> Module {
    let func = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Println("Hello".to_string()),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    Module { items: vec![Item::Function(func)] }
}

#[test]
fn linux_println_codegen_contains_syscall_and_rodata() {
    let mut cg = X86_64LinuxCodegen::new_linux();
    let module = build_module_with_println_and_return();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains(".global _start"));
    assert!(asm.contains("mov $1, %rax"));
    assert!(asm.contains("mov $1, %rdi"));
    assert!(asm.contains("syscall"));
    assert!(asm.contains(".section .rodata"));
    assert!(asm.contains(".ascii"));
}

#[test]
fn windows_float_return_still_sets_xmm0() {
    let func = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::F64,
        body: vec![
            Stmt::Return(Expr::BinOp(
                Box::new(Expr::Lit(Value::Float64(1.5))),
                BinOpKind::Add,
                Box::new(Expr::Lit(Value::Float64(2.5))),
            )),
        ],
        is_pub: true,
        is_threaded: false,
    };
    let module = Module { items: vec![Item::Function(func)] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("movsd xmm0"));
}
#[test]
fn windows_println_codegen_contains_winapi_calls_and_data() {
    let func = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Println("Hello Win".to_string()),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    let module = Module { items: vec![Item::Function(func)] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains(".extern GetStdHandle"));
    assert!(asm.contains(".extern WriteFile"));
    assert!(asm.contains("call GetStdHandle"));
    assert!(asm.contains("call WriteFile"));
    assert!(asm.contains("sub rsp, 40"));
    assert!(asm.contains(".data"));
    assert!(asm.contains(".ascii"));
}
#[test]
fn windows_println_string_call_uses_ptr_len_and_writefile() {
    let func_ret = Function {
        name: "ret_str".to_string(),
        params: vec![],
        ret: Type::String,
        body: vec![
            Stmt::Return(Expr::Lit(Value::String("HelloExpr".to_string()))),
        ],
        is_pub: false,
        is_threaded: false,
    };
    let main = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Call("ret_str".to_string(), vec![])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    let module = Module { items: vec![Item::Function(main), Item::Function(func_ret)] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("call ret_str"));
    assert!(asm.contains("mov rdx, rax"));
    assert!(asm.contains("mov r8d, edx"));
    assert!(asm.contains("call WriteFile"));
}
#[test]
fn linux_runtime_ifelse_branches_and_prints_once() {
    use aether_backend_x86_64::X86_64LinuxCodegen;
    use aether_frontend::ast::*;
    let func = Function {
        name: "main".to_string(),
        params: vec![
            Param { name: "a".to_string(), ty: Type::I32 },
            Param { name: "b".to_string(), ty: Type::I32 },
        ],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::IfElse {
                cond: Box::new(Expr::BinOp(
                    Box::new(Expr::Var("a".to_string())),
                    BinOpKind::Lt,
                    Box::new(Expr::Var("b".to_string())),
                )),
                then_expr: Box::new(Expr::Lit(Value::String("LT".to_string()))),
                else_expr: Box::new(Expr::Lit(Value::String("GE".to_string()))),
            }),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    let module = Module { items: vec![Item::Function(func)] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains(".LIF_THEN_main_0"));
    assert!(asm.contains(".LIF_ELSE_main_0"));
    assert!(asm.contains(".LIF_JOIN_main_0"));
    assert!(asm.contains("cmp %r11, %r10"));
    assert!(asm.contains("jl .LIF_THEN_main_0"));
    assert!(asm.contains("syscall"));
}

#[test]
fn linux_multi_arg_call_places_ints_in_rdi_rsi() {
    use aether_backend_x86_64::X86_64LinuxCodegen;
    use aether_frontend::ast::*;
    let callee = Function {
        name: "foo".to_string(),
        params: vec![Param { name: "x".to_string(), ty: Type::I32 }, Param { name: "y".to_string(), ty: Type::I32 }],
        ret: Type::String,
        body: vec![
            Stmt::Return(Expr::Lit(Value::String("OK".to_string()))),
        ],
        is_pub: false,
        is_threaded: false,
    };
    let mainf = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Call("foo".to_string(), vec![Expr::Lit(Value::Int(7)), Expr::Lit(Value::Int(8))])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    let module = Module { items: vec![Item::Function(mainf), Item::Function(callee)] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("mov $7, %rdi"));
    assert!(asm.contains("mov $8, %rsi"));
    assert!(asm.contains("call foo"));
}
