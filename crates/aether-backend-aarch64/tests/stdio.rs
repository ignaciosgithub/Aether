use aether_codegen::CodeGenerator;

use aether_frontend::ast::*;
use aether_backend_aarch64::AArch64Codegen;

#[test]
fn println_string_call_aarch64() {
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
    let mut cg = AArch64Codegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("bl ret_str"));
    assert!(asm.contains("mov x2, x1"));
    assert!(asm.contains("mov x1, x0"));
    assert!(asm.contains("mov x8, #64"));
}

fn build_module_with_println_and_return() -> Module {
    let func = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Println("Hello AArch64".to_string()),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    Module { items: vec![Item::Function(func)] }
}

#[test]
fn aarch64_linux_println_contains_svc_and_rodata() {
    let mut cg = AArch64Codegen::new_linux();
    let module = build_module_with_println_and_return();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains(".global _start"));
    assert!(asm.contains("mov x8, #64"));
    assert!(asm.contains("mov x0, #1"));
    assert!(asm.contains("svc #0"));
    assert!(asm.contains(".section .rodata"));
    assert!(asm.contains(".ascii"));
}

#[test]
fn aarch64_float_return_sets_d0() {
    let func = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::F64,
        body: vec![
            Stmt::Return(Expr::BinOp(
                Box::new(Expr::Lit(Value::Float64(3.0))),
                BinOpKind::Mul,
                Box::new(Expr::Lit(Value::Float64(2.0))),
            )),
        ],
        is_pub: true,
        is_threaded: false,
    };
    let module = Module { items: vec![Item::Function(func)] };
    let mut cg = AArch64Codegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("ldr d0, [x1]") || asm.contains("ldr d0, [x0]") || asm.contains("ldr d0"));
}
#[test]
fn aarch64_multi_arg_call_places_ints_in_x0_x1() {
    use aether_backend_aarch64::AArch64Codegen;
    use aether_frontend::ast::*;
    let callee = Function {
        name: "bar".to_string(),
        params: vec![
            Param { name: "x".to_string(), ty: Type::I32 },
            Param { name: "y".to_string(), ty: Type::I32 },
        ],
        ret: Type::String,
        body: vec![Stmt::Return(Expr::Lit(Value::String("OK".to_string())))],
        is_pub: false,
        is_threaded: false,
    };
    let mainf = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Call("bar".to_string(), vec![Expr::Lit(Value::Int(1)), Expr::Lit(Value::Int(2))])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    let module = Module { items: vec![Item::Function(mainf), Item::Function(callee)] };
    let mut cg = AArch64Codegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("mov x0, #1") || asm.contains("movz x0"));
    assert!(asm.contains("mov x1, #2") || asm.contains("movz x1"));
    assert!(asm.contains("bl bar"));
}
