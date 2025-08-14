use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;
use aether_backend_aarch64::AArch64Codegen;

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
