use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;
use aether_backend_aarch64::AArch64Codegen;

fn module_with_call() -> Module {
    let callee = Function {
        name: "foo".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![Stmt::Return(Expr::Lit(Value::Int(3)))],
        is_pub: false,
        is_threaded: false,
    };
    let mainf = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Expr(Expr::Call("foo".to_string(), vec![])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    Module { items: vec![Item::Function(mainf), Item::Function(callee)] }
}

#[test]
fn aarch64_emits_bl_and_label() {
    let mut cg = AArch64Codegen::new_linux();
    let asm = cg.generate(&module_with_call()).expect("codegen ok");
    assert!(asm.contains("bl foo"));
    assert!(asm.contains("\nfoo:\n"));
}
