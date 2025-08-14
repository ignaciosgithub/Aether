use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen;

fn module_with_call() -> Module {
    let callee = Function {
        name: "foo".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![Stmt::Return(Expr::Lit(Value::Int(7)))],
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
fn linux_emits_call_and_label() {
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module_with_call()).expect("codegen ok");
    assert!(asm.contains("call foo"));
    assert!(asm.contains("\nfoo:\n"));
}

#[test]
fn windows_emits_call_and_label() {
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module_with_call()).expect("codegen ok");
    assert!(asm.contains("call foo"));
    assert!(asm.contains("\nfoo:\n"));
}
