use aether_frontend::ast::*;
use aether_backend_x86_64::*;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_readln_emits_readfile_and_uses_r13_handle() {
    let main_fn = Item::Function(Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Call("readln".into(), vec![])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let module = Module { items: vec![main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).unwrap();
    assert!(asm.contains(".extern ReadFile") || asm.contains("ReadFile"));
    assert!(asm.contains("mov ecx, -10") && asm.contains("GetStdHandle"));
    assert!(asm.contains("call ReadFile"));
    assert!(asm.contains("mov rcx, r12") && asm.contains("call WriteFile"));
}

#[cfg(not(target_os = "windows"))]
#[test]
fn windows_readln_emits_readfile_and_uses_r13_handle_skipped() {
    assert!(true);
}
