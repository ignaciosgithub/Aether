use aether_frontend::ast::*;
use aether_backend_x86_64::*;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_stdin_to_int_does_not_call_to_int_symbol() {
    let main_fn = Item::Function(Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Call("to_int".into(), vec![Expr::Call("readln".into(), vec![])])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let module = Module { items: vec![main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).unwrap();

    assert!(!asm.contains("call to_int"), "Should inline to_int parsing, not call an external symbol");
    assert!(asm.contains(".LTOIERRWIN") || asm.contains("to_int error"));
    assert!(asm.contains("call WriteFile"));
}

#[cfg(not(target_os = "windows"))]
#[test]
fn windows_stdin_to_int_does_not_call_to_int_symbol_skipped() {
    assert!(true);
}
