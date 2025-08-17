use aether_frontend::ast::*;
use aether_backend_x86_64::*;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_to_int_parses_and_prints() {
    let main_fn = Item::Function(Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Call(
                "to_int".into(),
                vec![Expr::Lit(Value::String("-42".into()))],
            )),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let module = Module { items: vec![main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).unwrap();

    assert!(asm.contains("imul rdi, rdi, 10") || asm.contains("imul rax, rax, 10"));

    assert!(asm.contains("call WriteFile"));
    assert!(asm.contains("mov r11, rcx") && asm.contains("mov rcx, r11"));
}

#[cfg(not(target_os = "windows"))]
#[test]
fn windows_to_int_parses_and_prints_skipped() {
    assert!(true);
}
