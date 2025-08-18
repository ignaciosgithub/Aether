use aether_frontend::ast::*;
use aether_backend_x86_64::*;
use aether_codegen::CodeGenerator;

#[test]
fn stdin_to_int_linux_generated_asm_has_no_rust_injection() {
    let main_fn = Item::Function(Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Println("enter number:".into()),
            Stmt::PrintExpr(Expr::Call("to_int".into(), vec![Expr::Call("readln".into(), vec![])])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let module = Module { items: vec![main_fn] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).unwrap();

    assert!(!asm.contains("if self.target.os == TargetOs::Linux"), "Injected Rust source leaked into assembly");
    assert!(asm.contains(".bss") || asm.contains(".text"));
    assert!(asm.contains("mov $0, %rax") && asm.contains("syscall"), "Expected read syscall sequence");
}
