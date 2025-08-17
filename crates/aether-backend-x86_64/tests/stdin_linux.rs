use aether_frontend::ast::*;
use aether_backend_x86_64::*;
use aether_codegen::CodeGenerator;

#[test]
fn linux_readln_emits_read_syscall_and_prints() {
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
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).unwrap();
    assert!(asm.contains("mov $0, %rax"));
    assert!(asm.contains("mov $0, %rdi"));
    assert!(asm.contains("syscall"));
    assert!(asm.contains("mov $1, %rax"));
}
