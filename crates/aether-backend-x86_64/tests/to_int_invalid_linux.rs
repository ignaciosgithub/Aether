use aether_frontend::ast::*;
use aether_backend_x86_64::*;
use aether_codegen::CodeGenerator;

#[test]
fn linux_to_int_errors_on_nondigit() {
    let main_fn = Item::Function(Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Call(
                "to_int".into(),
                vec![Expr::Lit(Value::String("12&3".into()))],
            )),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let module = Module { items: vec![main_fn] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).unwrap();

    assert!(asm.contains(".LTOIERR") || asm.contains("to_int error"));
    assert!(asm.contains("mov $60, %rax") || asm.contains("mov $60,%rax"));
    assert!(asm.contains("mov $1, %rdi") || asm.contains("mov $1,%rdi"));
    assert!(asm.contains("syscall"));
}
