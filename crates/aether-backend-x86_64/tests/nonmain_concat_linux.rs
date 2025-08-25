use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn nonmain_string_concat_and_print_linux() {
    let bar = Item::Function(Function {
        name: "bar".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Call(
                "concat".into(),
                vec![Expr::Lit(Value::String("hi".into())), Expr::Lit(Value::String("there".into()))],
            )),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let main = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![Stmt::Return(Expr::Lit(Value::Int(0)))],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![bar, main] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nbar:\n") || asm.contains("\nbar:\r\n"), "expects non-main function label 'bar:'");
    assert!(asm.contains(".rodata"), "expects rodata for string literals");
    assert!(asm.contains("syscall"), "expects write syscall usage for println");
}
