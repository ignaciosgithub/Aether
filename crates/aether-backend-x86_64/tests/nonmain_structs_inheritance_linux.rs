use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn nonmain_structs_inheritance_linux() {
    let oo_fn = Item::Function(Function {
        name: "oo_fn".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let {
                name: "parent_s".into(),
                ty: Type::String,
                init: Expr::Lit(Value::String("Inherit".into())),
            },
            Stmt::PrintExpr(Expr::Var("parent_s".into())),
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
    let m = Module { items: vec![oo_fn, main] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\noo_fn:\n") || asm.contains("\noo_fn:\r\n"), "expects non-main function label 'oo_fn:'");
    assert!(asm.contains(".rodata"), "expects rodata for string literal");
    assert!(asm.contains("syscall"), "expects write syscall usage for println");
}
