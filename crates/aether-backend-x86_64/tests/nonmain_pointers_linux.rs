use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn nonmain_addr_of_and_deref_linux() {
    let ptr_fn = Item::Function(Function {
        name: "ptr_fn".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let { name: "x".into(), ty: Type::I64, init: Expr::Lit(Value::Int(42)) },
            Stmt::PrintExpr(Expr::Deref(Box::new(Expr::AddrOf(Box::new(Expr::Var("x".into())))))),
            Stmt::Let {
                name: "arr".into(),
                ty: Type::Array(Box::new(Type::I32), 3),
                init: Expr::ArrayLit(vec![Expr::Lit(Value::Int(1)), Expr::Lit(Value::Int(2)), Expr::Lit(Value::Int(3))]),
            },
            Stmt::PrintExpr(Expr::Deref(Box::new(Expr::AddrOf(Box::new(
                Expr::Index(Box::new(Expr::Var("arr".into())), Box::new(Expr::Lit(Value::Int(1))))
            ))))),
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

    let m = Module { items: vec![ptr_fn, main] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nptr_fn:\n") || asm.contains("\nptr_fn:\r\n"), "expects non-main function label 'ptr_fn:'");
    assert!(asm.contains("leaq "), "expects addr-of to produce leaq");
    assert!(asm.contains("(%r") || asm.contains("(%rax)"), "expects deref via memory load");
    assert!(asm.contains("syscall"), "expects write syscall for prints");
}
