use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn nonmain_list_i32_and_string_linux() {
    let foo = Item::Function(Function {
        name: "foo".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let {
                name: "xs".into(),
                ty: Type::List(Box::new(Type::I32)),
                init: Expr::ArrayLit(vec![Expr::Lit(Value::Int(1)), Expr::Lit(Value::Int(2)), Expr::Lit(Value::Int(3))]),
            },
            Stmt::Let {
                name: "ss".into(),
                ty: Type::List(Box::new(Type::String)),
                init: Expr::ArrayLit(vec![Expr::Lit(Value::String("a".into())), Expr::Lit(Value::String("bc".into()))]),
            },
            Stmt::PrintExpr(Expr::Call("len".into(), vec![Expr::Var("xs".into())])),
            Stmt::PrintExpr(Expr::Index(Box::new(Expr::Var("xs".into())), Box::new(Expr::Lit(Value::Int(1))))),
            Stmt::PrintExpr(Expr::Index(Box::new(Expr::Var("ss".into())), Box::new(Expr::Lit(Value::Int(1))))),
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
    let m = Module { items: vec![foo, main] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nfoo:\n") || asm.contains("\nfoo:\r\n"), "expects non-main function label 'foo:'");
    assert!(asm.contains(".rodata"), "expects rodata present for list literals");
    assert!(asm.contains("syscall"), "expects write syscall usage for prints");
}
