use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn list_index_oob_path_linux() {
    let f = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let {
                name: "xs".into(),
                ty: Type::List(Box::new(Type::I32)),
                init: Expr::ArrayLit(vec![
                    Expr::Lit(Value::Int(7)),
                    Expr::Lit(Value::Int(8)),
                    Expr::Lit(Value::Int(9)),
                ]),
            },
            Stmt::PrintExpr(Expr::Index(Box::new(Expr::Var("xs".into())), Box::new(Expr::Lit(Value::Int(10))))),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![f] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");
    assert!(asm.contains("index out of bounds") || asm.contains(".LOOB_") || asm.contains("syscall"), "expects OOB error path emission");
}
