use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn nonmain_worker_like_fn_with_while_and_print_linux() {
    let worker_body = Item::Function(Function {
        name: "worker_body".into(),
        params: vec![Param { name: "n".into(), ty: Type::I64 }],
        ret: Type::I32,
        body: vec![
            Stmt::While {
                cond: Expr::BinOp(Box::new(Expr::Var("n".into())), BinOpKind::Gt, Box::new(Expr::Lit(Value::Int(0)))),
                body: vec![
                    Stmt::PrintExpr(Expr::Lit(Value::String("tick".into()))),
                    Stmt::Assign { target: Expr::Var("n".into()), value: Expr::BinOp(Box::new(Expr::Var("n".into())), BinOpKind::Sub, Box::new(Expr::Lit(Value::Int(1)))) },
                    Stmt::Continue,
                ],
            },
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

    let m = Module { items: vec![worker_body, main] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nworker_body:\n") || asm.contains("\nworker_body:\r\n"), "expects non-main function label 'worker_body:'");
    assert!(asm.contains(".rodata"), "expects rodata for \"tick\" string");
    assert!(asm.contains("LWH_HEAD_") || asm.contains(".LWH_HEAD_"), "expects while head label");
    assert!(asm.contains("syscall"), "expects write syscall usage for println inside worker-like body");
}
