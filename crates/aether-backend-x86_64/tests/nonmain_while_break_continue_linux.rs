use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn nonmain_while_break_continue_linux() {
    let loop_fn = Item::Function(Function {
        name: "loop_fn".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let { name: "i".into(), ty: Type::I64, init: Expr::Lit(Value::Int(0)) },
            Stmt::While {
                cond: Expr::BinOp(Box::new(Expr::Var("i".into())), BinOpKind::Lt, Box::new(Expr::Lit(Value::Int(3)))),
                body: vec![
                    Stmt::PrintExpr(Expr::Var("i".into())),
                    Stmt::While {
                        cond: Expr::Lit(Value::Int(1)),
                        body: vec![
                            Stmt::Continue,
                        ],
                    },
                    Stmt::Break,
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

    let m = Module { items: vec![loop_fn, main] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nloop_fn:\n") || asm.contains("\nloop_fn:\r\n"), "expects non-main function label 'loop_fn:'");
    assert!(asm.contains("LWH_HEAD_") || asm.contains(".LWH_HEAD_"), "expects while head label");
    assert!(asm.contains("LWH_END_") || asm.contains(".LWH_END_"), "expects while end label");
    assert!(asm.contains("syscall"), "expects integer print path to use syscall");
}
