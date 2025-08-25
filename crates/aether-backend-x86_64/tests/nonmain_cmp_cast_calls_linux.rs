use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn nonmain_cmp_cast_calls_recursion_linux() {
    let recur = Item::Function(Function {
        name: "recur".into(),
        params: vec![Param { name: "n".into(), ty: Type::I64 }],
        ret: Type::I64,
        body: vec![
            Stmt::Return(Expr::IfElse {
                cond: Box::new(Expr::BinOp(Box::new(Expr::Var("n".into())), BinOpKind::Le, Box::new(Expr::Lit(Value::Int(1))))),
                then_expr: Box::new(Expr::Var("n".into())),
                else_expr: Box::new(Expr::BinOp(
                    Box::new(Expr::Var("n".into())),
                    BinOpKind::Add,
                    Box::new(Expr::Call("recur".into(), vec![
                        Expr::BinOp(Box::new(Expr::Var("n".into())), BinOpKind::Sub, Box::new(Expr::Lit(Value::Int(1))))
                    ]))
                )),
            }),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let math_fn = Item::Function(Function {
        name: "math_fn".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let { name: "x".into(), ty: Type::I64, init: Expr::Lit(Value::Int(5)) },
            Stmt::Let { name: "y".into(), ty: Type::I64, init: Expr::Lit(Value::Int(7)) },
            Stmt::While {
                cond: Expr::BinOp(Box::new(Expr::Var("x".into())), BinOpKind::Lt, Box::new(Expr::Var("y".into()))),
                body: vec![
                    Stmt::PrintExpr(Expr::IfElse {
                        cond: Box::new(Expr::BinOp(Box::new(Expr::Var("y".into())), BinOpKind::Gt, Box::new(Expr::Var("x".into())))),
                        then_expr: Box::new(Expr::Lit(Value::String("GT".into()))),
                        else_expr: Box::new(Expr::Lit(Value::String("LE".into()))),
                    }),
                    Stmt::Assign { target: Expr::Var("x".into()), value: Expr::BinOp(Box::new(Expr::Var("x".into())), BinOpKind::Add, Box::new(Expr::Lit(Value::Int(1)))) },
                    Stmt::Continue,
                ],
            },
            Stmt::PrintExpr(Expr::Cast(Box::new(Expr::Var("x".into())), Type::F64)),
            Stmt::PrintExpr(Expr::Call("recur".into(), vec![Expr::Lit(Value::Int(4))])),
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
    let m = Module { items: vec![recur, math_fn, main] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nmath_fn:\n") || asm.contains("\nmath_fn:\r\n"), "expects non-main function label 'math_fn:'");
    assert!(asm.contains("LWH_HEAD_") || asm.contains(".LWH_HEAD_"), "expects while head label");
    assert!(asm.contains("LWH_END_") || asm.contains(".LWH_END_"), "expects while end label");
    assert!(asm.contains("syscall"), "expects write syscall usage for prints");
}
