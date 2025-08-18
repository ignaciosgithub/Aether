use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[test]
fn linux_nested_while_in_function_emits_unique_labels() {
    let worker = Item::Function(Function {
        name: "worker".into(),
        params: vec![Param { name: "n".into(), ty: Type::I64 }],
        ret: Type::I32,
        body: vec![
            Stmt::While {
                cond: Expr::BinOp(Box::new(Expr::Lit(Value::Int(0))), BinOpKind::Lt, Box::new(Expr::Var("n".into()))),
                body: vec![
                    Stmt::While {
                        cond: Expr::Lit(Value::Int(1)),
                        body: vec![
                            Stmt::Println("inner".into()),
                            Stmt::Break,
                        ],
                    },
                    Stmt::Assign {
                        target: Expr::Var("n".into()),
                        value: Expr::BinOp(Box::new(Expr::Var("n".into())), BinOpKind::Sub, Box::new(Expr::Lit(Value::Int(1)))),
                    },
                ],
            },
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let {
                name: "h".into(),
                ty: Type::I64,
                init: Expr::Call("spawn".into(), vec![Expr::Lit(Value::String("worker".into())), Expr::Lit(Value::Int(2))]),
            },
            Stmt::Let {
                name: "r".into(),
                ty: Type::I32,
                init: Expr::Call("join".into(), vec![Expr::Var("h".into())]),
            },
            Stmt::Let {
                name: "ok".into(),
                ty: Type::I32,
                init: Expr::Call("destroy".into(), vec![Expr::Var("h".into())]),
            },
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![worker, main_fn] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");
    assert!(asm.contains("LWH_HEAD_worker_0:") || asm.contains(".LWH_HEAD_worker_0:"), "missing outer while head");
    assert!(asm.contains("LWH_END_worker_0:") || asm.contains(".LWH_END_worker_0:"), "missing outer while end");
    assert!(asm.contains("LWH_HEAD_worker_1:") || asm.contains(".LWH_HEAD_worker_1:"), "missing inner while head");
    assert!(asm.contains("LWH_END_worker_1:") || asm.contains(".LWH_END_worker_1:"), "missing inner while end");
}
