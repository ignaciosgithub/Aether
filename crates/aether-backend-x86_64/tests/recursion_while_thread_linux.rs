use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

fn module_rec_while_thread() -> Module {
    let fact = Item::Function(Function {
        name: "fact".into(),
        params: vec![Param { name: "n".into(), ty: Type::I64 }],
        ret: Type::I64,
        body: vec![
            Stmt::Return(Expr::IfElse {
                cond: Box::new(Expr::BinOp(
                    Box::new(Expr::Var("n".into())),
                    BinOpKind::Le,
                    Box::new(Expr::Lit(Value::Int(1))),
                )),
                then_expr: Box::new(Expr::Lit(Value::Int(1))),
                else_expr: Box::new(Expr::BinOp(
                    Box::new(Expr::Var("n".into())),
                    BinOpKind::Mul,
                    Box::new(Expr::Call("fact".into(), vec![
                        Expr::BinOp(
                            Box::new(Expr::Var("n".into())),
                            BinOpKind::Sub,
                            Box::new(Expr::Lit(Value::Int(1))),
                        ),
                    ])),
                )),
            }),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let worker = Item::Function(Function {
        name: "worker".into(),
        params: vec![Param { name: "n".into(), ty: Type::I64 }],
        ret: Type::I32,
        body: vec![
            Stmt::While {
                cond: Expr::BinOp(
                    Box::new(Expr::Lit(Value::Int(0))),
                    BinOpKind::Lt,
                    Box::new(Expr::Var("n".into())),
                ),
                body: vec![
                    Stmt::Expr(Expr::Call("fact".into(), vec![Expr::Var("n".into())])),
                    Stmt::Println("tick".into()),
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
                init: Expr::Call("spawn".into(), vec![Expr::Lit(Value::String("worker".into())), Expr::Lit(Value::Int(3))]),
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

    Module { items: vec![fact, worker, main_fn] }
}

#[test]
fn linux_threaded_recursion_while_codegen() {
    let m = module_rec_while_thread();
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("_start:"), "missing linux entry");
    assert!(asm.contains("mov $56, %rax") || asm.contains("movq $56, %rax"), "clone syscall missing");
    assert!(asm.contains("TSTACK0"), "thread stack missing");
    assert!(asm.contains("worker:\n") || asm.contains("\nworker:\r\n") || asm.contains("\nworker:\n"), "worker label missing");
    assert!(asm.contains("call fact"), "recursive call to fact missing");
    let has_worker_while = asm.contains("LWH_HEAD_worker_0:") || asm.contains(".LWH_HEAD_worker_0:");
    let has_main_while = asm.contains("LWH_HEAD_main_0:") || asm.contains(".LWH_HEAD_main_0:");
    assert!(has_worker_while || has_main_while, "while-head label for worker/main missing");
}
