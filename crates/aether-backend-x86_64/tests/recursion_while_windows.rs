#[cfg(target_os = "windows")]
use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen as Codegen;
use aether_codegen::CodeGenerator;

#[test]
fn windows_recursive_function_has_unified_epilogue_and_loop() {
    let fact = Item::Function(Function {
        name: "fact".into(),
        params: vec![
            aether_frontend::ast::Param { name: "n".into(), ty: Type::I64 },
        ],
        ret: Type::I64,
        body: vec![
            Stmt::While {
                cond: Expr::BinOp(Box::new(Expr::Var("n".into())), aether_frontend::ast::BinOpKind::Gt, Box::new(Expr::Lit(Value::Int(1)))),
                body: vec![
                    Stmt::Break,
                ],
            },
            Stmt::Return(Expr::IfElse {
                cond: Box::new(Expr::BinOp(Box::new(Expr::Var("n".into())), aether_frontend::ast::BinOpKind::Le, Box::new(Expr::Lit(Value::Int(1))))),
                then_expr: Box::new(Expr::Lit(Value::Int(1))),
                else_expr: Box::new(Expr::BinOp(
                    Box::new(Expr::Var("n".into())),
                    aether_frontend::ast::BinOpKind::Mul,
                    Box::new(Expr::Call("fact".into(), vec![
                        Expr::BinOp(Box::new(Expr::Var("n".into())), aether_frontend::ast::BinOpKind::Sub, Box::new(Expr::Lit(Value::Int(1)))),
                    ]))
                )),
            }),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Expr(Expr::Call("fact".into(), vec![Expr::Lit(Value::Int(5))])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![fact, main_fn] };
    let mut cg = Codegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("fact:\n"), "fact label missing");
    assert!(asm.contains("LRET_fact:"), "unified epilogue label missing");
    let has_shadow_call = asm.contains("sub rsp, 40\n        call fact\n        add rsp, 40") || asm.contains("sub rsp, 32\n        call fact\n        add rsp, 32");
    assert!(has_shadow_call, "shadow space not used around recursive call");
    assert!(asm.contains("LWH_HEAD_fact_0:"), "loop head missing");
    assert!(asm.contains("je LWH_END_fact_0"), "je to end missing");
    assert!(asm.contains("jmp LWH_HEAD_fact_0"), "backedge to head missing");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn skip_on_non_windows() {
    assert!(true);
}
