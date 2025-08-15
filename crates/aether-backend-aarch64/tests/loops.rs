use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;
use aether_backend_aarch64::AArch64Codegen;

#[test]
fn aarch64_while_emits_labels_and_branches() {
    let func = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::While {
                cond: Expr::BinOp(
                    Box::new(Expr::Lit(Value::Int(1))),
                    BinOpKind::Lt,
                    Box::new(Expr::Lit(Value::Int(2))),
                ),
                body: vec![
                    Stmt::Println("L".to_string()),
                    Stmt::Break,
                ],
            },
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    let module = Module { items: vec![Item::Function(func)] };
    let mut cg = AArch64Codegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains(".LWH_HEAD_main_0:"));
    assert!(asm.contains(".LWH_END_main_0:"));
    assert!(asm.contains("cmp x10, x11"));
    assert!(asm.contains("b.ge .LWH_END_main_0") || asm.contains("b.lt .LWH_HEAD_main_0"));
    assert!(asm.contains("b .LWH_HEAD_main_0"));
}
