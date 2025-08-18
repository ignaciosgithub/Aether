use aether_frontend::ast::*;
use aether_backend_x86_64::*;
use aether_codegen::CodeGenerator;

#[test]
fn windows_writefile_preserves_rcx_via_r11() {
    let rec_fn = Item::Function(Function {
        name: "recur".into(),
        params: vec![("n".into(), Type::I32)],
        ret: Type::I32,
        body: vec![
            Stmt::IfElse {
                cond: Expr::BinOp(
                    Box::new(Expr::Var("n".into())),
                    BinOpKind::Le,
                    Box::new(Expr::Lit(Value::Int(0))),
                ),
                then_body: vec![
                    Stmt::Println("x".into()),
                    Stmt::Return(Expr::Lit(Value::Int(0))),
                ],
                else_body: vec![],
            },
            Stmt::Println("y".into()),
            Stmt::Expr(Expr::Call("recur".into(), vec![
                Expr::BinOp(
                    Box::new(Expr::Var("n".into())),
                    BinOpKind::Sub,
                    Box::new(Expr::Lit(Value::Int(1))),
                )
            ])),
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
            Stmt::Println("start".into()),
            Stmt::Expr(Expr::Call("recur".into(), vec![Expr::Lit(Value::Int(2))])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let module = Module { items: vec![rec_fn, main_fn] };

    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).unwrap();

    let has_save = asm.contains("mov r11, rcx");
    let has_restore = asm.contains("mov rcx, r11");
    assert!(has_save && has_restore, "Expected rcx save/restore around WriteFile via r11");

    assert!(asm.contains("sub rsp, 40") && asm.contains("call WriteFile") && asm.contains("add rsp, 40"),
        "Expected 32-byte shadow space around WriteFile");
}
