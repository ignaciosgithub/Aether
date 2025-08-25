use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value, Param};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_supports_recursive_call_codegen() {
    let fact = Item::Function(Function {
        name: "fact".to_string(),
        params: vec![Param { name: "n".into(), ty: Type::I32 }],
        ret: Type::I32,
        body: vec![
            Stmt::IfElse(
                Expr::Le(Box::new(Expr::Var("n".into())), Box::new(Expr::Lit(Value::Int(1)))),
                vec![Stmt::Return(Expr::Lit(Value::Int(1)))],
                vec![]
            ),
            Stmt::Return(
                Expr::Mul(
                    Box::new(Expr::Var("n".into())),
                    Box::new(Expr::Call("fact".into(), vec![
                        Expr::BinOp(Box::new(Expr::Var("n".into())), BinOpKind::Sub, Box::new(Expr::Lit(Value::Int(1))))
                    ]))
                )
            ),
        ],
        is_pub: false,
        is_threaded: false,
    });

    let main_fn = Item::Function(Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Return(Expr::Call("fact".into(), vec![Expr::Lit(Value::Int(5))])),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![fact, main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nfact:\n"), "should define fact function label");
    assert!(asm.contains("\n        call fact"), "should emit recursive call to fact");

    assert!(asm.contains("push rbp") && asm.contains("mov rbp, rsp"), "should set up rbp frame");
    assert!(asm.contains("pop rbp") || asm.contains("\n        leave\n"), "should tear down frame");

    let has_shadow = asm.contains("sub rsp, 40") || asm.contains("sub rsp, 32");
    assert!(has_shadow, "should reserve shadow space before calls on Win64");

}

#[cfg(not(target_os = "windows"))]
#[test]
fn windows_supports_recursive_call_codegen_skipped() {
    assert!(true);
}
