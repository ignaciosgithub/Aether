use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value, Param};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_runtime_ifelse_branches_in_text_and_prints() {
    let demo = Item::Function(Function {
        name: "demo".to_string(),
        params: vec![Param { name: "a".to_string(), ty: Type::I32 }, Param { name: "b".to_string(), ty: Type::I32 }],
        ret: Type::F64, // dummy float ret to exercise LC0 in data too
        body: vec![
            Stmt::PrintExpr(Expr::IfElse {
                cond: Box::new(Expr::BinOp(
                    Box::new(Expr::Var("a".to_string())),
                    aether_frontend::ast::BinOpKind::Lt,
                    Box::new(Expr::Var("b".to_string()))
                )),
                then_expr: Box::new(Expr::Lit(Value::String("LT".to_string()))),
                else_expr: Box::new(Expr::Lit(Value::String("GE".to_string()))),
            }),
            Stmt::Return(Expr::Lit(Value::Float(0.0))),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let main_fn = Item::Function(Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let {
                name: "r".to_string(),
                ty: Type::F64,
                init: Expr::Call("demo".to_string(), vec![Expr::Lit(Value::Int(2)), Expr::Lit(Value::Int(3))]),
            },
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![demo, main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("LIF_THEN_demo_") && asm.contains("LIF_ELSE_demo_") && asm.contains("LIF_JOIN_demo_"), "if/else labels must exist");
    assert!(asm.contains("\n        .data\nLSNL:\n") && asm.contains("\n        .text\n"), "LSNL under .data, back to .text");
    assert!(asm.contains("call WriteFile"), "should print via WriteFile");
    assert!(asm.contains("mov rcx, r12"), "WriteFile rcx=r12 handle");
    assert!(asm.contains("sub rsp, 40"), "shadow space before WriteFile");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn ifelse_windows_skipped_on_non_windows() {
    assert!(true);
}
