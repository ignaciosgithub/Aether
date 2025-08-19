use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn linux_codegen_emits_gt_in_while() {
    let f = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::While {
                cond: Expr::BinOp(Box::new(Expr::Lit(Value::Int(3))), BinOpKind::Gt, Box::new(Expr::Lit(Value::Int(1)))),
                body: vec![
                    Stmt::Break,
                ],
            },
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![f] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("jle .LWH_END_main_") || asm.contains("jle\t"),
            "expected greater-than condition to use jle to leave loop when false");
}

#[test]
fn linux_codegen_emits_ge_in_while() {
    let f = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::While {
                cond: Expr::BinOp(Box::new(Expr::Lit(Value::Int(2))), BinOpKind::Ge, Box::new(Expr::Lit(Value::Int(0)))),
                body: vec![
                    Stmt::Break,
                ],
            },
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![f] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("jl .LWH_END_main_") || asm.contains("jl\t") || asm.contains("jl .LWH_END_"),
            "expected greater-equal condition to use jl to leave loop when false");
}
