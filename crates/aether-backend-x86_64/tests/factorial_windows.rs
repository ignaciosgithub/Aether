use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value, Param};
use aether_backend_x86_64::X86_64LinuxCodegen as Codegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_factorial_uses_param_and_recurses() {
    let fact = Item::Function(Function {
        name: "fact".into(),
        params: vec![Param { name: "n".into(), ty: Type::I64 }],
        ret: Type::I64,
        body: vec![
            Stmt::Return(Expr::IfElse {
                cond: Box::new(Expr::BinOp(
                    Box::new(Expr::Var("n".into())),
                    aether_frontend::ast::BinOpKind::Le,
                    Box::new(Expr::Lit(Value::Int(1))),
                )),
                then_expr: Box::new(Expr::Lit(Value::Int(1))),
                else_expr: Box::new(Expr::BinOp(
                    Box::new(Expr::Var("n".into())),
                    aether_frontend::ast::BinOpKind::Mul,
                    Box::new(Expr::Call("fact".into(), vec![
                        Expr::BinOp(
                            Box::new(Expr::Var("n".into())),
                            aether_frontend::ast::BinOpKind::Sub,
                            Box::new(Expr::Lit(Value::Int(1))),
                        ),
                    ])),
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
    assert!(asm.contains("cmp rcx, 1") || asm.contains("cmp rcx, 0x1"), "expected cmp rcx, 1 in base case");
    let has_shadow = asm.contains("sub rsp, 40\n        call fact\n        add rsp, 40") ||
                     asm.contains("sub rsp, 32\n        call fact\n        add rsp, 32");
    assert!(has_shadow, "missing shadow space around recursive call");
    assert!(asm.contains("mov rbx, rcx"), "expected save param n in rbx");
    assert!(asm.contains("imul rax, rbx"), "expected multiply by saved n");
    assert!(asm.contains("LRET_fact:"), "missing unified epilogue label");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn skip_non_windows() { assert!(true); }
