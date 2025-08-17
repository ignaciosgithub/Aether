use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value, Param};
use aether_backend_x86_64::X86_64LinuxCodegen as Codegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_recursive_print_initializes_stdout_handle() {
    let factp = Item::Function(Function {
        name: "factp".into(),
        params: vec![Param { name: "n".into(), ty: Type::I64 }],
        ret: Type::I64,
        body: vec![
            Stmt::Println("step".into()),
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
                    Box::new(Expr::Call("factp".into(), vec![
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
            Stmt::Expr(Expr::Call("factp".into(), vec![Expr::Lit(Value::Int(3))])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![factp, main_fn] };
    let mut cg = Codegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    let has_getstd = asm.contains("factp:\n")
        && (asm.contains("mov ecx, -11\n        call GetStdHandle") ||
            asm.contains("mov ecx, -11\n        call GetStdHandle\n"));
    assert!(has_getstd, "expected GetStdHandle initialization inside factp before printing");

    let uses_rbx = asm.contains("mov rcx, rbx\n        call WriteFile");
    assert!(uses_rbx, "WriteFile should use rcx=rbx in factp");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn skip_non_windows() { assert!(true); }
