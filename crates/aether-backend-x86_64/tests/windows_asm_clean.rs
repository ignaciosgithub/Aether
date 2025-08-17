use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen as Codegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_no_orphan_ret_and_minimal_section_toggles() {
    let fact = Item::Function(Function {
        name: "fact".into(),
        params: vec![aether_frontend::ast::Param { name: "n".into(), ty: Type::I64 }],
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

    let forbidden = "\n        .text\n        ret\n";
    assert!(
        !asm.contains(forbidden),
        "unexpected orphan ret after .text section switch"
    );

    let mut seen = std::collections::HashSet::new();
    for line in asm.lines() {
        if let Some(s) = line.strip_suffix("_thunk:") {
            assert!(seen.insert(s.to_string()), "duplicate thunk label for {}", s);
        }
    }
}

#[cfg(not(target_os = "windows"))]
#[test]
fn skip_non_windows() { assert!(true); }
