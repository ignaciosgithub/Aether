use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen as Codegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_break_continue_in_nested_while_targets_nearest_labels() {
    let worker = Item::Function(Function {
        name: "worker".into(),
        params: vec![Param { name: "n".into(), ty: Type::I64 }],
        ret: Type::I32,
        body: vec![
            Stmt::While {
                cond: Expr::BinOp(Box::new(Expr::Lit(Value::Int(0))), BinOpKind::Lt, Box::new(Expr::Var("n".into()))),
                body: vec![
                    Stmt::While {
                        cond: Expr::Lit(Value::Int(1)),
                        body: vec![
                            Stmt::Continue,
                        ],
                    },
                    Stmt::Break,
                ],
            },
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![worker] };
    let mut cg = Codegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");
    assert!(asm.contains("LWH_HEAD_worker_0:"), "missing outer head");
    assert!(asm.contains("LWH_END_worker_0:"), "missing outer end");
    assert!(asm.contains("LWH_HEAD_worker_1:"), "missing inner head");
    assert!(asm.contains("LWH_END_worker_1:"), "missing inner end");
    assert!(asm.contains("jmp LWH_HEAD_worker_1"), "continue should jump to inner head");
    assert!(asm.contains("jmp LWH_END_worker_0"), "break should jump to outer end");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn skip_on_non_windows() { assert!(true); }
