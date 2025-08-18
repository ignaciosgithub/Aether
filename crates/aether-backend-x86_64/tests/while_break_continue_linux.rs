use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[test]
fn linux_break_continue_in_nested_while_targets_nearest_labels() {
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
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");
    let h0 = asm.contains(".LWH_HEAD_worker_0:") || asm.contains("LWH_HEAD_worker_0:");
    let e0 = asm.contains(".LWH_END_worker_0:") || asm.contains("LWH_END_worker_0:");
    let h1 = asm.contains(".LWH_HEAD_worker_1:") || asm.contains("LWH_HEAD_worker_1:");
    let e1 = asm.contains(".LWH_END_worker_1:") || asm.contains("LWH_END_worker_1:");
    assert!(h0 && e0 && h1 && e1, "missing while labels");
    assert!(asm.contains(&format!("jmp {}", if asm.contains(".LWH_HEAD_worker_1:") { ".LWH_HEAD_worker_1" } else { "LWH_HEAD_worker_1" })), "continue should jump to inner head");
    assert!(asm.contains(&format!("jmp {}", if asm.contains(".LWH_END_worker_0:") { ".LWH_END_worker_0" } else { "LWH_END_worker_0" })), "break should jump to outer end");
}
