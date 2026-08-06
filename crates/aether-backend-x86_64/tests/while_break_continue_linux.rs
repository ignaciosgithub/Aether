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
    // Works for both the legacy (.LWH_HEAD_/.LWH_END_) and general emitter
    // (.LG_WH_/.LG_WE_) label schemes: collect head/end labels in emission
    // order (outer first) and check break/continue jump targets.
    let heads: Vec<String> = asm
        .lines()
        .map(str::trim)
        .filter(|l| (l.starts_with(".LWH_HEAD_worker_") || l.starts_with(".LG_WH_worker_")) && l.ends_with(':'))
        .map(|l| l.trim_end_matches(':').to_string())
        .collect();
    let ends: Vec<String> = asm
        .lines()
        .map(str::trim)
        .filter(|l| (l.starts_with(".LWH_END_worker_") || l.starts_with(".LG_WE_worker_")) && l.ends_with(':'))
        .map(|l| l.trim_end_matches(':').to_string())
        .collect();
    assert_eq!(heads.len(), 2, "missing while head labels: {:?}", heads);
    assert_eq!(ends.len(), 2, "missing while end labels: {:?}", ends);
    let (outer_head, inner_head) = (&heads[0], &heads[1]);
    // Ends appear in emission order: inner end first, then outer end.
    let outer_end = ends.iter().max_by_key(|e| asm.find(format!("{}:", e).as_str())).unwrap();
    assert_ne!(outer_head, inner_head, "while heads must be unique");
    assert!(asm.contains(&format!("jmp {}", inner_head)), "continue should jump to inner head");
    assert!(asm.contains(&format!("jmp {}", outer_end)), "break should jump to outer end");
}
