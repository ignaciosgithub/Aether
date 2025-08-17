use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[test]
fn windows_while_with_early_return_codegen() {
    let loop_fn = Item::Function(Function {
        name: "loop_ret".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::While {
                cond: Expr::BinOp(
                    Box::new(Expr::Lit(Value::Int(1))),
                    aether_frontend::ast::BinOpKind::Lt,
                    Box::new(Expr::Lit(Value::Int(3))),
                ),
                body: vec![
                    Stmt::Println("x".into()),
                    Stmt::Return(Expr::Lit(Value::Int(1))),
                ],
            },
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: false,
        is_threaded: false,
    });

    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Return(Expr::Call("loop_ret".into(), vec![])),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![loop_fn, main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nloop_ret:\n"));
    let head_pos = asm.find("\nLWH_HEAD_loop_ret_0:\n").expect("loop head label");
    let end_pos = asm.find("\nLWH_END_loop_ret_0:\n").expect("loop end label");
    assert!(head_pos < end_pos);

    let body_region = &asm[head_pos..end_pos];
    let has_ret_inside = body_region.contains("\n        ret");
    assert!(
        !has_ret_inside,
        "no direct ret inside loop body; returns should jump to epilogue"
    );
    assert!(body_region.contains("cmp rax, 0\n        je LWH_END_"), "expected je to loop end in head");
    assert!(body_region.contains("jmp LWH_HEAD_"), "expected back-edge jmp to loop head");

    assert!(asm.contains("LRET_loop_ret:"), "expected function epilogue label for non-main");
    assert!(asm.contains("LRET_main:"), "expected epilogue label for main");
}
