use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};

use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[test]
fn linux_while_true_enters_body() {
    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::While {
                cond: Expr::Lit(Value::Bool(true)),
                body: vec![
                    Stmt::Println("x".into()),
                    Stmt::Break,
                ],
            },
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![main_fn] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains(".LWH_HEAD_main_0") && asm.contains(".LWH_END_main_0"), "loop labels missing");
    let head_idx = asm.find(".LWH_HEAD_main_0").unwrap();
    let end_idx = asm.find(".LWH_END_main_0").unwrap();
    let head_to_end = &asm[head_idx..end_idx];
    assert!(!head_to_end.contains("jmp .LWH_END_main_0\n"), "should not unconditionally jump to END at loop head for true");
    assert!(asm.contains("syscall") || asm.contains("write"), "expected a write/syscall in loop body");
    assert!(asm.contains("jmp .LWH_HEAD_main_0"), "expected backedge to loop head");
}
