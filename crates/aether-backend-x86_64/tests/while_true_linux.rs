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

    // Accept both the legacy (.LWH_*) and general emitter (.LG_WH_*) schemes.
    let (head, end): (&str, String) = if asm.contains(".LWH_HEAD_main_0") {
        (".LWH_HEAD_main_0", ".LWH_END_main_0".to_string())
    } else {
        let head_idx = asm.find(".LG_WH_main_").expect("loop head label missing");
        let head = &asm[head_idx..head_idx + asm[head_idx..].find([':', '\n']).unwrap()];
        let end_idx = asm.find(".LG_WE_main_").expect("loop end label missing");
        let end = asm[end_idx..end_idx + asm[end_idx..].find([':', '\n']).unwrap()].to_string();
        (head, end)
    };
    // Between the loop head and the body's first write there must be no
    // unconditional jump to END: `while (true)` always enters the body.
    let head_idx = asm.find(&format!("{}:", head)).unwrap();
    let body_idx = head_idx + asm[head_idx..].find("syscall").unwrap_or(asm.len() - head_idx);
    let head_to_body = &asm[head_idx..body_idx];
    assert!(!head_to_body.contains(&format!("jmp {}\n", end)), "should not unconditionally jump to END at loop head for true");
    assert!(asm.contains("syscall") || asm.contains("write"), "expected a write/syscall in loop body");
    assert!(asm.contains(&format!("jmp {}", head)), "expected backedge to loop head");
}
