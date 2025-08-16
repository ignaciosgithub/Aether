use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_nonmain_while_codegen() {
    let loop_fn = Item::Function(Function {
        name: "loop_once".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::While {
                cond: Expr::BinOp(
                    Box::new(Expr::Lit(Value::Int(1))),
                    aether_frontend::ast::BinOpKind::Lt,
                    Box::new(Expr::Lit(Value::Int(2))),
                ),
                body: vec![
                    Stmt::Println("in loop".into()),
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
            Stmt::Return(Expr::Call("loop_once".into(), vec![])),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![loop_fn, main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nloop_once:\n"), "function label must exist");
    let has_loop_labels = asm.contains("LWH_HEAD_") && asm.contains("LWH_END_");
    assert!(has_loop_labels, "expected loop head/end labels");
    let has_cmp_jump = asm.contains("cmp rax, 0") && (asm.contains(" je LWH_END_") || asm.contains("\nje ") || asm.contains("\njz "));
    assert!(has_cmp_jump, "expected cmp rax, 0 and conditional jump to loop end");
    assert!(asm.contains("jmp LWH_HEAD_"), "expected back-edge jump to loop head");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn windows_nonmain_while_codegen_skipped() { assert!(true); }
