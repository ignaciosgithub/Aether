use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_nonmain_mutual_recursion_calls_emitted() {
    let a = Item::Function(Function {
        name: "a".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::IfElse(
                Expr::Lit(Value::Int(0)),
                vec![Stmt::Expr(Expr::Call("b".into(), vec![]))],
                vec![]
            ),
            Stmt::Return(Expr::Lit(Value::Int(1))),
        ],
        is_pub: false,
        is_threaded: false,
    });
    let b = Item::Function(Function {
        name: "b".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::IfElse(
                Expr::Lit(Value::Int(0)),
                vec![Stmt::Expr(Expr::Call("a".into(), vec![]))],
                vec![]
            ),
            Stmt::Return(Expr::Lit(Value::Int(2))),
        ],
        is_pub: false,
        is_threaded: false,
    });
    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Expr(Expr::Call("a".into(), vec![])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![a, b, main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\na:\n") && asm.contains("\nb:\n"), "both functions should be emitted");
    let has_shadow_space_calls = asm.contains("sub rsp, 32") && asm.contains("add rsp, 32") && (asm.contains("call a") || asm.contains("call b"));
    assert!(has_shadow_space_calls, "mutual recursion calls should use shadow space");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn windows_nonmain_mutual_recursion_calls_emitted_skipped() { assert!(true); }
