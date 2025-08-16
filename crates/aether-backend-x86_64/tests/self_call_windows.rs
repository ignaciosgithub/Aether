use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_emits_self_call_inside_non_main() {
    let foo = Item::Function(Function {
        name: "foo".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Expr(Expr::Call("foo".to_string(), vec![])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: false,
        is_threaded: false,
    });

    let main_fn = Item::Function(Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Expr(Expr::Call("foo".to_string(), vec![])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![foo, main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    let foo_idx = asm.find("\nfoo:\n").expect("foo label");
    let call_foo_idx = asm[foo_idx..].find("call foo").map(|i| foo_idx + i).expect("self call in foo");
    assert!(asm.contains("push rbp") && asm.contains("mov rbp, rsp"), "prologue present");
    assert!(asm.contains("pop rbp") || asm.contains("\n        leave\n"), "epilogue present");
    let around = &asm[call_foo_idx.saturating_sub(100)..call_foo_idx + 100];
    assert!(around.contains("sub rsp, 32") && around.contains("add rsp, 32"), "shadow space around call");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn windows_emits_self_call_inside_non_main_skipped() {
    assert!(true);
}
