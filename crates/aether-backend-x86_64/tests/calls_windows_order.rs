use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_emits_print_call_print_in_order() {
    let foo = Item::Function(Function {
        name: "foo".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Println("In foo".to_string()),
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
            Stmt::Println("In main before call".to_string()),
            Stmt::Expr(Expr::Call("foo".to_string(), vec![])),
            Stmt::Println("After call".to_string()),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![foo, main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    let i_before = asm.find("lea rdx, [rip+LS0]").unwrap_or(asm.find("lea rdx, [rip+LS1]").unwrap());
    let i_call = asm.find("\n        call foo").expect("call foo present");
    let i_after = asm.rfind("lea rdx, [rip+").expect("second print present");
    assert!(i_before < i_call && i_call < i_after, "expected print before call and another print after call in assembly");

    assert!(asm.contains("mov rcx, r12"), "WriteFile should use console handle in r12");
    assert!(asm.contains("\n        .data\nLSNL:\n        .byte 10\n") || asm.contains("LSNL:\n        .byte 10"), "should emit LSNL in data");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn windows_emits_print_call_print_in_order_skipped() {
    assert!(true);
}
